#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(lubridate)
  library(yaml)
})
source("R/utils_config.R")
source("R/utils_common.R")
source("R/io_fetch.R")
source("R/preprocess.R")
source("R/windows.R")
source("R/fpca_model.R")
source("R/metrics.R")
source("R/thresholds.R")
source("R/decide.R")

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(key, default=NULL) {
  if (key %in% args) {
    idx <- which(args == key)
    if (idx < length(args)) return(args[idx+1])
  }
  default
}

sensor_id <- get_arg("--sensor_id", "VG09")
date_str <- get_arg("--date", as.character(Sys.Date()-1))
set.seed(42)

cfg <- load_config("config.yaml")
target_date <- as.Date(date_str)

log_file <- file.path(cfg$runtime$log_dir, paste0("dryrun_", format(target_date, "%Y%m%d"), ".log"))
con <- file(log_file, open = "at")
sink(con, split = TRUE)
on.exit({sink(); close(con)}, add = TRUE)

message("start dry run for ", sensor_id, " on ", target_date)

raw_today <- fetch_day(sensor_id, target_date, cfg)
if (is.null(raw_today)) stop("no data for today")
prep_today <- preprocess_day(raw_today, cfg)
Y_today <- prep_today$vec

days_list <- build_reference_days(target_date, cfg)
ref_data <- list()
counts <- list()
for (nm in names(days_list)) {
  kept <- 0; dropped <- 0; vecs <- list()
  for (d in days_list[[nm]]) {
    raw <- fetch_day(sensor_id, d, cfg)
    if (is.null(raw)) next
    prep <- preprocess_day(raw, cfg)
    if (is.null(prep)) next
    if (prep$meta$missing_ratio > cfg$preprocess$max_missing_ratio) {
      dropped <- dropped + 1
      next
    }
    kept <- kept + 1
    vecs[[length(vecs)+1]] <- prep$vec
  }
  message(nm, ": kept=", kept, ", dropped=", dropped)
  counts[[nm]] <- list(kept=kept, dropped=dropped)
  if (kept > 0) ref_data[[nm]] <- do.call(rbind, vecs)
}

metrics_today_list <- list()
thresholds_list <- list()
decisions <- list()

if (cfg$model$per_window) {
  for (nm in names(days_list)) {
    Y_ref_mat <- ref_data[[nm]]
    if (!is.null(Y_ref_mat)) {
      fpca_fit_w <- fit_fpca(Y_ref_mat, prep_today$t_grid, cfg)
      if (!is.null(fpca_fit_w)) message(nm, ": 使用PC数 = ", fpca_fit_w$pcnum, " | 累積寄与率 = ", round(fpca_fit_w$cumvar_used*100,1), "%")
      scores_ref <- if (!is.null(fpca_fit_w)) project_fpca_scores(Y_ref_mat, fpca_fit_w) else matrix(nrow=nrow(Y_ref_mat), ncol=0)
      scores_today <- if (!is.null(fpca_fit_w)) project_fpca_scores(matrix(Y_today, nrow=1), fpca_fit_w) else matrix(nrow=1, ncol=0)
      recon_today <- if (!is.null(fpca_fit_w)) reconstruct_from_fpca(scores_today, fpca_fit_w)[1,] else rep(NA, length(Y_today))
      metrics <- compute_metrics(Y_ref_mat, Y_today, scores_ref, scores_today, recon_today, fpca_fit_w, cfg)
      metrics_today_list[[nm]] <- metrics$today
      thr <- fit_thresholds(metrics$ref, cfg)
      thr$fpca_scores_topk_z <- list(attention = cfg$decision$attention_sigma, alert = cfg$decision$alert_sigma)
      thresholds_list[[nm]] <- thr
      dec <- decide_today(metrics$today, thr, cfg, context_info=list(window=nm))
      decisions[[nm]] <- dec
      if (!is.null(fpca_fit_w)) saveRDS(fpca_fit_w, file.path(cfg$runtime$output_dir, paste0("fpca_model_", nm, ".rds")))
    }
  }
} else {
  Y_ref_all <- if (length(ref_data)>0) do.call(rbind, ref_data) else matrix(nrow=0, ncol=length(Y_today))
  fpca_fit <- fit_fpca(Y_ref_all, prep_today$t_grid, cfg)
  if (!is.null(fpca_fit)) message("global: 使用PC数 = ", fpca_fit$pcnum, " | 累積寄与率 = ", round(fpca_fit$cumvar_used*100,1), "%")
  scores_today <- if (!is.null(fpca_fit)) project_fpca_scores(matrix(Y_today, nrow=1), fpca_fit) else matrix(nrow=1, ncol=0)
  recon_today <- if (!is.null(fpca_fit)) reconstruct_from_fpca(scores_today, fpca_fit)[1,] else rep(NA, length(Y_today))
  for (nm in names(days_list)) {
    Y_ref_mat <- ref_data[[nm]]
    scores_ref <- if (!is.null(fpca_fit) && !is.null(Y_ref_mat)) project_fpca_scores(Y_ref_mat, fpca_fit) else matrix(nrow=ifelse(is.null(Y_ref_mat),0,nrow(Y_ref_mat)), ncol=0)
    metrics <- compute_metrics(if (is.null(Y_ref_mat)) matrix(nrow=0, ncol=length(Y_today)) else Y_ref_mat, Y_today, scores_ref, scores_today, recon_today, fpca_fit, cfg)
    metrics_today_list[[nm]] <- metrics$today
    thr <- fit_thresholds(metrics$ref, cfg)
    thr$fpca_scores_topk_z <- list(attention = cfg$decision$attention_sigma, alert = cfg$decision$alert_sigma)
    thresholds_list[[nm]] <- thr
    dec <- decide_today(metrics$today, thr, cfg, context_info=list(window=nm))
    decisions[[nm]] <- dec
  }
  if (!is.null(fpca_fit)) saveRDS(fpca_fit, file.path(cfg$runtime$output_dir, "fpca_model.rds"))
}

final_level <- "ok"
reasons <- c()
for (nm in names(decisions)) {
  d <- decisions[[nm]]
  reasons <- c(reasons, d$reasons)
  if (d$level == "alert") {
    final_level <- "alert"
  } else if (d$level == "attention" && final_level == "ok") {
    final_level <- "attention"
  }
}
decision_final <- list(level = final_level, reasons = reasons)

met_df <- tibble()
for (nm in names(metrics_today_list)) {
  met_df <- bind_rows(met_df, as_tibble(metrics_today_list[[nm]]) %>% mutate(window=nm, .before=1))
}
write_csv(met_df, file.path(cfg$runtime$output_dir, "metrics_today.csv"))

th_df <- tibble()
for (nm in names(thresholds_list)) {
  thr <- thresholds_list[[nm]]
  for (met in names(thr)) {
    th_df <- bind_rows(th_df, tibble(window=nm, metric=met, attention=thr[[met]]$attention, alert=thr[[met]]$alert, mean=thr[[met]]$mean, sd=thr[[met]]$sd))
  }
}
write_csv(th_df, file.path(cfg$runtime$output_dir, "thresholds.csv"))

write_csv(tibble(level=decision_final$level, reason=paste(decision_final$reasons, collapse=";")), file.path(cfg$runtime$output_dir, "decision.csv"))

print(met_df)
print(decision_final)
