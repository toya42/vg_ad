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
all_ref <- list()
for (nm in names(days_list)) {
  vecs <- list()
  for (d in days_list[[nm]]) {
    raw <- fetch_day(sensor_id, d, cfg)
    if (is.null(raw)) next
    prep <- preprocess_day(raw, cfg)
    if (is.null(prep)) next
    vecs[[length(vecs)+1]] <- prep$vec
  }
  if (length(vecs)>0) {
    all_ref[[nm]] <- do.call(rbind, vecs)
  }
}
Y_ref_all <- if (length(all_ref)>0) do.call(rbind, all_ref) else matrix(nrow=0, ncol=length(Y_today))

fpca_fit <- fit_fpca(Y_ref_all, prep_today$t_grid, cfg)
if (!is.null(fpca_fit)) message("使用PC数 = ", fpca_fit$pcnum, " | 累積寄与率 = ", round(fpca_fit$cumvar_used*100,1), "%")

scores_ref_all <- if (!is.null(fpca_fit) && nrow(Y_ref_all)>0) project_fpca_scores(Y_ref_all, fpca_fit) else matrix(nrow=nrow(Y_ref_all), ncol=0)
scores_today <- if (!is.null(fpca_fit)) project_fpca_scores(matrix(Y_today, nrow=1), fpca_fit) else matrix(nrow=1,ncol=0)
recon_today <- if (!is.null(fpca_fit)) reconstruct_from_fpca(scores_today, fpca_fit)[1,] else rep(NA, length(Y_today))

metrics_all <- compute_metrics(Y_ref_all, Y_today, scores_ref_all, scores_today, recon_today, fpca_fit, cfg)
metrics_today <- metrics_all$today

metrics_ref_list <- list()
for (nm in names(all_ref)) {
  Y_ref_mat <- all_ref[[nm]]
  scores_ref_w <- if (!is.null(fpca_fit) && nrow(Y_ref_mat)>0) project_fpca_scores(Y_ref_mat, fpca_fit) else matrix(nrow=nrow(Y_ref_mat), ncol=0)
  metrics_w <- compute_metrics(Y_ref_mat, Y_today, scores_ref_w, scores_today, recon_today, fpca_fit, cfg)
  metrics_ref_list[[nm]] <- metrics_w$ref
}

thresholds_list <- lapply(metrics_ref_list, fit_thresholds, cfg = cfg)
for (nm in names(thresholds_list)) {
  thresholds_list[[nm]]$fpca_scores_topk_z <- list(attention = cfg$decision$attention_sigma, alert = cfg$decision$alert_sigma)
}
if (length(thresholds_list) == 0) {
  thresholds_list[["default"]] <- list(fpca_scores_topk_z = list(attention = cfg$decision$attention_sigma, alert = cfg$decision$alert_sigma))
}

decisions <- list()
for (nm in names(thresholds_list)) {
  dec <- decide_today(metrics_today, thresholds_list[[nm]], cfg, context_info=list(window=nm))
  decisions[[nm]] <- dec
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

write_csv(as.data.frame(t(metrics_today)), file.path(cfg$runtime$output_dir, "metrics_today.csv"))

th_df <- tibble()
for (nm in names(thresholds_list)) {
  thr <- thresholds_list[[nm]]
  for (met in names(thr)) {
    th_df <- bind_rows(th_df, tibble(window=nm, metric=met, attention=thr[[met]]$attention, alert=thr[[met]]$alert, mean=thr[[met]]$mean, sd=thr[[met]]$sd))
  }
}
write_csv(th_df, file.path(cfg$runtime$output_dir, "thresholds.csv"))

write_csv(tibble(level=decision_final$level, reason=paste(decision_final$reasons, collapse=";")), file.path(cfg$runtime$output_dir, "decision.csv"))
if (!is.null(fpca_fit)) saveRDS(fpca_fit, file.path(cfg$runtime$output_dir, "fpca_model.rds"))

print(metrics_today)
print(decision_final)
