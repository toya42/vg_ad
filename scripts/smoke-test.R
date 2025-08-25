#!/usr/bin/env Rscript
cat("== vgAd smoke test ==\n")

if (!requireNamespace("vgAd", quietly = TRUE)) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    install.packages("pkgload", repos = "https://cloud.r-project.org")
  }
  pkgload::load_all(".")
  cat("Loaded package with pkgload::load_all()\n")
} else {
  library(vgAd)
  cat("Loaded installed vgAd\n")
}

ok <- function(name, expr) {
  t0 <- proc.time()[3]
  out <- try(eval.parent(substitute(expr)), silent = TRUE)
  dt <- sprintf("(%.3fs)", proc.time()[3] - t0)
  if (inherits(out, "try-error")) {
    cat(sprintf("[SKIP] %s: %s %s\n\n", name, attr(out, "condition")$message, dt))
    return(list(ok = FALSE, value = NULL))
  } else {
    cat(sprintf("[ OK ] %s %s\n\n", name, dt))
    return(list(ok = TRUE, value = out))
  }
}

cfg <- NULL
if (file.exists("config.yaml")) {
  cfg <- vgAd:::load_config("config.yaml")
} else {
  cfg <- list(
    runtime = list(timezone = "UTC", output_dir = "out", log_dir = "logs"),
    preprocess = list(
      epsilon = 1e-6,
      clip_log = list(min = -3, max = 3),
      window_utc = list(start = "00:00", end = "23:59"),
      resample = list(dt_sec = 900, method = "linear"),
      max_missing_ratio = 1
    ),
    model = list(fpca = list(cumvar_min = 0.95, min_pc = 2), per_window = FALSE),
    metrics = list(
      fpca_scores = list(use = TRUE, top_k = 3),
      recon_mse = list(use = TRUE),
      level_median = list(use = TRUE),
      slope_linear = list(use = TRUE),
      variability_iqr = list(use = TRUE),
      quietness_flag = list(use = TRUE, threshold_iqr = 0.05),
      high_pressure_flag = list(use = TRUE, threshold_logpa = 0.5)
    ),
    decision = list(attention_sigma = 2, alert_sigma = 3)
  )
  dir.create(cfg$runtime$output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(cfg$runtime$log_dir,   showWarnings = FALSE, recursive = TRUE)
}
cat("-- config ready --\n\n")

set.seed(42)
T <- 96
t_grid_num <- seq(0, 24, length.out = T)
mk_day <- function(amp = 1, noise = 0.05) amp*sin(2*pi*t_grid_num/24) + 0.2*cos(2*pi*t_grid_num/8) + rnorm(T, 0, noise)

Y_ref <- t(replicate(12, mk_day(amp = runif(1, 0.8, 1.2), noise = 0.08)))
Y_today <- mk_day(amp = 0.9, noise = 0.08) + rnorm(T, 0, 0.03)

cat(sprintf("ref matrix: %d days x %d timepoints\n\n", nrow(Y_ref), ncol(Y_ref)))

res_fit <- ok("fit_fpca", {
  vgAd::fit_fpca(Y_ref, t_grid_num, cfg)
})
fpca_fit <- res_fit$value

S_ref   <- ok("project_fpca_scores(ref)",   { vgAd::project_fpca_scores(Y_ref, fpca_fit) })$value
S_today <- ok("project_fpca_scores(today)", { vgAd::project_fpca_scores(matrix(Y_today, nrow = 1), fpca_fit) })$value
recon_1 <- ok("reconstruct_from_fpca(today)", { vgAd::reconstruct_from_fpca(S_today, fpca_fit) })$value
recon_today <- as.numeric(recon_1[1, ])

res_metrics <- ok("compute_metrics", {
  vgAd::compute_metrics(Y_ref, Y_today, S_ref, S_today, recon_today, fpca_fit, cfg)
})
metrics_today <- res_metrics$value$today
metrics_ref   <- res_metrics$value$ref

thresholds <- ok("fit_thresholds", {
  th <- vgAd::fit_thresholds(metrics_ref, cfg)
  th$fpca_scores_topk_z <- list(attention = cfg$decision$attention_sigma,
                                alert     = cfg$decision$alert_sigma)
  th
})$value

decision <- ok("decide_today", {
  vgAd::decide_today(metrics_today, thresholds, cfg)
})$value

cat("== Summary ==\n")
print(list(
  steps_ok = list(
    fit_fpca                 = res_fit$ok,
    `project_fpca_scores(ref)`   = !is.null(S_ref),
    `project_fpca_scores(today)` = !is.null(S_today),
    `reconstruct_from_fpca(today)` = !is.null(recon_today),
    compute_metrics          = res_metrics$ok,
    fit_thresholds           = !is.null(thresholds),
    decide_today             = !is.null(decision)
  ),
  decision = decision
))
cat("\n")
