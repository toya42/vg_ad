#!/usr/bin/env Rscript
# Purpose: End-to-end pipeline on real data window (22:00–02:00, 15-min bins)
# Inputs (created by scripts/realdata-window-test.R):
#   - out/realdata-window-Y.csv       (days x bins numeric matrix, may contain "NA" strings)
#   - out/realdata-window-days.csv    (one column 'day' in YYYY-MM-DD)
#   - out/realdata-window-meta.rds    (metadata incl. tz/bin/window info)
# Output:
#   - out/realdata-pipeline-summary.txt
#   - out/realdata-pipeline.rds

suppressPackageStartupMessages({
  if (!requireNamespace("vgAd", quietly = TRUE)) {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
      install.packages("pkgload", repos = "https://cloud.r-project.org")
    }
    pkgload::load_all(".")  # dev mode
    cat("Loaded vgAd with pkgload::load_all()\n")
  } else {
    library(vgAd)
    cat("Loaded vgAd as installed package\n")
  }
})

# ---------- CLI args (optional) ----------
# --today=YYYY-MM-DD (default: latest valid day)
# --ref-days=N       (default: 60; use last N valid days before 'today')
args <- commandArgs(trailingOnly = TRUE)
argval <- function(key, default = NULL) {
  hit <- grep(paste0("^", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", key, "="), "", hit[1])
}
today_arg   <- argval("--today",  default = NA)
refdays_arg <- as.integer(argval("--ref-days", default = "60"))

# ---------- Paths ----------
y_csv    <- "out/realdata-window-Y.csv"
days_csv <- "out/realdata-window-days.csv"
meta_rds <- "out/realdata-window-meta.rds"

stopifnot(file.exists(y_csv), file.exists(days_csv), file.exists(meta_rds))

# ---------- Load inputs with robust numeric coercion ----------
# Read Y as data.frame; convert to numeric matrix safely even if there are "NA" strings.
dfY <- read.csv(y_csv, check.names = FALSE, na.strings = c("", "NA", "NaN"))
Y   <- as.matrix(dfY)
# Force numeric mode (will coerce and produce NA where needed)
suppressWarnings(storage.mode(Y) <- "double")
# Replace non-finite values with NA
Y[!is.finite(Y)] <- NA_real_

days_df <- read.csv(days_csv, stringsAsFactors = FALSE)
if (!"day" %in% names(days_df)) stop("days CSV must contain a 'day' column.")
days <- as.Date(days_df$day)

meta <- readRDS(meta_rds)

cat(sprintf(
  "Loaded matrix: %d days x %d bins | tz=%s | bin=%dmin | window=%s-%s\n",
  nrow(Y), ncol(Y),
  meta$tz, meta$bin_minutes,
  format(meta$window_start, "%H:%M"), format(meta$window_end, "%H:%M")
))

# ---------- Keep rows with sufficient data & simple in-row interpolation ----------
enough <- rowMeans(!is.na(Y)) >= 0.8
Y    <- Y[enough, , drop = FALSE]
days <- days[enough]

fill_row_na <- function(x) {
  # Linear interpolation across time bins; carry edges if needed.
  idx <- which(!is.na(x))
  if (length(idx) == 0) return(x)
  if (length(idx) == 1) return(rep(x[idx], length(x)))
  all <- seq_along(x)
  approx(x = idx, y = x[idx], xout = all, method = "linear", rule = 2)$y
}
if (anyNA(Y)) {
  for (i in seq_len(nrow(Y))) if (anyNA(Y[i, ])) Y[i, ] <- fill_row_na(Y[i, ])
}

# Extra guard: drop columns that remain all-NA after interpolation (should not happen)
all_na_cols <- which(colSums(is.na(Y)) == nrow(Y))
if (length(all_na_cols) > 0) {
  Y <- Y[, -all_na_cols, drop = FALSE]
  warning(sprintf("Dropped %d all-NA columns after interpolation.", length(all_na_cols)))
}

# ---------- Choose today and reference set ----------
latest_valid_day <- max(days, na.rm = TRUE)
today <- if (is.na(today_arg)) latest_valid_day else as.Date(today_arg)
if (!(today %in% days)) stop(sprintf("Today (%s) not found in available days.", today))

today_idx <- which(days == today)[1]
ref_idx   <- which(days < today)
if (length(ref_idx) == 0) stop("No reference days available before 'today'.")
if (length(ref_idx) > refdays_arg) ref_idx <- tail(ref_idx, refdays_arg)

Y_ref   <- Y[ref_idx, , drop = FALSE]
Y_today <- as.numeric(Y[today_idx, ])

# ---------- Time grid (0..4 hours) ----------
hours_span <- 4
t_grid <- seq(0, hours_span, length.out = ncol(Y))  # numeric, monotone

# ---------- Minimal config for vgAd pipeline ----------
cfg <- list(
  window = list(label = "22:00-02:00", hours = hours_span, bin_minutes = meta$bin_minutes),
  fpca   = list(pve = 0.95, npc_max = min(8, ncol(Y)), center = TRUE),
  metrics = list(
    fpca_scores    = list(use = TRUE),
    reconstruction = list(use = TRUE),
    pressure       = list(use = TRUE)
  ),
  thresholds = list(
    fpca_scores         = list(q = 0.99),
    reconstruction_rmse = list(q = 0.99)
  )
)

# ---------- Fit FPCA on reference ----------
fpca_fit     <- vgAd::fit_fpca(Y_ref, t_grid, cfg)
scores_ref   <- vgAd::project_fpca_scores(Y_ref, fpca_fit)
scores_today <- vgAd::project_fpca_scores(matrix(Y_today, nrow = 1), fpca_fit)[1, ]
recon_today  <- as.numeric(vgAd::reconstruct_from_fpca(scores_today, fpca_fit))

# ---------- Metrics on today ----------
metrics_today <- vgAd::compute_metrics(
  Y_ref         = Y_ref,
  Y_today       = Y_today,
  scores_ref    = scores_ref,
  scores_today  = scores_today,
  recon_today   = recon_today,
  fpca_fit      = fpca_fit,
  cfg           = cfg
)

# ---------- Metrics on reference set (for thresholds) ----------
metric_values_ref <- NULL
for (i in seq_len(nrow(Y_ref))) {
  yi <- as.numeric(Y_ref[i, ])
  si <- as.numeric(scores_ref[i, ])
  ri <- as.numeric(vgAd::reconstruct_from_fpca(si, fpca_fit))
  mi <- vgAd::compute_metrics(
    Y_ref        = Y_ref,
    Y_today      = yi,
    scores_ref   = scores_ref,
    scores_today = si,
    recon_today  = ri,
    fpca_fit     = fpca_fit,
    cfg          = cfg
  )
  metric_values_ref <- rbind(metric_values_ref, as.data.frame(as.list(mi)))
}
thresholds <- vgAd::fit_thresholds(metric_values_ref, cfg)

# ---------- Decision ----------
decision <- vgAd::decide_today(
  metrics_today = metrics_today,
  thresholds    = thresholds,
  cfg           = cfg,
  context_info  = list(date = as.character(today))
)

# ---------- Report ----------
cat("\n== Pipeline summary ==\n")
cat(sprintf("today: %s\n", as.character(today)))
cat(sprintf("reference days: %d (from %s to %s)\n",
            nrow(Y_ref), as.character(min(days[ref_idx])), as.character(max(days[ref_idx]))))
cat(sprintf("FPCA components: %d (pve=%.2f)\n",
            if (!is.null(ncol(scores_ref))) ncol(scores_ref) else length(scores_ref),
            cfg$fpca$pve))
cat("\nDecision:\n")
print(decision)

# Save artifacts
dir.create("out", showWarnings = FALSE)
saveRDS(
  list(
    today = today,
    ref_days = days[ref_idx],
    fpca_fit = fpca_fit,
    scores_ref = scores_ref,
    scores_today = scores_today,
    recon_today = recon_today,
    metrics_today = metrics_today,
    metric_values_ref = metric_values_ref,
    thresholds = thresholds,
    decision = decision,
    cfg = cfg
  ),
  file = "out/realdata-pipeline.rds"
)

sink("out/realdata-pipeline-summary.txt")
cat("== vgAd realdata pipeline ==\n")
cat(sprintf("today: %s\n", as.character(today)))
cat(sprintf("ref_days: %d\n", length(ref_idx)))
cat(sprintf("matrix: %d days x %d bins\n", nrow(Y), ncol(Y)))
cat(sprintf("tz=%s | bin=%dmin | window=%s-%s\n",
            meta$tz, meta$bin_minutes,
            format(meta$window_start, "%H:%M"), format(meta$window_end, "%H:%M")))
cat(sprintf("fpca pve=%.2f npc_max=%d\n", cfg$fpca$pve, cfg$fpca$npc_max))
cat("\n-- decision --\n")
capture.output(print(decision), file = "", append = TRUE)
cat("\n-- metrics_today --\n")
capture.output(print(metrics_today), file = "", append = TRUE)
cat("\n-- thresholds (str) --\n")
capture.output(str(thresholds), file = "", append = TRUE)
sink()

cat("\nArtifacts saved:\n - out/realdata-pipeline.rds\n - out/realdata-pipeline-summary.txt\n")
