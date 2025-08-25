#!/usr/bin/env Rscript

# End-to-end pipeline using 3-second interpolated windows (22:00–02:00).
# - Builds per-day matrix from real CSV
# - Selects reference days and today
# - Runs FPCA + metrics + thresholds + decision using vgAd package
#
# Usage:
#   Rscript scripts/realdata-pipeline-3s.R --csv=data/vg09.csv --today=2025-08-20 --ref-days=45 --tz=UTC
#
# Outputs:
#   Prints decision summary; saves artifacts to out/ (scores/thresholds/decision).

suppressPackageStartupMessages({
  library(optparse)
  library(pkgload)
})

# ---------- CLI ----------
opt_list <- list(
  make_option("--csv", type = "character", default = "data/vg09.csv",
              help = "Input CSV path [default: %default]"),
  make_option("--today", type = "character", default = NA,
              help = "Target day (YYYY-MM-DD) for decision [required]"),
  make_option("--ref-days", type = "integer", default = 45L,
              help = "Number of reference days before 'today' [default: %default]"),
  make_option("--tz", type = "character", default = "UTC",
              help = "Timezone for parsing timestamps [default: %default]"),
  make_option("--valid-frac", type = "double", default = 0.8,
              help = "Min non-NA ratio per day to keep [default: %default]"),
  make_option("--out-dir", type = "character", default = "out",
              help = "Output directory [default: %default]")
)
opt <- parse_args(OptionParser(option_list = opt_list))
if (is.na(opt$today)) stop("--today=YYYY-MM-DD is required")

dir.create(opt$`out-dir`, showWarnings = FALSE, recursive = TRUE)

# ---------- Import vgAd (installed or dev) ----------
if (!requireNamespace("vgAd", quietly = TRUE)) {
  message("Loading vgAd via pkgload::load_all()")
  suppressMessages(pkgload::load_all("."))
} else {
  library(vgAd)
}

# ---------- Reuse window builder (embedded minimal copy) ----------
read_input_csv <- function(path, tz = "UTC") {
  stopifnot(file.exists(path))
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(path, stringsAsFactors = FALSE)
  )
  names(df)[1] <- sub("\ufeff", "", names(df)[1])
  if (!"Date" %in% names(df)) stop("CSV must have 'Date' column")
  val_cols <- setdiff(names(df), "Date")
  if (length(val_cols) == 0) stop("No value column found")
  value_col <- val_cols[1]
  df$Date <- as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (anyNA(df$Date)) df$Date <- as.POSIXct(df$Date, tz = tz)
  df <- df[order(df$Date), ]
  list(df = df[, c("Date", value_col)], value_col = value_col)
}
make_window_grid <- function(day, tz = "UTC") {
  start <- as.POSIXct(sprintf("%s 22:00:00", format(day, "%Y-%m-%d")), tz = tz)
  seq(from = start, by = 3, length.out = 4800L)
}
resample_linear <- function(ts, y, grid) {
  x <- as.numeric(ts); gx <- as.numeric(grid)
  o <- order(x); x <- x[o]; y <- as.numeric(y[o]); y[!is.finite(y)] <- NA_real_
  ag <- aggregate(y ~ x, FUN = function(v) mean(v, na.rm = TRUE))
  x <- ag$x; y <- ag$y
  approx(x = x, y = y, xout = gx, method = "linear", rule = 1, ties = "ordered")$y
}
build_window_matrix_3s <- function(csv_path, tz = "UTC", valid_frac = 0.8) {
  info <- read_input_csv(csv_path, tz)
  df <- info$df; names(df) <- c("Date", "value")
  days_all <- seq.Date(as.Date(min(df$Date, na.rm = TRUE)),
                       as.Date(max(df$Date, na.rm = TRUE)), by = "day")
  Y_list <- vector("list", length(days_all))
  valid <- logical(length(days_all))
  for (i in seq_along(days_all)) {
    g <- make_window_grid(days_all[i], tz)
    start_buf <- g[1]; end_buf <- g[length(g)] + 3600
    idx <- which(df$Date >= start_buf & df$Date <= end_buf)
    if (length(idx) == 0) {
      y_i <- rep(NA_real_, length(g))
    } else {
      y_i <- resample_linear(df$Date[idx], df$value[idx], g)
    }
    Y_list[[i]] <- y_i
    valid[i] <- mean(!is.na(y_i)) >= valid_frac
  }
  Y <- do.call(rbind, Y_list); rownames(Y) <- as.character(days_all)
  list(days = days_all, Y = Y, valid = valid, tz = tz, value_col = info$value_col)
}

# ---------- Config (use package config if available, else sensible defaults) ----------
make_dummy_cfg <- function() {
  list(
    tz = opt$tz,
    fpca = list(center = TRUE, pve = 0.95, npc_max = 10L),
    metrics = list(
      fpca_scores = list(use = TRUE),
      reconstruction = list(use = TRUE),
      basic_stats = list(use = TRUE) # mean/max etc. if your package uses them
    ),
    thresholds = list(
      method = "quantile",
      probs = 0.95
    ),
    decision = list(
      rules = list(
        high_pressure_flag = TRUE
      )
    )
  )
}

cfg <- tryCatch(vgAd::load_config("config.yaml"),
                error = function(e) make_dummy_cfg())

# ---------- Build matrix ----------
built <- build_window_matrix_3s(opt$csv, tz = opt$tz, valid_frac = opt$`valid-frac`)
days <- built$days
Y <- built$Y

today <- as.Date(opt$today, tz = opt$tz)
if (!today %in% days) stop("Specified --today not found in data days")

today_idx <- which(days == today)
ref_idx_all <- which(days < today & built$valid)
if (length(ref_idx_all) == 0) stop("No valid reference days before --today")

ref_idx <- tail(ref_idx_all, opt$`ref-days`)
Y_ref <- Y[ref_idx, , drop = FALSE]
Y_today <- matrix(Y[today_idx, ], nrow = 1)

# Normalize time grid [0, 1] for FPCA
t_grid <- seq(0, 1, length.out = ncol(Y_ref))

cat(sprintf("Reference days: %d (from %s to %s)\n",
            nrow(Y_ref), min(days[ref_idx]), max(days[ref_idx])))
cat(sprintf("Today: %s (index %d)\n", as.character(today), today_idx))
cat(sprintf("Matrix dims: ref %d x %d, today 1 x %d\n",
            nrow(Y_ref), ncol(Y_ref), ncol(Y_today)))

# ---------- FPCA ----------
fpca_fit <- vgAd::fit_fpca(Y_ref, t_grid, cfg)
scores_ref <- vgAd::project_fpca_scores(Y_ref, fpca_fit)
scores_today <- vgAd::project_fpca_scores(Y_today, fpca_fit)
recon_today <- vgAd::reconstruct_from_fpca(scores_today, fpca_fit)

# ---------- Reference metric distribution (leave-one-out to avoid leakage) ----------
compute_one <- function(i) {
  Y_ref_i <- Y_ref[i, , drop = FALSE]
  scores_ref_loo <- scores_ref[-i, , drop = FALSE]
  Y_ref_loo <- Y_ref[-i, , drop = FALSE]
  scores_today_i <- vgAd::project_fpca_scores(Y_ref_i, fpca_fit)
  recon_today_i <- vgAd::reconstruct_from_fpca(scores_today_i, fpca_fit)
  vgAd::compute_metrics(
    Y_ref = Y_ref_loo,
    Y_today = Y_ref_i,
    scores_ref = scores_ref_loo,
    scores_today = scores_today_i,
    recon_today = recon_today_i,
    fpca_fit = fpca_fit,
    cfg = cfg
  )
}

metric_values_ref <- do.call(rbind, lapply(seq_len(nrow(Y_ref)), compute_one))
metric_values_ref <- as.data.frame(metric_values_ref)

# ---------- Today metrics, thresholds, decision ----------
metrics_today <- vgAd::compute_metrics(
  Y_ref = Y_ref,
  Y_today = Y_today,
  scores_ref = scores_ref,
  scores_today = scores_today,
  recon_today = recon_today,
  fpca_fit = fpca_fit,
  cfg = cfg
)

thresholds <- vgAd::fit_thresholds(metric_values_ref, cfg)
decision <- vgAd::decide_today(metrics_today, thresholds, cfg, context_info = list())

# ---------- Save + print ----------
saveRDS(list(
  days = days,
  ref_idx = ref_idx,
  today_idx = today_idx,
  fpca = fpca_fit,
  scores_ref = scores_ref,
  scores_today = scores_today,
  metrics_today = metrics_today,
  metric_values_ref = metric_values_ref,
  thresholds = thresholds,
  decision = decision
), file = file.path(opt$`out-dir`, sprintf("pipeline-3s-%s.rds", as.character(today))))

cat("\n== Decision ==\n")
print(decision)
cat("\nArtifacts saved to:\n")
cat(sprintf(" - %s\n", file.path(opt$`out-dir`, sprintf("pipeline-3s-%s.rds", as.character(today)))))
