#!/usr/bin/env Rscript

# realdata-pipeline-3s.R (debuggable, English comments)
# - Build 3s-grid (22:00–02:00) windows for reference days and "today"
# - Run FPCA (prefer 'refund' if available, optionally force it)
# - Compute metrics -> thresholds -> decision
# - Rich debug logs, memory footprint, checkpoints, and stop-after stages
#
# Usage example:
# Rscript scripts/realdata-pipeline-3s.R \
#   --csv=data/vg09.csv --tz=UTC --today=2025-08-12 \
#   --ref-days=20 --valid-frac=0.8 \
#   --fpca-engine=refund --no-fallback \
#   --fpca-dt-sec=15 --debug --checkpoints --export-csv --export-plots

suppressPackageStartupMessages({
  library(optparse)
  library(pkgload)
})

# ---------- small utils ----------
timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

proc_mem <- function() {
  # Linux-only: read current process memory from /proc
  st <- readLines("/proc/self/status")
  pick <- function(key) {
    ln <- st[startsWith(st, key)]
    if (length(ln)) as.numeric(sub(".*:\\s*([0-9]+)\\s*kB.*", "\\1", ln))/1024 else NA
  }
  c(RSS_MiB = pick("VmRSS"), HWM_MiB = pick("VmHWM"))
}

# printf-style logger with optional memory stats
log_step <- function(fmt, ..., mem = TRUE) {
  msg <- sprintf(fmt, ...)
  if (mem) {
    m <- proc_mem()
    cat(sprintf("[%s] %s | RSS=%.1f MiB HWM=%.1f MiB\n",
                timestamp(), msg, m[["RSS_MiB"]], m[["HWM_MiB"]]))
  } else {
    cat(sprintf("[%s] %s\n", timestamp(), msg))
  }
  flush.console()
}

save_cp <- function(path, obj) {
  # Do not interrupt flow on failure; checkpoints are best-effort
  try(saveRDS(obj, path), silent = TRUE)
}

maybe_stop_after <- function(stage, target) {
  # If --stop-after matches 'stage', exit gracefully (for stepwise debugging)
  if (!is.na(target) && nzchar(target) && identical(stage, target)) {
    log_step("Reached stop-after='%s' -> exiting.", stage)
    quit(save = "no", status = 0)
  }
}

# dump str() to a text file (for diagnostics)
dump_str <- function(obj, path) {
  con <- file(path, open = "wt"); on.exit(close(con), add = TRUE)
  sink(con); on.exit(sink(), add = TRUE)
  str(obj)
}

# safe write.csv for data.frame or matrix
write_csv_silent <- function(x, path, row.names = FALSE) {
  try(write.csv(x, path, row.names = row.names), silent = TRUE)
}

# ---------- CLI ----------
opt_list <- list(
  make_option("--csv", type="character", default="data/vg09.csv", help="Input CSV path"),
  make_option("--today", type="character", help="YYYY-MM-DD (required)"),
  make_option("--ref-days", type="integer", default=45L, help="# of reference days"),
  make_option("--tz", type="character", default="UTC", help="Time zone"),
  make_option("--valid-frac", type="double", default=0.8, help="Min non-NA ratio per day"),
  make_option("--out-dir", type="character", default="out", help="Output directory"),
  # Debug / control
  make_option("--debug", action="store_true", default=FALSE, help="Verbose debug prints"),
  make_option("--checkpoints", action="store_true", default=FALSE,
              help="Save RDS checkpoints per stage"),
  make_option("--stop-after", type="character", default=NA,
              help="Stop after: build|fpca|scores|metrics|thresholds|decision|save"),
  # FPCA backend & downsampling
  make_option("--fpca-engine", type="character", default="auto",
              help="FPCA backend: auto|refund|prcomp"),
  make_option("--no-fallback", action="store_true", default=FALSE,
              help="When backend=refund, do NOT fallback to prcomp (stop on error)"),
  make_option("--fpca-dt-sec", type="integer", default=3L,
              help="Downsample only for FPCA (seconds). e.g., 9 turns 3s-grid into 9s for FPCA."),
  # Diagnostics export
  make_option("--export-csv",   action="store_true", default=FALSE,
              help="Export diagnostics as CSV files"),
  make_option("--export-plots", action="store_true", default=FALSE,
              help="Export quick-look PNG plots (base graphics)")
)
opt <- parse_args(OptionParser(option_list = opt_list))
# normalize hyphenated option names to local booleans
export_csv   <- isTRUE(opt$`export-csv`)
export_plots <- isTRUE(opt$`export-plots`)
if (is.na(opt$today)) stop("--today=YYYY-MM-DD is required")
dir.create(opt$`out-dir`, showWarnings = FALSE, recursive = TRUE)

# Debug error dump
if (opt$debug) {
  options(error = function() {
    dump.frames("last.dump", TRUE)
    cat("\n*** dump.frames() saved to out/last.dump.rds\n")
    try(saveRDS(get("last.dump", envir = .GlobalEnv),
                file = file.path(opt$`out-dir`, "last.dump.rds")), silent = TRUE)
    q(status = 1, save = "no")
  })
  log_step("DEBUG mode on.", mem = TRUE)
}

# ---------- load vgAd (dev) *BEFORE ANY vgAd:: calls ----------
# Prefer installed vgAd; otherwise load from the current repository (dev-mode)
if (!requireNamespace("vgAd", quietly = TRUE)) {
  message("Loading vgAd via pkgload::load_all()")
  suppressMessages(pkgload::load_all("."))
} else {
  library(vgAd)
}
if (opt$debug) {
  log_step("refund installed? %s", requireNamespace("refund", quietly = TRUE))
}

# ---------- I/O helpers ----------
read_input_csv <- function(path, tz = "UTC") {
  # Robust CSV loader that tolerates BOM and multiple datetime formats
  stopifnot(file.exists(path))
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(path, stringsAsFactors = FALSE)
  )
  # Normalize a possible BOM in the first column name
  names(df)[1] <- sub("\ufeff", "", names(df)[1])
  if (!"Date" %in% names(df)) stop("CSV must contain a 'Date' column")

  # Use the first non-Date column as the value column
  val_cols <- setdiff(names(df), "Date")
  if (length(val_cols) == 0) stop("No value column found in CSV")
  value_col <- val_cols[1]

  # Robust timestamp parsing (supports ISO8601, slash, with/without seconds/millis)
  df$Date <- as.POSIXct(
    df$Date, tz = tz,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
      "%m/%d/%Y %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%d %H:%M",     "%Y/%m/%d %H:%M"
    )
  )
  if (anyNA(df$Date)) stop("Failed to parse Date column; please check input formats.")
  df <- df[order(df$Date), ]
  list(df = df[, c("Date", value_col)], value_col = value_col)
}

make_window_grid <- function(day, tz = "UTC") {
  # Build a 3-second grid from 22:00:00 of 'day' for 4800 points (till 02:00 next day, exclusive)
  start <- as.POSIXct(sprintf("%s 22:00:00", format(day, "%Y-%m-%d")), tz = tz)
  seq(from = start, by = 3, length.out = 4800L)
}

resample_linear <- function(ts, y, grid) {
  # 1-D linear interpolation with deduplication by timestamp (mean for ties)
  x <- as.numeric(ts); gx <- as.numeric(grid)
  o <- order(x); x <- x[o]; y <- as.numeric(y[o]); y[!is.finite(y)] <- NA_real_
  ag <- aggregate(y ~ x, FUN = function(v) mean(v, na.rm = TRUE))
  x <- ag$x; y <- ag$y
  approx(x = x, y = y, xout = gx, method = "linear", rule = 1, ties = "ordered")$y
}

build_window_matrix_3s <- function(csv_path, tz = "UTC", valid_frac = 0.8) {
  # Build day-wise 3s-grid windows (22:00–02:00) from CSV
  info <- read_input_csv(csv_path, tz)
  df <- info$df; names(df) <- c("Date", "value")

  days_all <- seq.Date(as.Date(min(df$Date, na.rm = TRUE)),
                       as.Date(max(df$Date, na.rm = TRUE)), by = "day")
  Y_list <- vector("list", length(days_all))
  valid <- logical(length(days_all))

  for (i in seq_along(days_all)) {
    g <- make_window_grid(days_all[i], tz)
    # Include a 1-hour tail buffer to capture data slightly beyond 02:00 (interpolation safety)
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

  Y <- do.call(rbind, Y_list)
  rownames(Y) <- as.character(days_all)
  list(days = days_all, Y = Y, valid = valid, tz = tz, value_col = info$value_col)
}

# ---------- FPCA-only downsampling ----------
# Keep the original 3s grid for build/outputs, but allow coarser view for FPCA to avoid OOM.
build_fpca_view <- function(Y_ref, Y_today, base_dt = 3L, fpca_dt = 3L) {
  if (fpca_dt <= base_dt) {
    list(Y_ref = Y_ref, Y_today = Y_today, idx = seq_len(ncol(Y_ref)))
  } else {
    step <- as.integer(round(fpca_dt / base_dt))
    if (step < 1) step <- 1L
    idx <- seq(1L, ncol(Y_ref), by = step)
    list(Y_ref = Y_ref[, idx, drop = FALSE],
         Y_today = Y_today[, idx, drop = FALSE],
         idx = idx)
  }
}

# ---------- config ----------
make_dummy_cfg <- function() {
  # Minimal config compatible with vgAd::fit_fpca / compute_metrics / fit_thresholds / decide_today
  list(
    model = list(fpca = list(min_pc = 2L, cumvar_min = 0.95)),
    preprocess = list(resample = list(dt_sec = 3)),
    metrics = list(
      fpca_scores     = list(use = TRUE, top_k = 5L),
      recon_mse       = list(use = TRUE),
      level_median    = list(use = TRUE),
      slope_linear    = list(use = TRUE),
      variability_iqr = list(use = TRUE),
      quietness_flag     = list(use = FALSE, threshold_iqr = 0.05),
      high_pressure_flag = list(use = FALSE, threshold_logpa = 10.0)
    ),
    thresholds = list(
      scheme = "quantile", quantile_p = 0.99, min_samples = 5L,
      fallback_scheme = "quantile"
    ),
    decision = list(attention_sigma = 2, alert_sigma = 3)
  )
}
cfg <- tryCatch(vgAd::load_config("config.yaml"),
                error = function(e) make_dummy_cfg())

# ---------- build matrix ----------
log_step("STEP build: start")
built <- build_window_matrix_3s(opt$csv, tz = opt$tz, valid_frac = opt$`valid-frac`)

days <- built$days
Y     <- built$Y

today <- as.Date(opt$today, tz = opt$tz)
if (!today %in% days) stop("Specified --today not found in data days")
today_idx   <- which(days == today)
ref_idx_all <- which(days < today & built$valid)
if (length(ref_idx_all) == 0) stop("No valid reference days before --today")
ref_idx <- tail(ref_idx_all, opt$`ref-days`)

Y_ref   <- Y[ref_idx, , drop = FALSE]
Y_today <- matrix(Y[today_idx, ], nrow = 1)

cat(sprintf("Reference days: %d (from %s to %s)\n",
            nrow(Y_ref), min(days[ref_idx]), max(days[ref_idx])))
cat(sprintf("Today: %s (index %d)\n", as.character(today), today_idx))
cat(sprintf("Matrix dims: ref %d x %d, today 1 x %d\n",
            nrow(Y_ref), ncol(Y_ref), ncol(Y_today)))

if (opt$checkpoints) {
  save_cp(file.path(opt$`out-dir`, "cp_build.rds"),
          list(days = days, ref_idx = ref_idx, today_idx = today_idx))
}
maybe_stop_after("build", opt$`stop-after`)
log_step("STEP build: end")

# ---------- FPCA view (downsample only for FPCA) ----------
base_dt <- 3L
fpca_view <- build_fpca_view(Y_ref, Y_today, base_dt = base_dt, fpca_dt = opt$`fpca-dt-sec`)
Y_ref_use   <- fpca_view$Y_ref
Y_today_use <- fpca_view$Y_today
t_grid      <- seq(0, 1, length.out = ncol(Y_ref_use))

log_step("FPCA view: dt=%ds -> cols %d (from %d)", opt$`fpca-dt-sec`, ncol(Y_ref_use), ncol(Y_ref))
log_step("FPCA input dims: ref=%d x %d, today=1 x %d",
         nrow(Y_ref_use), ncol(Y_ref_use), ncol(Y_today_use))
log_step("FPCA input NA frac(ref)=%.4f", mean(is.na(Y_ref_use)))
if (opt$checkpoints) {
  save_cp(file.path(opt$`out-dir`, "cp_fpca_input.rds"),
          list(idx = fpca_view$idx,
               Y_ref_head = Y_ref_use[seq_len(min(5, nrow(Y_ref_use))), 1:min(50, ncol(Y_ref_use))],
               t_grid = t_grid))
}

# ---------- FPCA ----------
log_step("STEP fpca: start (engine=%s)", opt$`fpca-engine`)

# Decide backend and enforce it where possible
engine <- opt$`fpca-engine`
if (engine == "prcomp") {
  # Force prcomp fallback by unloading refund if loaded
  if ("refund" %in% loadedNamespaces()) try(unloadNamespace("refund"), silent = TRUE)
} else if (engine == "refund") {
  if (!requireNamespace("refund", quietly = TRUE)) {
    stop("backend=refund was requested but 'refund' is not installed")
  }
} else if (engine == "auto") {
  # Prefer refund if available; otherwise prcomp (handled inside fit_fpca)
  # No explicit action needed here
} else {
  stop("Unknown --fpca-engine value (use: auto|refund|prcomp)")
}

fpca_try <- function() vgAd::fit_fpca(Y_ref_use, t_grid, cfg)

fpca_fit <- NULL
gc(); log_step("Calling vgAd::fit_fpca() ...")
fpca_fit <- tryCatch(
  fpca_try(),
  error = function(e) {
    log_step("vgAd::fit_fpca error: %s", conditionMessage(e))
    # Fallback policy
    if (engine == "refund" && opt$`no-fallback`) {
      stop("backend=refund with --no-fallback -> stopping on error")
    }
    if (engine %in% c("refund", "auto")) {
      # Try prcomp fallback
      if ("refund" %in% loadedNamespaces()) try(unloadNamespace("refund"), silent = TRUE)
      gc(); log_step("Retrying FPCA with prcomp fallback ...")
      return(tryCatch(vgAd::fit_fpca(Y_ref_use, t_grid, cfg),
                      error = function(e2) {
                        log_step("prcomp fallback also failed: %s", conditionMessage(e2))
                        NULL
                      }))
    }
    NULL
  }
)

if (is.null(fpca_fit)) stop("FPCA fit failed (NULL)")
if (opt$checkpoints) save_cp(file.path(opt$`out-dir`, "cp_fpca.rds"), list(fpca = fpca_fit))
maybe_stop_after("fpca", opt$`stop-after`)
log_step("STEP fpca: end")

# ----- FPCA diagnostics export -----
if (export_csv) {
  dump_str(fpca_fit, file.path(opt$`out-dir`,
           sprintf("diag-fpca-fit-%s.txt", as.character(today))))
}

# ---------- scores & reconstruction ----------
log_step("STEP scores: start")
scores_ref   <- vgAd::project_fpca_scores(Y_ref_use,   fpca_fit)
scores_today <- vgAd::project_fpca_scores(Y_today_use, fpca_fit)
recon_today  <- vgAd::reconstruct_from_fpca(scores_today, fpca_fit)

# Ensure 2D shape for downstream
if (!is.matrix(scores_today)) scores_today <- matrix(scores_today, nrow = 1)

if (opt$debug) {
  cat(sprintf("scores_ref dim: %d x %d\n",  NROW(scores_ref),  NCOL(scores_ref)))
  cat(sprintf("scores_today dim: %d x %d\n", NROW(scores_today), NCOL(scores_today)))
}
if (opt$checkpoints) {
  save_cp(file.path(opt$`out-dir`, "cp_scores.rds"),
          list(scores_ref=scores_ref, scores_today=scores_today))
}
maybe_stop_after("scores", opt$`stop-after`)
log_step("STEP scores: end")

# ----- Scores / reconstruction diagnostics -----
if (export_csv) {
  # Save today's FPCA-view series and reconstruction (coarser grid if --fpca-dt-sec > 3)
  t_norm <- t_grid
  today_df <- data.frame(
    t_norm = t_norm,
    y_today = as.numeric(Y_today_use[1, ]),
    y_recon = as.numeric(recon_today)
  )
  write_csv_silent(today_df,
    file.path(opt$`out-dir`, sprintf("diag-today-series-%s.csv", as.character(today))))

  # Save scores (ref + today)
  sc_ref_df <- as.data.frame(scores_ref); sc_ref_df$day <- as.character(days[ref_idx])
  write_csv_silent(sc_ref_df,
    file.path(opt$`out-dir`, sprintf("diag-scores-ref-%s.csv", as.character(today))))
  sc_today_df <- as.data.frame(scores_today); sc_today_df$day <- as.character(today)
  write_csv_silent(sc_today_df,
    file.path(opt$`out-dir`, sprintf("diag-scores-today-%s.csv", as.character(today))))
}

if (export_plots) {
  # Quick-look overlay plot (base graphics)
  png(file.path(opt$`out-dir`, sprintf("plot-today-vs-recon-%s.png", as.character(today))),
      width = 1200, height = 500)
  par(mar=c(4,4,1,1))
  x <- seq_along(recon_today)
  plot(x, as.numeric(Y_today_use[1, ]), type="l", xlab="grid index (FPCA view)",
       ylab="value")
  lines(x, as.numeric(recon_today))
  legend("topleft", col=c("black","black"), lty=c(1,1),
         legend=c("today (FPCA view)","reconstruction"), bty="n")
  dev.off()
}

# ---------- metrics (today & ref) ----------
log_step("STEP metrics: start")
# vgAd::compute_metrics returns list(today=<list>, ref=<data.frame>)
met_all <- vgAd::compute_metrics(
  Y_ref = Y_ref_use,
  Y_today = as.numeric(Y_today_use[1, ]),
  scores_ref = scores_ref,
  scores_today = scores_today,
  recon_today = as.numeric(recon_today),
  fpca_fit = fpca_fit,
  cfg = cfg
)
metrics_today <- met_all$today
metrics_ref   <- met_all$ref
if (opt$debug) {
  cat("metrics_today names: ", paste(names(metrics_today), collapse=", "), "\n", sep="")
  cat("metrics_ref cols: ", paste(colnames(metrics_ref), collapse=", "), "\n", sep="")
}
if (opt$checkpoints) {
  save_cp(file.path(opt$`out-dir`, "cp_metrics.rds"),
          list(metrics_today = metrics_today, metrics_ref = metrics_ref))
}
maybe_stop_after("metrics", opt$`stop-after`)
log_step("STEP metrics: end")

# Export metrics diagnostics
if (export_csv) {
  write_csv_silent(metrics_ref,
    file.path(opt$`out-dir`, sprintf("diag-metrics-ref-%s.csv", as.character(today))))
  write_csv_silent(as.data.frame(metrics_today),
    file.path(opt$`out-dir`, sprintf("diag-metrics-today-%s.csv", as.character(today))))
}

# ---------- thresholds ----------
log_step("STEP thresholds: start")
thresholds <- vgAd::fit_thresholds(metrics_ref, cfg)
if (opt$checkpoints) save_cp(file.path(opt$`out-dir`, "cp_thresholds.rds"), thresholds)
maybe_stop_after("thresholds", opt$`stop-after`)
log_step("STEP thresholds: end")

# Export thresholds diagnostics
if (export_csv) {
  thr <- thresholds
  if (is.list(thr) && !is.data.frame(thr)) {
    try(saveRDS(thr, file.path(opt$`out-dir`,
        sprintf("diag-thresholds-%s.rds", as.character(today)))), silent=TRUE)
    flat <- data.frame(name = names(thr), value = I(unname(thr)))
    write_csv_silent(flat,
      file.path(opt$`out-dir`, sprintf("diag-thresholds-%s.csv", as.character(today))))
  } else {
    write_csv_silent(thr,
      file.path(opt$`out-dir`, sprintf("diag-thresholds-%s.csv", as.character(today))))
  }
}

# ---------- decision ----------
log_step("STEP decision: start")
decision <- vgAd::decide_today(
  metrics_today, thresholds, cfg,
  context_info = list(window = "22:00–02:00", date = as.character(today))
)
maybe_stop_after("decision", opt$`stop-after`)
log_step("STEP decision: end")

# ---------- save ----------
log_step("STEP save: start")
out_rds <- file.path(opt$`out-dir`, sprintf("pipeline-3s-%s.rds", as.character(today)))
saveRDS(list(
  days = days, ref_idx = ref_idx, today_idx = today_idx,
  fpca = fpca_fit,
  scores_ref = scores_ref, scores_today = scores_today,
  metrics_today = metrics_today, metrics_ref = metrics_ref,
  thresholds = thresholds, decision = decision
), file = out_rds)

cat("\n== Decision ==\n"); print(decision)
cat("\nArtifacts saved to:\n - ", out_rds, "\n", sep = "")
log_step("STEP save: end")
