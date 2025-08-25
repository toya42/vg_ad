#!/usr/bin/env Rscript

# Build per-day windows (22:00–02:00) by linearly interpolating to 3-second grid.
# Saves artifacts to out/: days CSV, matrix CSV, meta RDS.
# Usage:
#   Rscript scripts/realdata-window-3s.R --csv=data/vg09.csv --tz=UTC --valid-frac=0.8
#
# Notes:
# - Expects CSV with columns: Date, <value-column> (e.g., VG09)
# - Handles UTF-8 BOM in header
# - Window grid length is exactly 4800 points (= 4h * 3600 / 3)

suppressPackageStartupMessages({
  library(optparse)
  library(pkgload)
})

# ---------- CLI ----------
opt_list <- list(
  make_option("--csv", type = "character", default = "data/vg09.csv",
              help = "Input CSV path [default: %default]"),
  make_option("--tz", type = "character", default = "UTC",
              help = "Timezone for parsing timestamps [default: %default]"),
  make_option("--valid-frac", type = "double", default = 0.8,
              help = "Min non-NA ratio per day to keep [default: %default]"),
  make_option("--out-dir", type = "character", default = "out",
              help = "Output directory [default: %default]")
)
opt <- parse_args(OptionParser(option_list = opt_list))

dir.create(opt$`out-dir`, showWarnings = FALSE, recursive = TRUE)

# ---------- Helpers ----------
read_input_csv <- function(path, tz = "UTC") {
  stopifnot(file.exists(path))
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(path, stringsAsFactors = FALSE)
  )
  # Normalize BOM in first column name, if present
  names(df)[1] <- sub("\ufeff", "", names(df)[1])
  if (!"Date" %in% names(df)) stop("CSV must have 'Date' column")
  # Pick the first non-Date numeric column as value
  val_cols <- setdiff(names(df), "Date")
  if (length(val_cols) == 0) stop("No value column found")
  value_col <- val_cols[1]
  df$Date <- as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (anyNA(df$Date)) {
    # Retry with flexible parsing if needed
    df$Date <- as.POSIXct(df$Date, tz = tz)
  }
  df <- df[order(df$Date), ]
  list(df = df[, c("Date", value_col)], value_col = value_col)
}

make_window_grid <- function(day, tz = "UTC") {
  # 22:00:00 of 'day' to 02:00:00 of 'day+1' (4 hours)
  start <- as.POSIXct(sprintf("%s 22:00:00", format(day, "%Y-%m-%d")), tz = tz)
  # Use fixed length to guarantee exactly 4800 points at 3s step
  seq(from = start, by = 3, length.out = 4800L)
}

resample_linear <- function(ts, y, grid) {
  # ts, grid: POSIXct; y: numeric
  x <- as.numeric(ts)
  gx <- as.numeric(grid)

  # Deduplicate identical timestamps by averaging
  o <- order(x)
  x <- x[o]; y <- as.numeric(y[o])
  if (any(!is.finite(y))) y[!is.finite(y)] <- NA_real_
  ag <- aggregate(y ~ x, FUN = function(v) mean(v, na.rm = TRUE))
  x <- ag$x; y <- ag$y

  approx(x = x, y = y, xout = gx, method = "linear", rule = 1, ties = "ordered")$y
}

build_window_matrix_3s <- function(csv_path, tz = "UTC", valid_frac = 0.8) {
  info <- read_input_csv(csv_path, tz)
  df <- info$df
  names(df) <- c("Date", "value")

  cat(sprintf("Reading: %s\n", csv_path))
  cat(sprintf("Rows read: %d, from %s to %s\n",
              nrow(df), format(min(df$Date)), format(max(df$Date))))

  # Collect unique days covered by data range
  days_all <- seq.Date(as.Date(min(df$Date, na.rm = TRUE)),
                       as.Date(max(df$Date, na.rm = TRUE)), by = "day")

  # For each day, build 22:00–02:00 grid and interpolate
  Y_list <- vector("list", length(days_all))
  valid <- logical(length(days_all))

  for (i in seq_along(days_all)) {
    g <- make_window_grid(days_all[i], tz)
    # Pull raw samples within [22:00-03:00] buffer to be safe for interpolation
    start_buf <- g[1]
    end_buf   <- g[length(g)] + 3600 # +1h buffer
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

save_artifacts <- function(days, Y, meta, out_dir) {
  write.csv(data.frame(day = days), file.path(out_dir, "realdata-3s-days.csv"), row.names = FALSE)
  write.csv(Y, file.path(out_dir, "realdata-3s-Y.csv"), row.names = FALSE)
  saveRDS(meta, file.path(out_dir, "realdata-3s-meta.rds"))
}

# ---------- Run ----------
set.seed(1)
built <- build_window_matrix_3s(opt$csv, tz = opt$tz, valid_frac = opt$`valid-frac`)

cat(sprintf("Window matrix: %d days x %d points (valid days >=%.0f%%: %d)\n",
            length(built$days), ncol(built$Y), 100 * opt$`valid-frac`, sum(built$valid)))

meta <- list(
  tz = built$tz,
  value_col = built$value_col,
  grid_step_sec = 3L,
  window = c("22:00:00", "02:00:00"),
  points_per_day = ncol(built$Y),
  valid_frac = opt$`valid-frac`
)

save_artifacts(built$days, built$Y, meta, opt$`out-dir`)

cat("\n== Summary ==\n")
cat(sprintf("CSV: %s\n", opt$csv))
cat(sprintf("Time zone: %s\n", built$tz))
cat(sprintf("Grid: 3 seconds, window: 22:00–02:00\n"))
cat(sprintf("Days covered: %s .. %s\n", min(built$days), max(built$days)))
cat(sprintf("Matrix shape: %d days x %d points\n", length(built$days), ncol(built$Y)))
cat(sprintf("Valid days (>=%.0f%% points present): %d\n", 100*opt$`valid-frac`, sum(built$valid)))
cat(sprintf("\nArtifacts saved in '%s':\n - realdata-3s-days.csv\n - realdata-3s-Y.csv\n - realdata-3s-meta.rds\n", opt$`out-dir`))
