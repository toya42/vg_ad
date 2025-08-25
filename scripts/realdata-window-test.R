#!/usr/bin/env Rscript

# realdata-window-test.R
# Read real CSV, bin to 15-minute intervals, and build 22:00–02:00 daily windows.
# - Robust CSV reader (handles UTF-8 BOM and explicit format)
# - Constructs a fixed-length grid per day across midnight (22:00 -> 02:00 next day)
# - Saves window matrix and metadata to out/ for inspection
# - Comments are in English per project convention

suppressPackageStartupMessages({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    install.packages("pkgload", repos = "https://cloud.r-project.org")
  }
  # Load package under development if not installed
  if (!requireNamespace("vgAd", quietly = TRUE)) {
    pkgload::load_all(".")
    message("Loaded vgAd with pkgload::load_all()")
  } else {
    library(vgAd)
    message("Loaded vgAd from installed library")
  }
  library(lubridate)
})

# ----------------------------
# Parameters
# ----------------------------
csv_path <- "data/vg09.csv"  # repository-relative path
tz       <- "UTC"
bin_min  <- 15               # minutes per bin
start_h  <- 22               # window start hour (22:00)
end_h    <- 2                # window end hour (02:00 next day)
out_dir  <- "out"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# Helpers
# ----------------------------

# Read CSV robustly (handles UTF-8 BOM and header variants) and parse UTC datetimes
read_input_csv <- function(path, tz) {
  stopifnot(file.exists(path))
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(path, stringsAsFactors = FALSE)
  )
  # Strip BOM from the first header if present
  names(df)[1] <- sub("^\ufeff", "", names(df)[1])

  # Determine date/value columns (expected: "Date", "VG09")
  date_col  <- if ("Date" %in% names(df)) "Date" else names(df)[1]
  value_col <- if ("VG09" %in% names(df)) "VG09" else setdiff(names(df), date_col)[1]

  # Parse with explicit format in the given tz
  df[[date_col]] <- as.POSIXct(df[[date_col]], format = "%Y-%m-%d %H:%M:%S", tz = tz)

  # Keep only parsed rows and standardize column names
  df <- df[!is.na(df[[date_col]]), c(date_col, value_col), drop = FALSE]
  names(df) <- c("datetime", "value")
  df <- df[order(df$datetime), , drop = FALSE]
  df
}

# Align to bin boundaries (floor) and average within bins
bin_series <- function(df, bin_min, tz) {
  df$datetime <- with_tz(df$datetime, tz = tz)
  df$bin <- floor_date(df$datetime, unit = paste0(bin_min, " minutes"))
  agg <- stats::aggregate(value ~ bin, data = df, FUN = mean, na.rm = TRUE)
  out <- data.frame(datetime = agg$bin, value = agg$value)
  out <- out[order(out$datetime), , drop = FALSE]
  out
}

# Build daily window matrix (rows = days, cols = fixed time bins) across midnight
build_window_matrix <- function(df_bin, start_h = 22, end_h = 2, tz, bin_min) {
  if (nrow(df_bin) == 0) {
    stop("No rows in df_bin; parsing or binning likely failed.")
  }
  # Safe min/max timestamps
  min_ts <- suppressWarnings(min(df_bin$datetime, na.rm = TRUE))
  max_ts <- suppressWarnings(max(df_bin$datetime, na.rm = TRUE))
  if (!is.finite(as.numeric(min_ts)) || !is.finite(as.numeric(max_ts))) {
    stop("No valid datetimes found (min/max not finite). Check CSV parsing of 'Date'.")
  }

  days <- seq.Date(as.Date(min_ts), as.Date(max_ts), by = "day")

  # Number of bins in the window (handles midnight crossing)
  win_hours <- (end_h - start_h) %% 24
  if (win_hours == 0) win_hours <- 24
  n_bins <- as.integer((win_hours * 60) / bin_min)
  if (n_bins <= 0) stop("Computed n_bins <= 0; check start_h/end_h/bin_min.")

  # Build result containers
  Y <- matrix(NA_real_, nrow = length(days), ncol = n_bins)
  t_grid <- seq(0, by = bin_min / 60, length.out = n_bins)  # hours from window start

  for (i in seq_along(days)) {
    d  <- days[i]
    # Start at HH:00 of day d
    t0 <- ymd_hms(paste(d, sprintf("%02d:00:00", start_h)), tz = tz)
    # End time by adding the window length in hours
    t1 <- t0 + hours(win_hours)
    # Fixed-length grid of bin timestamps
    grid <- seq(from = t0, by = paste(bin_min, "mins"), length.out = n_bins)

    # Slice the binned series covering [t0, t1)
    sli <- df_bin[df_bin$datetime >= t0 & df_bin$datetime < t1, ]
    if (nrow(sli)) {
      m <- match(grid, sli$datetime)
      Y[i, ] <- sli$value[m]
    }
  }
  list(Y = Y, t_grid = t_grid, days = days)
}

# ----------------------------
# Main
# ----------------------------
message(sprintf("Reading: %s", csv_path))
df_raw <- read_input_csv(csv_path, tz = tz)
message(sprintf("Rows read: %d, from %s to %s",
                nrow(df_raw),
                format(min(df_raw$datetime), "%Y-%m-%d %H:%M:%S %Z"),
                format(max(df_raw$datetime), "%Y-%m-%d %H:%M:%S %Z")))

# 15-minute binning
df_bin <- bin_series(df_raw, bin_min = bin_min, tz = tz)
message(sprintf("Binned rows: %d (every %d minutes)", nrow(df_bin), bin_min))

# Build 22:00–02:00 windows
win <- build_window_matrix(df_bin, start_h = start_h, end_h = end_h, tz = tz, bin_min = bin_min)
Y <- win$Y
days <- win$days
t_grid <- win$t_grid

# Basic QA: count non-NA per row and filter "valid" days
non_na_per_row <- rowSums(!is.na(Y))
valid_rows <- non_na_per_row >= ceiling(ncol(Y) * 0.8)  # keep days with >=80% bins present
num_valid <- sum(valid_rows)
message(sprintf("Window matrix: %d days x %d bins (valid days >=80%% bins: %d)",
                nrow(Y), ncol(Y), num_valid))

# Save artifacts for inspection
write.csv(
  data.frame(day = as.character(days),
             non_na_bins = non_na_per_row,
             valid = valid_rows),
  file = file.path(out_dir, "realdata-window-days.csv"),
  row.names = FALSE
)
Y_df <- as.data.frame(Y)
colnames(Y_df) <- paste0("bin_", sprintf("%02d", seq_len(ncol(Y_df)) - 1))
Y_df <- cbind(day = as.character(days), Y_df)
write.csv(Y_df, file = file.path(out_dir, "realdata-window-Y.csv"), row.names = FALSE)

meta <- list(
  tz = tz,
  bin_min = bin_min,
  start_h = start_h,
  end_h = end_h,
  n_days = nrow(Y),
  n_bins = ncol(Y),
  valid_days = num_valid,
  first_day = as.character(min(days)),
  last_day  = as.character(max(days))
)
saveRDS(meta, file = file.path(out_dir, "realdata-window-meta.rds"))

cat("\n== Summary ==\n")
cat(sprintf("CSV: %s\n", csv_path))
cat(sprintf("Time zone: %s\n", tz))
cat(sprintf("Bin: %d minutes, window: %02d:00–%02d:00 (across midnight)\n", bin_min, start_h, end_h))
cat(sprintf("Days covered: %s .. %s\n", as.character(min(days)), as.character(max(days))))
cat(sprintf("Matrix shape: %d days x %d bins\n", nrow(Y), ncol(Y)))
cat(sprintf("Valid days (>=80%% bins present): %d\n", num_valid))
cat(sprintf("\nArtifacts saved in '%s':\n", out_dir))
cat(" - realdata-window-days.csv\n")
cat(" - realdata-window-Y.csv\n")
cat(" - realdata-window-meta.rds\n")
