#!/usr/bin/env Rscript
# Memory instrumentation for the 3s-window pipeline (22:00–02:00)
# - Uses object.size and lobstr::obj_size for key objects
# - Reports process RSS/HWM from /proc/self/status
# - Uses peakRAM only for measurement (no assignment), then runs the real build with assignment + RSS delta

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
  library(lubridate)
})

# ---------- helpers ----------
size <- function(name, x) {
  cat(sprintf("%-22s object.size=%8s | ",
              paste0(name, ":"), format(object.size(x), units="auto")))
  if (requireNamespace("lobstr", quietly=TRUE)) {
    cat(sprintf("lobstr::obj_size=%8s | ", format(lobstr::obj_size(x), units="auto")))
  } else {
    cat("(lobstr not installed) | ")
  }
  cat(sprintf("class=%s\n", paste(class(x), collapse=", ")))
}

proc_mem <- function() {
  st <- readLines("/proc/self/status")
  pick <- function(key) {
    ln <- st[startsWith(st, key)]
    if (length(ln)) as.numeric(sub(".*:\\s*([0-9]+)\\s*kB.*", "\\1", ln))/1024 else NA
  }
  data.frame(VmRSS_MiB = pick("VmRSS"), VmHWM_MiB = pick("VmHWM"))
}

# Simple RSS-delta measurement that evaluates in the caller and preserves assignment
rss_delta_eval <- function(expr, label="expr") {
  gc()
  before <- proc_mem()
  eval.parent(substitute(expr))  # evaluate in caller so assignments stick
  after <- proc_mem()
  cat(sprintf("[%s] RSS: %.1f -> %.1f MiB\n", label, before$VmRSS_MiB, after$VmRSS_MiB))
}

# ---------- CLI ----------
opt <- parse_args(OptionParser(option_list = list(
  make_option("--csv", type="character", default="data/vg09.csv", help="CSV path"),
  make_option("--tz", type="character", default="UTC", help="timezone"),
  make_option("--valid-frac", type="double", default=0.8, help="min non-NA fraction per day"),
  make_option("--today", type="character", help="today date (YYYY-MM-DD)"),
  make_option("--ref-days", type="integer", default=5, help="#ref days before today")
)))

# ---------- read CSV ----------
read_input_csv <- function(path, tz) {
  stopifnot(file.exists(path))
  dt <- fread(path, encoding = "UTF-8")
  nms <- names(dt); nms[1] <- sub("\ufeff","", nms[1]); setnames(dt, nms)
  if (!("Date" %in% names(dt))) stop("CSV must have a 'Date' column")
  val_col <- setdiff(names(dt), "Date")
  if (length(val_col) != 1) stop("CSV must have exactly one value column")

  dt[, Date := as.POSIXct(Date, tz = tz,
                          tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M:%S"))]
  suppressWarnings(dt[, (val_col) := lapply(.SD, as.numeric), .SDcols = val_col])

  rng <- range(dt$Date, na.rm = TRUE)
  cat(sprintf("Reading: %s\nRows read: %d, from %s to %s\n",
              path, nrow(dt), format(rng[1], usetz = TRUE), format(rng[2], usetz = TRUE)))
  list(dt = dt, value_col = val_col)
}

# Build 3s grid matrix for 22:00–02:00
build_window_matrix_3s <- function(dt, value_col, tz, valid_frac) {
  dt[, day := as.Date(Date, tz = tz)]
  days <- sort(unique(dt$day))
  n_per_day <- 4 * 3600 / 3  # 4800 points in 4 hours at 3-second grid

  Y_list <- vector("list", length(days))
  keep <- logical(length(days))

  for (i in seq_along(days)) {
    d0 <- days[i]
    start_dt <- as.POSIXct(paste0(d0, " 22:00:00"), tz = tz)
    end_dt   <- as.POSIXct(paste0(d0 + 1, " 02:00:00"), tz = tz)

    grid <- seq(from = start_dt, to = end_dt, by = 3)
    # ensure exactly 4800 points (drop the last endpoint if inclusive)
    if (length(grid) >= (n_per_day + 1)) grid <- grid[1:n_per_day]

    sub <- dt[Date >= start_dt & Date <= end_dt, .(Date, val = get(value_col))]
    y <- rep(NA_real_, length(grid))
    good <- !is.na(sub$val)
    if (nrow(sub) >= 2 && any(good)) {
      xp <- as.numeric(sub$Date); yp <- as.numeric(sub$val)
      ok <- is.finite(xp) & is.finite(yp); xp <- xp[ok]; yp <- yp[ok]
      if (length(xp) >= 2) {
        y <- approx(x = xp, y = yp, xout = as.numeric(grid), method = "linear",
                    ties = mean, rule = 1)$y
      }
    }
    frac <- mean(is.finite(y))
    keep[i] <- is.finite(frac) && (frac >= valid_frac)
    Y_list[[i]] <- y
  }

  Y <- do.call(rbind, Y_list)
  rownames(Y) <- as.character(days)
  list(Y = Y[keep, , drop = FALSE], days = days[keep])
}

# ---------- run ----------
rd <- read_input_csv(opt$csv, opt$tz)
size("dt", rd$dt)
cat("Process mem (MiB) at start:\n"); print(proc_mem()); cat("\n")

# 1) PeakRAM measurement ONLY (no assignment) to estimate peak usage of the build
if (requireNamespace("peakRAM", quietly = TRUE)) {
  gc()
  pr <- peakRAM::peakRAM(
    build_window_matrix_3s(rd$dt, rd$value_col, tz = opt$tz, valid_frac = opt$valid_frac),
    print = FALSE
  )
  cat(sprintf("[build_window_matrix_3s] PeakRAM (no-assign): %.1f MiB\n", pr$Peak_RAM_Used_MiB))
  rm(pr); gc()
} else {
  cat("peakRAM not installed; skipping peak measurement.\n")
}

# 2) Real build WITH assignment and RSS delta
rss_delta_eval(
  win <- build_window_matrix_3s(rd$dt, rd$value_col, tz = opt$tz, valid_frac = opt$valid_frac),
  label = "build_window_matrix_3s (assigned)"
)

size("Y (all valid days)", win$Y); size("days", win$days)
cat("Process mem (MiB) after Y build:\n"); print(proc_mem()); cat("\n")

# Slice ref/today (this part keeps assignments in caller)
stopifnot(!is.null(opt$today))
today <- as.Date(opt$today, tz = opt$tz)
if (!(today %in% win$days)) stop("today not present in valid days")
tidx <- which(win$days == today)
ref_end <- tidx - 1; ref_start <- ref_end - (opt$`ref-days`) + 1
if (ref_start < 1) stop("not enough reference days before today")
ref_idx <- ref_start:ref_end

Y_ref  <- win$Y[ref_idx, , drop = FALSE]
y_today <- win$Y[tidx, , drop = TRUE]
size("Y_ref", Y_ref); size("y_today", y_today)
cat("Process mem (MiB) after slicing:\n"); print(proc_mem()); cat("\n")
cat(sprintf("Dims: ref %d x %d, today %d\n\n", nrow(Y_ref), ncol(Y_ref), length(y_today)))

cat("Done.\n")
