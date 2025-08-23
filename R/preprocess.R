preprocess_day <- function(df_day, cfg) {
  stopifnot(!is.null(df_day))
  eps <- as.numeric(cfg$preprocess$epsilon)
  log_min <- cfg$preprocess$clip_log$min
  log_max <- cfg$preprocess$clip_log$max
  tz <- cfg$runtime$timezone
  win <- get_time_window(as.Date(df_day$datetime[1]), cfg$preprocess$window_utc, tz)
  df <- dplyr::filter(df_day, datetime >= win$start, datetime <= win$end)
  if (nrow(df) == 0) {
    warning("no data in window")
    return(NULL)
  }
  val_log <- log10(df$value + eps)
  val_log <- clip_values(val_log, log_min, log_max)
  t_seq <- seq(win$start, win$end, by = cfg$preprocess$resample$dt_sec)
  interp <- approx(x = as.numeric(df$datetime), y = val_log, xout = as.numeric(t_seq), method = cfg$preprocess$resample$method, rule = 1)
  vec <- interp$y
  meta <- list(missing_ratio = mean(is.na(vec)), n_points = length(vec))
  list(vec = vec, meta = meta, t_grid = t_seq)
}
