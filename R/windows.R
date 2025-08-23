build_reference_days <- function(target_date, cfg) {
  root <- cfg$data_source$csv$root
  windows <- list()
  for (w in cfg$reference_windows) {
    if (w$type == "rolling") {
      dates <- seq(target_date - w$lookback_days, target_date - 1, by = "day")
    } else if (w$type == "relative_range") {
      dates <- seq(target_date - w$start_days_ago, target_date - w$end_days_ago, by = "day")
    } else {
      next
    }
    existing <- dates[file.exists(file.path(root, paste0(format(dates, "%Y%m%d"), ".csv")))]
    windows[[w$name]] <- existing
  }
  windows
}
