#' Build reference day windows
#'
#' @description Generate lists of past dates to use as references for a target day.
#' @param target_date Date. Day for which references are needed.
#' @param cfg List. Parsed config.
#' @return Named list of date vectors for each window.
#' @export
#' @examples
#' \dontrun{build_reference_days(as.Date("2025-08-10"), cfg)}
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
