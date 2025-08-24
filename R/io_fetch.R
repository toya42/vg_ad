#' Fetch 1-day sensor data
#'
#' @description Retrieve raw measurements for a given sensor and date.
#' @param sensor_id Character. Sensor identifier (e.g., "VG09").
#' @param date Date. Target local date.
#' @param cfg List. Parsed config.
#' @return A tibble with columns `datetime` (POSIXct, local tz) and `value` (numeric).
#' @export
#' @examples
#' \dontrun{fetch_day("VG09", as.Date("2025-08-10"), cfg)}
fetch_day <- function(sensor_id, date, cfg) {
  stopifnot(requireNamespace("readr", quietly = TRUE))
  tz <- cfg$runtime$timezone
  file <- file.path(cfg$data_source$csv$root, paste0(format(date, "%Y%m%d"), ".csv"))
  if (!file.exists(file)) {
    warning("file not found: ", file)
    return(NULL)
  }
  df <- tryCatch(readr::read_csv(file, show_col_types = FALSE), error = function(e) {
    warning("failed to read ", file)
    return(NULL)
  })
  if (!sensor_id %in% names(df)) {
    warning("sensor column not found")
    return(NULL)
  }
  tibble::tibble(datetime = lubridate::ymd_hms(df$Date, tz = tz), value = df[[sensor_id]])
}
