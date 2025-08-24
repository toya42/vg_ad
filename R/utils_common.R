library(lubridate)

#' Compute time window bounds
#'
#' @description Given a date and start/end times, compute POSIXct bounds.
#' @param date Date. The reference date.
#' @param window Character vector of length 2 giving start and end times (HH:MM).
#' @param tz Character. Timezone.
#' @return List with `start` and `end` POSIXct values.
#' @keywords internal
get_time_window <- function(date, window, tz) {
  start <- as.POSIXct(paste(date, window[1]), tz = tz)
  end <- as.POSIXct(paste(date, window[2]), tz = tz)
  if (end < start) end <- end + days(1)
  list(start = start, end = end)
}

#' Clip numeric values
#'
#' @description Clamp values to a specified range.
#' @param x Numeric vector.
#' @param minv Numeric. Minimum allowed value.
#' @param maxv Numeric. Maximum allowed value.
#' @return Numeric vector.
#' @keywords internal
clip_values <- function(x, minv, maxv) {
  x[x < minv] <- minv
  x[x > maxv] <- maxv
  x
}
