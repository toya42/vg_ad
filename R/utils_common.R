library(lubridate)

get_time_window <- function(date, window, tz) {
  start <- as.POSIXct(paste(date, window[1]), tz = tz)
  end <- as.POSIXct(paste(date, window[2]), tz = tz)
  if (end < start) end <- end + days(1)
  list(start = start, end = end)
}

clip_values <- function(x, minv, maxv) {
  x[x < minv] <- minv
  x[x > maxv] <- maxv
  x
}
