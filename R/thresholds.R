fit_thresholds <- function(metric_values_ref, cfg) {
  res <- list()
  if (nrow(metric_values_ref) < 2) return(res)
  for (nm in names(metric_values_ref)) {
    x <- metric_values_ref[[nm]]
    mu <- mean(x, na.rm = TRUE)
    sdv <- stats::sd(x, na.rm = TRUE)
    res[[nm]] <- list(mean = mu, sd = sdv)
  }
  res
}
