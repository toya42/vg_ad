# Threshold estimation for each metric based on reference distributions.
# Note: z-score thresholds for FPCA scores are provided separately via
# cfg$decision$attention_sigma and cfg$decision$alert_sigma.
#' Fit metric thresholds
#'
#' @description Estimate thresholds for metrics based on reference data.
#' @param metric_values_ref Data frame of metric values from reference days.
#' @param cfg List. Parsed config.
#' @return Named list of threshold lists for each metric.
#' @export
#' @examples
#' \dontrun{fit_thresholds(metrics_ref, cfg)}
fit_thresholds <- function(metric_values_ref, cfg) {
  res <- list()
  if (nrow(metric_values_ref) == 0) return(res)
  scheme <- cfg$thresholds$scheme
  min_samples <- cfg$thresholds$min_samples
  fallback <- cfg$thresholds$fallback_scheme
  q_p <- cfg$thresholds$quantile_p
  for (nm in names(metric_values_ref)) {
    x <- metric_values_ref[[nm]]
    x <- x[!is.na(x)]
    if (length(x) == 0) next
    if (length(x) < min_samples && fallback == "quantile") {
      qv <- stats::quantile(x, q_p, na.rm = TRUE)
      res[[nm]] <- list(attention = qv, alert = qv, mean = NA, sd = NA, scheme = "quantile")
    } else if (scheme == "gaussian_3sigma") {
      mu <- mean(x)
      sdv <- stats::sd(x)
      att <- mu + cfg$decision$attention_sigma * sdv
      alt <- mu + cfg$decision$alert_sigma * sdv
      res[[nm]] <- list(attention = att, alert = alt, mean = mu, sd = sdv, scheme = "gaussian")
    } else if (scheme == "quantile") {
      qv <- stats::quantile(x, q_p, na.rm = TRUE)
      res[[nm]] <- list(attention = qv, alert = qv, mean = NA, sd = NA, scheme = "quantile")
    } else {
      mu <- mean(x)
      sdv <- stats::sd(x)
      res[[nm]] <- list(attention = mu + 3 * sdv, alert = mu + 3 * sdv, mean = mu, sd = sdv, scheme = "gaussian")
    }
  }
  res
}
