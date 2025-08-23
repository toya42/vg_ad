decide_today <- function(metrics_today, thresholds, cfg, context_info=list()) {
  attention_sigma <- cfg$decision$attention_sigma
  alert_sigma <- cfg$decision$alert_sigma
  reasons <- c()
  level <- "ok"
  for (nm in names(metrics_today)) {
    val <- metrics_today[[nm]]
    thr <- thresholds[[nm]]
    if (!is.null(thr) && !is.na(val) && thr$sd > 0) {
      z <- (val - thr$mean) / thr$sd
      if (z >= alert_sigma) {
        level <- "alert"
        reasons <- c(reasons, paste0(nm, ": z=", round(z,2)))
      } else if (z >= attention_sigma && level != "alert") {
        level <- "attention"
        reasons <- c(reasons, paste0(nm, ": z=", round(z,2)))
      }
    } else if (grepl("flag", nm) && val > 0) {
      level <- "attention"
      reasons <- c(reasons, nm)
    }
  }
  list(level = level, reasons = reasons)
}
