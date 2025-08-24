decide_today <- function(metrics_today, thresholds, cfg, context_info=list()) {
  reasons <- c()
  level <- "ok"
  prefix <- if (!is.null(context_info$window)) paste0(context_info$window, ": ") else ""
  for (nm in names(metrics_today)) {
    val <- metrics_today[[nm]]
    if (is.null(val) || is.na(val)) next
    thr <- thresholds[[nm]]
    if (grepl("flag", nm)) {
      if (val > 0) {
        if (level != "alert") level <- "attention"
        reasons <- c(reasons, sprintf("%s%s=%g", prefix, nm, val))
      }
    } else if (!is.null(thr)) {
      if (!is.null(thr$alert) && !is.na(thr$alert) && val >= thr$alert) {
        level <- "alert"
        reasons <- c(reasons, sprintf("%s%s=%g >= alert(%g)", prefix, nm, val, thr$alert))
      } else if (!is.null(thr$attention) && !is.na(thr$attention) && val >= thr$attention && level != "alert") {
        level <- "attention"
        reasons <- c(reasons, sprintf("%s%s=%g >= attention(%g)", prefix, nm, val, thr$attention))
      }
    }
  }
  list(level = level, reasons = reasons)
}
