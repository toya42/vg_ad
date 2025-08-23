compute_metrics <- function(Y_ref, Y_today, scores_ref, scores_today, recon_today, fpca_fit, cfg) {
  metrics_today <- list()
  if (cfg$metrics$fpca_scores$use) {
    if (!is.null(scores_ref) && nrow(scores_ref) > 1 && ncol(scores_today) > 0) {
      mu <- apply(scores_ref[,1:min(ncol(scores_ref), cfg$metrics$fpca_scores$top_k), drop=FALSE], 2, mean)
      sdv <- apply(scores_ref[,1:min(ncol(scores_ref), cfg$metrics$fpca_scores$top_k), drop=FALSE], 2, sd)
      z <- abs((scores_today[1:length(mu)] - mu) / sdv)
      metrics_today$fpca_scores_topk_z <- max(z)
    } else {
      metrics_today$fpca_scores_topk_z <- NA
    }
  }
  if (cfg$metrics$recon_mse$use) {
    metrics_today$recon_mse <- mean((Y_today - recon_today)^2, na.rm = TRUE)
  }
  if (cfg$metrics$level_median$use) {
    metrics_today$level_median <- stats::median(Y_today, na.rm = TRUE)
  }
  if (cfg$metrics$slope_linear$use) {
    metrics_today$slope_linear <- coef(stats::lm(Y_today ~ seq_along(Y_today)))[2]
  }
  if (cfg$metrics$variability_iqr$use) {
    metrics_today$variability_iqr <- stats::IQR(Y_today, na.rm = TRUE)
  }
  if (cfg$metrics$quietness_flag$use) {
    metrics_today$quietness_flag <- as.integer(metrics_today$variability_iqr < 0.01)
  }
  if (cfg$metrics$high_pressure_flag$use) {
    thr <- cfg$metrics$high_pressure_flag$threshold_logpa
    metrics_today$high_pressure_flag <- as.integer(any(Y_today > thr, na.rm = TRUE))
  }

  metrics_ref <- data.frame()
  metrics_to_compute <- c()
  if (cfg$metrics$recon_mse$use) metrics_to_compute <- c(metrics_to_compute, "recon_mse")
  if (cfg$metrics$level_median$use) metrics_to_compute <- c(metrics_to_compute, "level_median")
  if (cfg$metrics$slope_linear$use) metrics_to_compute <- c(metrics_to_compute, "slope_linear")
  if (cfg$metrics$variability_iqr$use) metrics_to_compute <- c(metrics_to_compute, "variability_iqr")
  if (nrow(Y_ref) > 0 && length(metrics_to_compute) > 0) {
    recon_ref <- reconstruct_from_fpca(scores_ref, fpca_fit)
    for (i in seq_len(nrow(Y_ref))) {
      y <- Y_ref[i,]
      rec <- recon_ref[i,]
      row <- list()
      if ("recon_mse" %in% metrics_to_compute) row$recon_mse <- mean((y - rec)^2, na.rm=TRUE)
      if ("level_median" %in% metrics_to_compute) row$level_median <- stats::median(y, na.rm=TRUE)
      if ("slope_linear" %in% metrics_to_compute) row$slope_linear <- coef(stats::lm(y ~ seq_along(y)))[2]
      if ("variability_iqr" %in% metrics_to_compute) row$variability_iqr <- stats::IQR(y, na.rm=TRUE)
      metrics_ref <- rbind(metrics_ref, as.data.frame(row))
    }
  }
  list(today = metrics_today, ref = metrics_ref)
}
