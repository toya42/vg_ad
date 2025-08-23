compute_metrics <- function(Y_ref, Y_today, scores_ref, scores_today, recon_today, fpca_fit, cfg) {
  metrics_today <- list()
  if (!is.null(scores_ref) && nrow(scores_ref) > 1 && ncol(scores_today) > 0) {
    mu <- apply(scores_ref[,1:min(ncol(scores_ref), cfg$metrics$fpca_scores$top_k), drop=FALSE], 2, mean)
    sdv <- apply(scores_ref[,1:min(ncol(scores_ref), cfg$metrics$fpca_scores$top_k), drop=FALSE], 2, sd)
    z <- abs((scores_today[1:length(mu)] - mu) / sdv)
    metrics_today$fpca_scores_topk_z <- max(z)
  } else {
    metrics_today$fpca_scores_topk_z <- NA
  }
  metrics_today$recon_mse <- mean((Y_today - recon_today)^2, na.rm = TRUE)
  metrics_today$level_median <- stats::median(Y_today, na.rm = TRUE)
  metrics_today$slope_linear <- coef(stats::lm(Y_today ~ seq_along(Y_today)))[2]
  metrics_today$variability_iqr <- stats::IQR(Y_today, na.rm = TRUE)
  metrics_today$quietness_flag <- as.integer(metrics_today$variability_iqr < 0.01)
  thr <- cfg$metrics$high_pressure_flag$threshold_logpa
  metrics_today$high_pressure_flag <- as.integer(any(Y_today > thr, na.rm = TRUE))

  metrics_ref <- data.frame()
  if (nrow(Y_ref) > 0) {
    recon_ref <- reconstruct_from_fpca(scores_ref, fpca_fit)
    for (i in seq_len(nrow(Y_ref))) {
      y <- Y_ref[i,]
      rec <- recon_ref[i,]
      metrics_ref <- rbind(metrics_ref, data.frame(
        recon_mse = mean((y - rec)^2, na.rm=TRUE),
        level_median = stats::median(y, na.rm=TRUE),
        slope_linear = coef(stats::lm(y ~ seq_along(y)))[2],
        variability_iqr = stats::IQR(y, na.rm=TRUE)
      ))
    }
  }
  list(today = metrics_today, ref = metrics_ref)
}
