#' Fit FPCA model
#'
#' @description Perform functional principal components analysis on reference matrix.
#' @param Y_mat Matrix. Rows are days, columns are time points.
#' @param t_grid POSIXct vector for time grid.
#' @param cfg List. Parsed config.
#' @return List containing model object, number of PCs used, and basis info.
#' @export
#' @examples
#' \dontrun{fit_fpca(Y_mat, t_grid, cfg)}
fit_fpca <- function(Y_mat, t_grid, cfg) {
  if (nrow(Y_mat) < 2) {
    warning("not enough reference days for FPCA")
    return(NULL)
  }
  if (!requireNamespace("refund", quietly = TRUE)) {
    warning("refund package missing; using prcomp fallback")
    pc <- stats::prcomp(Y_mat, center = TRUE, scale. = FALSE)
    vars <- pc$sdev^2
    cum <- cumsum(vars) / sum(vars)
    K <- max(cfg$model$fpca$min_pc, which(cum >= cfg$model$fpca$cumvar_min)[1])
    list(model = pc, pcnum = K, cumvar_used = cum[K], center = pc$center, phi = pc$rotation[,1:K], t_grid = t_grid)
  } else {
    fit <- refund::fpca.sc(Y_mat, pve = cfg$model$fpca$cumvar_min)
    K_pve <- ncol(fit$efunctions)
    if (is.null(K_pve) || K_pve == 0) {
      warning("fpca.sc returned no components")
      return(NULL)
    }
    K <- K_pve
    cumvar_used <- sum(fit$varprop[1:K])
    list(model = fit, pcnum = K, cumvar_used = cumvar_used, center = fit$mu, phi = fit$efunctions[,1:K], t_grid = t_grid)
  }
}

#' Project data into FPCA scores
#'
#' @description Project new curves into fitted FPCA basis.
#' @param Y_mat_new Matrix with rows as new observations.
#' @param fpca_fit List returned by [fit_fpca()].
#' @return Matrix of FPCA scores.
#' @export
#' @examples
#' \dontrun{project_fpca_scores(Y_new, fpca_fit)}
project_fpca_scores <- function(Y_mat_new, fpca_fit) {
  if (is.null(fpca_fit)) return(matrix(NA, nrow = nrow(Y_mat_new), ncol = 0))
  phi <- fpca_fit$phi
  center <- fpca_fit$center
  scale(Y_mat_new, center = center, scale = FALSE) %*% phi
}

#' Reconstruct curves from FPCA scores
#'
#' @description Rebuild approximated curves from FPCA scores.
#' @param scores Matrix of FPCA scores.
#' @param fpca_fit List returned by [fit_fpca()].
#' @return Matrix of reconstructed curves.
#' @export
#' @examples
#' \dontrun{reconstruct_from_fpca(scores, fpca_fit)}
reconstruct_from_fpca <- function(scores, fpca_fit) {
  if (is.null(fpca_fit)) return(matrix(NA, nrow = nrow(scores), ncol = 0))
  center <- fpca_fit$center
  phi <- fpca_fit$phi
  scores %*% t(phi) + matrix(center, nrow = nrow(scores), ncol = length(center), byrow = TRUE)
}
