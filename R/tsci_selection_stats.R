#' @title Statistics Calculation for Violation Space Selection
#' @description Calculate the statistics needed for violation space selection.
#'
#' @param D_rep transformed treatment of dimension n_A1 by 1.
#' @param Cov_rep transformed augmented covariates (vio_space, X) of dimension
#' n_A1 by (p+ncol(vio_space)) corresponding to a violation space candidate.
#' @param weight n_A1 by n_A1 weight matrix.
#' @param eps_hat residuals in the outcome model.
#' @param delta_hat residuals of the treatment model corresponding to samples in A1.
#' @param iv_threshold the minimal value of the threshold of IV strength test.
#' @param threshold_boot logical. if \code{TRUE}, it determines the threshold of the IV strength using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param B number of bootstrap samples.
#'
#' @return:
#'     \item{\code{sd}}{estimated standard error for the treatment effect estimator.}
#'     \item{\code{D_resid}}{residuals for D_rep ~ Cov_rep.}
#'     \item{\code{iv_str}}{estimated IV strength.}
#'     \item{\code{iv_thol}}{threshold for IV strength test.}
#'     \item{\code{diag_M}}{diagonal of matrix M_{RF}(V) corresponding to a violation space candidate.}
#' @noRd
#'
#' @importFrom stats resid lm rnorm quantile
#'
tsci_selection_stats <- function(D_rep, Cov_rep, weight, eps_hat, delta_hat, iv_threshold, threshold_boot, B) {
  # this function returns the standard error of the trace of the treatment effect estimate (14),
  # D_resid used for the violation space selection (20, 23),
  # the estimated iv strength (17), the iv strength threshold (18)
  # and the trace of M (11).
  # For better understanding what certain parts in the codes do (x) will refer to the
  # corresponding equations in Guo and Bühlmann (2022).
  n_A1 <- length(D_rep)
  # computes the trace of M_{RF}(V)
  # the trace of M_{RF}(V) matrix can be computed as a sum of squared residuals
  # obtained from regressing each column of the weight matrix on Cov.rep.
  SigmaSqD <- mean(delta_hat^2)
  diag_M <- colSums(resid(lm(weight ~ Cov_rep - 1))^2)
  trace_M <- sum(diag_M)
  # estimates iv strength (17).
  D_resid <- resid(lm(D_rep ~ Cov_rep - 1))
  D_RSS <- sum(D_resid^2)
  iv_str <- D_RSS / SigmaSqD
  # standard error of the treatment effect estimate (14).
  sd <- sqrt(sum(eps_hat^2 * (weight %*% D_resid)^2)) / D_RSS
  # bootstrap for the threshold of IV strength test (bellow 17) if threshold_boot is TRUE.
  if (threshold_boot) {
    D_rep2 <- weight %*% D_rep
    delta_cent <- as.vector(delta_hat - mean(delta_hat))
    delta_rep_matrix <- weight %*% (delta_cent * matrix(rnorm(n_A1 * B), ncol = B))
    delta_resid_matrix <- resid(lm(delta_rep_matrix ~ Cov_rep - 1))
    boot_vec <- apply(delta_resid_matrix, 2, FUN = function(delta_resid) sum(delta_resid^2) + 2 * sum(D_rep2 * delta_resid))
    iv_thol <- quantile(boot_vec, 0.975) /
      SigmaSqD + max(2 * trace_M, iv_threshold)
  } else {
    iv_thol <- max(2 * trace_M, iv_threshold)
  }
  returnList <- list(
    sd = sd,
    D_resid = D_resid,
    iv_str = iv_str,
    iv_thol = iv_thol,
    diag_M = diag_M
  )
  returnList
}
