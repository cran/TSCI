#' @title Statistics Calculation for Violation Space Selection
#' @description Calculate the statistics needed for violation space selection.
#'
#' @param D_rep transformed treatment of dimension n_A1 by 1.
#' @param Cov_rep transformed augmented covariates (vio_space, X) of dimension
#' n_A1 by (p+ncol(vio_space)) corresponding to a violation space candidate.
#' @param weight n_A1 by n_A1 weight matrix.
#' @param eps_hat residuals in the outcome model.
#' @param delta_hat residuals of the treatment model corresponding to samples in A1.
#' @param sd_boot logical. if \code{TRUE}, it determines the standard error using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
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
tsci_selection_stats <- function(D_rep, Cov_rep, weight, eps_hat, delta_hat, sd_boot, iv_threshold, threshold_boot, B) {
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
  # bootstrap for the standard error and iv threshold if sd_boot is TRUE and threshold_boot is TRUE
  if (sd_boot | threshold_boot) {
    delta_cent <- as.vector(delta_hat - mean(delta_hat))
    u_matrix <- matrix(rnorm(n_A1 * B), ncol = B)
    delta_boo_matrix <- u_matrix * delta_cent
    if (sd_boot) {
      eps_cent <- as.vector(eps_hat - mean(eps_hat))
      eps_boo_matrix <- u_matrix * eps_cent
      #delta_resid <- resid(lm(weight %*% delta_boo_matrix ~ Cov_rep - 1))
      eps_resid <- resid(lm(weight %*% eps_boo_matrix ~ Cov_rep - 1))
      bias_term1 <- t(D_resid) %*% eps_resid
      #bias_Err1 <- colSums(eps_resid * delta_resid) - colSums(eps_boo_matrix * delta_boo_matrix * diag_M)
      bias_Err1 <- - colSums(eps_boo_matrix * delta_boo_matrix * diag_M)
      N_vec <- (bias_term1 + bias_Err1) / D_RSS
      sd <- sd(N_vec)
    } else {
      # standard error of the treatment effect estimate (14)
      sd <- sqrt(sum(eps_hat^2 * (weight %*% D_resid)^2)) / D_RSS
    }
    if (threshold_boot) {
      D_rep2 <- weight %*% D_rep
      delta_rep_matrix <- weight %*% delta_boo_matrix
      delta_resid_matrix <- resid(lm(delta_rep_matrix ~ Cov_rep - 1))
      boot_vec <- apply(delta_resid_matrix, 2, FUN = function(delta_resid) sum(delta_resid^2) + 2 * sum(D_rep2 * delta_resid))
      iv_thol <- quantile(boot_vec, 0.975) /
        SigmaSqD + max(2 * trace_M, iv_threshold)
    } else {
      iv_thol <- max(2 * trace_M, iv_threshold)
    }
  } else {
    sd <- sqrt(sum(eps_hat^2 * (weight %*% D_resid)^2)) / D_RSS
    iv_thol <- max(2 * trace_M, iv_threshold)
  }
  # When iv strength <= 100, the bias components we cannot estimate can have some effects
  sd <- ifelse(iv_str <= 100, 1.1 * sd, sd)
  # in practice, if iv strength >= 40, TSCI works well
  iv_thol <- min(iv_thol, 40)
  returnList <- list(
    sd = sd,
    D_resid = D_resid,
    iv_str = iv_str,
    iv_thol = iv_thol,
    diag_M = diag_M
  )
  returnList
}
