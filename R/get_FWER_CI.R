#' Family-Wise Error Rate controlled Confidence Interval and P-Value for Multi-Splitting.
#'
#' @description The confidence interval is constructed using a search over a range of values.
#'
#' @param Coef an vector of coefficient estimates.
#' @param SE an vector of standard errors of the same size as \code{Coef}.
#' @param level the confidence level required.
#' @param gamma the quantile used to aggregate the p-values and to construct the confidence interval.
#'
#' @return A list with two elements. \code{CI} and \code{p_value}.
#' @noRd
#' @importFrom stats qnorm quantile
get_FWER_CI <- function(Coef, SE, level, gamma = 0.5) {
  # this functions returns the confidence interval and p-value
  # applying the method introduced in introduced in Meinshausen, Meier, Bühlmann (2009).

  alpha <- 1 - level
  beta_min <- min(Coef - qnorm(1 - alpha / 2 * gamma)  * SE)
  beta_max <- max(Coef + qnorm(1 - alpha / 2 * gamma)  * SE)
  beta_range <- seq(beta_min, beta_max, length.out = 10^4)
  p_val_quant <-
    sapply(beta_range,
           FUN = function(beta_test) quantile(p_val(Coef, SE, beta_test), probs = gamma, names = FALSE))
  lower <- beta_range[max(min(which(p_val_quant > alpha * gamma)) - 1, 1)]
  upper <- beta_range[min(max(which(p_val_quant > alpha * gamma)) + 1,10^4)]
  p_value <- min(1, quantile(p_val(Coef, SE, 0) / gamma, probs = gamma, names = FALSE))
  return(list(CI = c("lower" = lower, "upper" = upper), p_value = p_value))
}
