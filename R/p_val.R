#' @noRd
#' @importFrom stats pnorm
p_val <- function(Coef, SE, beta_test) {
  # this functions calculates the two-sided p-values.
  diff_norm <- abs(Coef - beta_test) / SE
  p_val <- 2 * pnorm(diff_norm, lower.tail = FALSE)
  return(p_val)
}
