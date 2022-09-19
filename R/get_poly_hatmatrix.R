#' @noRd
#' @importFrom stats lm coef
#' @noRd
get_poly_hatmatrix <- function(df_treatment_A1, params_grid, mse, ...) {
  # this function calculates the hat matrix of the treatment model for A1
  # using a polynomial basis expansion.
  params <- unlist(params_grid)
  A <- matrix(1, nrow = NROW(df_treatment_A1))
  for (j in seq_len(length(params))) {
    A <- cbind(A, poly(df_treatment_A1[, j + 1], degree = params[j]))
  }
  poly_hatmatrix <- A %*% chol2inv(chol(t(A) %*% A)) %*% t(A)

  return(list(
    weight = poly_hatmatrix,
    mse = mse
  ))
}
