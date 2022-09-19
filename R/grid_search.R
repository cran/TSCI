#' @noRd
grid_search <- function(df,
                        params_list,
                        gcv,
                        nfolds) {
  # this function performs a grid search of all parameter combinations to
  # select the optimal order of the polynomials.
  Y <- df[, 1]
  X <- df[, -1, drop = FALSE]
  n <- NROW(Y)
  p <- NCOL(X)

  params_grid <- expand.grid(params_list)
  MSE_CV <- Inf

  for (i in seq_len(NROW(params_grid))) {
    order_tmp <- unlist(params_grid[i, ])
    if(sum(order_tmp) < n) {
      A <- matrix(1, nrow = n)
      for (j in seq_len(p)) {
        A <- cbind(A, poly(X[, j], degree = order_tmp[j]))
      }
      if (gcv) {
        fit <- lm(Y ~ A - 1)
        mse_cv <- mean((Y - fit$fitted.values)^2) / (1 - NCOL(A) / NROW(A))^2
      } else {
        mse_cv <- cross_validate(Y = Y, X = A, nfolds = nfolds)
      }
      if (mse_cv < MSE_CV) {
        order_opt <- order_tmp
        MSE_CV <- mse_cv
      }
    }
  }
  return(unname(order_opt))
}
