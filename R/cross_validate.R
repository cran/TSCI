#' @noRd
cross_validate <- function(Y, X, nfolds) {
  # this functions performs k-fold cross-validation.
  n <- NROW(X)
  Resample <- sample(n)
  sub_obs <- floor(n / nfolds)

  mse_tmp <- rep(NA, nfolds)
  MSE_CV <- Inf

  for (k in seq_len(nfolds)) {
    if (k == nfolds) {
      sub_test_index <- seq((k - 1) * sub_obs + 1, n)
    } else {
      sub_test_index <- seq((k - 1) * sub_obs + 1, k * sub_obs)
    }
    sub_test_index <- Resample[sub_test_index]
    fit <- lm.fit(x = as.matrix(X[-sub_test_index, ]), y = Y[-sub_test_index])
    mse_tmp[k] <- mean((as.matrix(X[sub_test_index, ]) %*% coef(fit) - Y[sub_test_index])^2)
  }

  return(mean(mse_tmp))
}
