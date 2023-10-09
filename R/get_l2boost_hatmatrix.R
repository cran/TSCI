#' @noRd
#' @importFrom xgboost xgb.DMatrix xgb.train xgb.cv
#' @importFrom stats predict
#' @importFrom Rfast mat.mult
get_l2boost_hatmatrix <- function(df_treatment_A1,
                                  df_treatment_A2,
                                  params_grid,
                                  ...) {
  # this function applies cross validation to choose the best fitting parameter combination using A2
  # and calculates the hat matrix of the treatment model for A1 using boosting.
  n_A1 <- NROW(df_treatment_A1)
  n_A2 <- NROW(df_treatment_A2)

  xgbD_A2 <- xgb.DMatrix(as.matrix(df_treatment_A2[, -1]),
                         label = df_treatment_A2[, 1],
                         nthread = params_grid$nthread[1])

  MSE_CV_A2 <- Inf
  params_A2 <- NULL


  for (i in seq_len(NROW(params_grid))) {
    temp_A2 <- xgb.cv(
      params = list(
        eta = params_grid$eta[i],
        max_depth = params_grid$max_depth[i],
        subsample = params_grid$subsample[i],
        colsample_bytree = params_grid$colsample_bytree[i],
        lambda = params_grid$lambda[i],
        nthread = params_grid$nthread[i]
      ),
      metrics = "rmse",
      nrounds = params_grid$nrounds[i],
      nfold = 5,
      data = xgbD_A2,
      verbose = FALSE
    )
    mse_cv <- temp_A2$evaluation_log$test_rmse_mean^2

    if (params_grid$early_stopping[i] == TRUE) {
      # Determines the boosting iteration with the smallest mse and chooses the
      # smallest number of iterations with an mse not much bigger instead to save
      # computation time when calculating the weight matrix.
      m <- min(which(mse_cv <= 1.01 * min(mse_cv)))
    } else {
      m <- params_grid$nrounds[i]
    }
    if (mse_cv[m] <= MSE_CV_A2) {
      params_grid$nrounds[i] <- m
      params_A2 <- params_grid[i, ]
      MSE_CV_A2 <- mse_cv[m]
    }
  }
  xgbD_A1 <- xgb.DMatrix(as.matrix(df_treatment_A1[, -1]),
                         nthread = params_A2$nthread)
  l2boost_A2 <- xgb.train(
    nrounds = params_A2$nrounds,
    params = list(
      eta = params_A2$eta,
      max_depth = params_A2$max_depth,
      subsample = params_A2$subsample,
      colsample_bytree = params_A2$colsample_bytree,
      lambda = params_A2$lambda,
      nthread = params_A2$nthread
    ), data = xgbD_A2)

  nodes_A1 <- predict(l2boost_A2, newdata = xgbD_A1, predleaf = TRUE)

  if (n_A1 > 100) do_fast_matmult <- TRUE
  else do_fast_matmult <- FALSE

  l2boost_hatmatrix <- matrix(0, n_A1, n_A1)
  for (iter in seq_len(params_A2$nrounds)) {
    leaves_iter_A1 <- nodes_A1[, iter]
    # calculates the hat matrix of the tree.
    tree_hatmatrix <- get_tree_hatmatrix(leaves_iter_A1,
                                         self_predict = params_A2$self_predict)
    # updates the hat matrix of the boosting fit. See Algorithm 3 in Guo and Bühlmann (2022).
    if (do_fast_matmult) {
      l2boost_hatmatrix <- l2boost_hatmatrix + params_A2$eta *
        mat.mult(tree_hatmatrix, diag(n_A1) - l2boost_hatmatrix)
    } else {
      l2boost_hatmatrix <- l2boost_hatmatrix + params_A2$eta *
        tree_hatmatrix %*% (diag(n_A1) - l2boost_hatmatrix)
    }

  }
  # sets the diagonal of the hat matrix to zero and rescales the weights.
  if (!params_A2$self_predict) {
    row_sums_before <- rowSums(l2boost_hatmatrix)
    # if the sums of the weights is non-positive after setting the diagonal to zero then we use the original weights.
    ind <- row_sums_before - diag(l2boost_hatmatrix) > 0
    diag(l2boost_hatmatrix)[ind] <- 0
    row_sums_after <- rowSums(l2boost_hatmatrix)

    # rescales the weights such that the rows sum up to the same value as before setting the diagonal to zero.
    l2boost_hatmatrix <- l2boost_hatmatrix / row_sums_after * row_sums_before
  }
  return(list(
    weight = l2boost_hatmatrix,
    mse = MSE_CV_A2
  ))
}
