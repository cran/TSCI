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

  xgbD_A2 <- xgb.DMatrix(as.matrix(df_treatment_A2[, -1]), label = df_treatment_A2[, 1])

  MSE_CV_A2 <- Inf
  params_A2 <- NULL


  for (i in seq_len(NROW(params_grid))) {
    temp_A2 <- xgb.cv(
      params = list(
        eta = params_grid$eta[i],
        max_depth = params_grid$max_depth[i],
        subsample = params_grid$subsample[i],
        colsample_bytree = params_grid$colsample_bytree[i],
        lambda = 0
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

  xgbD_A1 <- xgb.DMatrix(as.matrix(df_treatment_A1[, -1]))
  l2boost_A2 <- xgb.train(
    nrounds = params_A2$nrounds,
    params = list(
      eta = params_A2$eta,
      max_depth = params_A2$max_depth,
      subsample = params_A2$subsample,
      colsample_bytree = params_A2$colsample_bytree,
      lambda = params_A2$lambda
    ), data = xgbD_A2)
  nodes_A1 <- predict(l2boost_A2, newdata = xgbD_A1, predleaf = TRUE)


  l2boost_hatmatrix <- matrix(0, n_A1, n_A1)
  for (iter in seq_len(params_A2$nrounds)) {
    leaves_iter_A1 <- nodes_A1[, iter]
    # calculates the hat matrix of the tree.
    tree_hatmatrix <- get_tree_hatmatrix(leaves_iter_A1)
    # updates the hat matrix of the boosting fit. See Algorithm 3 in Guo and Bühlmann (2022).
    l2boost_hatmatrix <- l2boost_hatmatrix + params_A2$eta *
      mat.mult(tree_hatmatrix, diag(n_A1) - l2boost_hatmatrix)
  }
  return(list(
    weight = l2boost_hatmatrix,
    mse = MSE_CV_A2
  ))
}
