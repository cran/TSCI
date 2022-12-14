#' @noRd
#' @importFrom ranger ranger
#' @importFrom stats predict
get_forest_hatmatrix <- function(df_treatment_A1, df_treatment_A2, params_grid, ...) {
  # this functions performs hyper-parameter tuning of the random forest parameters and
  # calculates the hat matrix of the treatment model for A1 using the random forest fit.
  forest_A2 <- NULL
  MSE_oob_A2 <- Inf
  params_A2 <- NULL
  # uses oob error to do hyper-parameter tuning.
  for (i in seq_len(nrow(params_grid))) {
    temp_A2 <- ranger(D ~ .,
                      data = df_treatment_A2,
                      num.trees = params_grid$num_trees[i],
                      mtry = params_grid$mtry[i][[1]],
                      max.depth = params_grid$max_depth[i],
                      min.node.size = params_grid$min_node_size[i],
                      importance = "impurity"
    )
    if (temp_A2$prediction.error <= MSE_oob_A2) {
      forest_A2 <- temp_A2
      params_A2 <- params_grid[i, ]
      MSE_oob_A2 <- temp_A2$prediction.error
    }
  }

  # calculates the hat matrix.
  leaves <- predict(forest_A2, data = df_treatment_A1, type = "terminalNodes")$predictions
  n_A1 <- NROW(leaves)
  forest_hatmatrix <- matrix(0, n_A1, n_A1)
  for (j in seq_len(params_A2$num_trees)) {
    # weight matrix of a single tree.
    tree_hatmatrix <- get_tree_hatmatrix(leaves = leaves[, j],
                                         self_predict = params_A2$self_predict)
    # updating weight matrix of the random forest.
    forest_hatmatrix <- forest_hatmatrix + tree_hatmatrix / params_A2$num_trees
  }
  return(list(
    weight = forest_hatmatrix,
    mse = MSE_oob_A2
  ))
}
