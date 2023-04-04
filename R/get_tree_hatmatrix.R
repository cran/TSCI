#' @noRd
get_tree_hatmatrix <- function(leaves, self_predict = TRUE) {
  # this function returns the hat matrix of a single fitted tree.
  n <- length(leaves)
  # initializes weight matrix for a single tree.
  weight_mat <- matrix(0, n, n)
  unique_leaves <- unique(leaves)
  single_obs <- FALSE
  for(leave in unique_leaves){
    # indices the observations in the current leaf.
    leaves_pos <- which(leave == leaves)
    # calculates the number of observations in the current leaf.
    num_samples <- length(leaves_pos)
    # flag if some leaves have only one observation.
    if (num_samples <= 1) single_obs <- TRUE
    # calculates the weights.
    if (!single_obs & !self_predict) {
      weight_mat[leaves_pos, leaves_pos] <- 1 / (num_samples - 1)
    } else {
      weight_mat[leaves_pos, leaves_pos] <- 1 / num_samples
    }
  }
  if (!self_predict) {
    if (single_obs) {
      ind <- rowSums(weight_mat) - diag(weight_mat) > 0
      diag(weight_mat)[ind] <- 0
    } else {
      diag(weight_mat) <- 0
    }
  }
  return(weight_mat)
}
