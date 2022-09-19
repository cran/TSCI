#' @noRd
#' @importFrom stats lm coef
get_poly_parameters <- function(df_treatment,
                                params_list,
                                order_selection_method,
                                max_iter,
                                conv_tol,
                                gcv,
                                nfolds) {
  # this function is used to select the optimal order of the polynomials.
  # selects optimal order of the polynomials.
  if (max(sapply(seq_len(length(params_list)), FUN = function(i) length(params_list[[i]]))) > 1) {
    if (order_selection_method == "backfitting") {
      optimal_order <- backfitting(df = df_treatment,
                                   params_list = params_list,
                                   max_iter = max_iter,
                                   conv_tol = conv_tol,
                                   gcv = gcv,
                                   nfolds = nfolds)
    }
    else if (order_selection_method == "grid search") {
      optimal_order <- grid_search(df = df_treatment,
                                   params_list = params_list,
                                   gcv = gcv,
                                   nfolds = nfolds)
    } else {
      stop("No valid order selection method chosen.")
    }
  } else {
    optimal_order <- unlist(params_list)
  }

  # performs k-fold cross-validation for the chosen order of polynomials to get an out-of-sample MSE.
  A <- matrix(1, nrow = NROW(df_treatment))
  for (j in seq_len(length(optimal_order))) {
    A <- cbind(A, poly(df_treatment[, j + 1], degree = optimal_order[j]))
  }
  MSE_CV <- cross_validate(Y = df_treatment[, 1],
                           X = A,
                           nfolds)
  return(list(
    params = list(optimal_order),
    mse = MSE_CV
  ))
}
