#' @noRd
#' @importFrom fastDummies dummy_cols
dummy_encoding <- function(Y = Y,
                           D = D,
                           Z = Z,
                           X = X,
                           W = W,
                           vio_space = vio_space) {
  # this function performs dummy encoding for character and factor type columns
  # and transforms variables to matrices.
  # Currently, no dummy encoding for the outcome and treatment variable is performed
  # as the direction of the treatment effect estimate depends on the level that would
  # be dropped.
  Y <- as.matrix(Y)
  D <- as.matrix(D)
  if (!is.numeric(as.matrix(Z))) {
    Z <- as.matrix(dummy_cols(Z,
                              remove_first_dummy = T,
                              remove_selected_columns = T))
  } else {
    Z <- as.matrix(Z)
  }
  if (!is.null(X)) {
    if (!is.numeric(as.matrix(X))) {
      X <- as.matrix(dummy_cols(X,
                                remove_first_dummy = T,
                                remove_selected_columns = T))
    } else {
      X <- as.matrix(X)
    }
  }
  if (!is.null(W)) {
    if (!is.numeric(as.matrix(W))) {
      W <- as.matrix(dummy_cols(W,
                              remove_first_dummy = T,
                              remove_selected_columns = T))
    } else {
      W <- as.matrix(W)
    }
  }
  if (!is.null(vio_space)) {
    vio_space <-
      lapply(vio_space,
             FUN = function(x) if (!is.numeric(as.matrix(x))) {
               return(as.matrix(dummy_cols(x,
                                    remove_first_dummy = T,
                                    remove_selected_columns = T)))
             } else {return(x)}
             )
  }
  return(list(Y = Y,
              D = D,
              Z = Z,
              X = X,
              W = W,
              vio_space = vio_space))
}
