#' @noRd
check_input_boosting <- function(error_message,
                                 check_values,
                                 nrounds,
                                 eta,
                                 max_depth,
                                 subsample,
                                 colsample_bytree,
                                 early_stopping,
                                 nfolds,
                                 self_predict,
                                 ...) {
  # this function checks if the input that is specific for tsci_boosting
  # is in the correct format and the values are valid.
  if (!check_values) {
    if (!is.numeric(nrounds))
      error_message <- paste(error_message, "nrounds is not numeric.", sep = "\n")
    if (!is.numeric(eta))
      error_message <- paste(error_message, "eta is not numeric.", sep = "\n")
    if (!is.numeric(max_depth))
      error_message <- paste(error_message, "max_depth is not numeric.", sep = "\n")
    if (!is.numeric(subsample))
      error_message <- paste(error_message, "subsample is not numeric.", sep = "\n")
    if (!is.numeric(colsample_bytree))
      error_message <- paste(error_message, "colsample_bytree is not numeric.", sep = "\n")
    if (!is.logical(early_stopping))
      error_message <- paste(error_message, "early_stopping is neither TRUE nor FALSE.", sep = "\n")
    if (!is.numeric(nfolds))
      error_message <- paste(error_message, "nfolds is not numeric.", sep = "\n")
    if (!is.logical(self_predict))
      error_message <- paste(error_message, "self_predict is neither TRUE nor FALSE", sep = "\n")
    error_message <- check_input_data_splitting(error_message = error_message,
                                                check_values = check_values,
                                                ...)
    return(error_message)
  } else {
    if (any(nrounds < 0))
      error_message <- paste(error_message, "num_trees cannot be negative.", sep = "\n")
    if (any(eta < 0))
      error_message <- paste(error_message, "eta cannot be negative.", sep = "\n")
    if (any(max_depth < 0))
      error_message <- paste(error_message, "max_depth cannot be negative.", sep = "\n")
    if (any(subsample < 0) | any(subsample > 1))
      error_message <- paste(error_message, "subsample is not in [0, 1].", sep = "\n")
    if (any(colsample_bytree < 0) | any(colsample_bytree > 1))
      error_message <- paste(error_message, "colsample_bytree is not in [0, 1].", sep = "\n")
    if (any(nfolds < 0))
      error_message <- paste(error_message, "nfolds cannot be negative.", sep = "\n")
    error_message <- check_input_data_splitting(error_message = error_message,
                                                check_values = check_values,
                                                ...)
    return(error_message)
  }
}
