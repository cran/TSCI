#' @noRd
check_input_forest <- function(error_message,
                               check_values,
                               num_trees,
                               mtry,
                               max_depth,
                               min_node_size,
                               self_predict,
                               ...) {
  # this function checks if the input that is specific for tsci_forest
  # is in the correct format and the values are valid.
  if (!check_values) {
    if (!is.numeric(num_trees))
      error_message <- paste(error_message, "num_trees is not numeric.", sep = "\n")
    if (!is.numeric(mtry) & !is.null(mtry) & !is.list(mtry))
      error_message <- paste(error_message, "mtry is not numeric nor (a list of) function(s).", sep = "\n")
    if (!is.numeric(max_depth))
      error_message <- paste(error_message, "max_depth is not numeric.", sep = "\n")
    if (!is.numeric(min_node_size))
      error_message <- paste(error_message, "min_node_size is not numeric.", sep = "\n")
    if (!is.logical(self_predict))
      error_message <- paste(error_message, "self_predict is neither TRUE nor FALSE", sep = "\n")
    error_message <- check_input_data_splitting(error_message = error_message,
                                                check_values = check_values,
                                                ...)
    return(error_message)
  } else {
    if (any(num_trees < 0))
      error_message <- paste(error_message, "num_trees cannot be negative.", sep = "\n")
    if (is.numeric(mtry))
      if (any(mtry <= 0))
        error_message <- paste(error_message, "mtry cannot be negative or larger than
                             the sum of the number of columns in Z and X.", sep = "\n")
    if (any(max_depth < 0))
      error_message <- paste(error_message, "max_depth cannot be negative.", sep = "\n")
    if (any(min_node_size < 0))
      error_message <- paste(error_message, "min_node_size cannot be negative.", sep = "\n")
    error_message <- check_input_data_splitting(error_message = error_message,
                                                check_values = check_values,
                                                ...)
    return(error_message)
  }
}
