#' @noRd
check_input_poly <- function(error_message,
                             check_values,
                             Z,
                             min_order,
                             max_order,
                             exact_order,
                             gcv,
                             ...) {
  # this function checks if the input that is specific for tsci_poly
  # is in the correct format and the values are valid.
  if (!check_values) {
    if (!is.numeric(min_order) & !is.null(min_order))
      error_message <- paste(error_message, "min_order is not numeric.", sep = "\n")
    if (!is.numeric(max_order) & !is.null(max_order))
      error_message <- paste(error_message, "max_order is not numeric.", sep = "\n")
    if (!is.numeric(exact_order) & !is.null(exact_order))
      error_message <- paste(error_message, "exact_order is not numeric.", sep = "\n")
    if (!is.logical(gcv))
      error_message <- paste(error_message, "gcv is neither TRUE nor FALSE.", sep = "\n")
    return(error_message)
  } else {
    if (is.null(exact_order) & (is.null(min_order) | is.null(max_order)))
      error_message <- paste(error_message, "Either exact_order or min_order and max_order must be specified.", sep = "\n")
    if (length(max_order) != length(min_order))
      error_message <- paste(error_message, "min_order and max_order must be of same length.", sep = "\n")
    if (length(max_order) > 1 & length(max_order) != NCOL(Z))
      error_message <- paste(error_message, "min_order and max_order have invalid length.", sep = "\n")
    if (!is.null(min_order) & any(min_order < 1))
      error_message <- paste(error_message, "Values in min_order cannot be smaller than 1.", sep = "\n")
    if (!is.null(max_order) & any(max_order < min_order))
      error_message <- paste(error_message, "Values in max_order cannot be smaller than values in min_order.", sep = "\n")
    return(error_message)
  }
}
