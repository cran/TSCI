#' @noRd
check_input_secondstage <- function(error_message,
                                    check_values,
                                    Y,
                                    A1_ind,
                                    weight,
                                    ...) {
  # this function checks if the input that is specific for tsci_secondstage
  # is in the correct format and the values are valid.
  if (!check_values) {
    if (!is.numeric(A1_ind) & !is.null(A1_ind))
      error_message <- paste(error_message, "A1_ind is neither numeric nor NULL.", sep = "\n")
    if (!is.numeric(weight))
      error_message <- paste(error_message, "weight is not numeric.", sep = "\n")
    return(error_message)
  } else {
    if (!is.null(A1_ind)) {
      if (any(duplicated(A1_ind)))
        error_message <- paste(error_message, "There are duplicates in A1_ind.", sep = "\n")
      if (length(A1_ind) > NROW(Y))
        error_message <- paste(error_message, "A1_ind contains more elements than Y has observations", sep = "\n")
      if (any(A1_ind < 0) | any(A1_ind > NROW(Y)))
        error_message <- paste(error_message, "A1_ind contains elements that are negative or larger than Y has observations", sep = "\n")
      if (length(A1_ind) != NROW(weight))
        error_message <- paste(error_message, "A1_ind contains not the same number of elements as weight has rows.", sep = "\n")
    }
    if (NROW(weight) != NROW(Y))
      error_message <- paste(error_message, "weight has not the same number of rows as Y has observations.", sep = "\n")
    return(error_message)
  }
}
