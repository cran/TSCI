#' @noRd
check_input_data_splitting <- function(error_message,
                                       check_values,
                                       split_prop,
                                       nsplits,
                                       ncores,
                                       raw_output,
                                       ...) {
  # this function checks if the input that is specific for multi-splitting
  # is in the correct format and the values are valid.
  if (!check_values) {
    if (!is.numeric(split_prop))
      error_message <- paste(error_message, "split_prop is not numeric.", sep = "\n")
    if (!is.numeric(nsplits))
      error_message <- paste(error_message, "nsplits is not numeric.", sep = "\n")
    if (!is.numeric(ncores))
      error_message <- paste(error_message, "ncores is not numeric.", sep = "\n")
    if (!is.logical(raw_output) & !is.null(raw_output))
      error_message <- paste(error_message, "raw_output is neither TRUE nor FALSE nor NULL", sep = "\n")
    return(error_message)
  } else {
    if (split_prop <= 0 | split_prop >= 1)
      error_message <- paste(error_message, "split_prop is not in (0, 1).", sep = "\n")
    if (nsplits < 1)
      error_message <- paste(error_message, "nsplits cannot be smaller than 1.", sep = "\n")
    if (ncores < 1)
      error_message <- paste(error_message, "ncores cannot be smaller than 1.", sep = "\n")
    return(error_message)
  }
}
