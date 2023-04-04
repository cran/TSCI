#' @noRd
check_output <- function(list_outputs, ind_start) {
  # this functions checks if errors occurred in each sample split.
  # it returns the proportion and position of data splits in which NA's occurred and
  # the error messages that were raised in this data splits.
  n <- length(list_outputs)
  ind_na <- sapply(list_outputs, FUN = function(x) any(is.na(x$value)))
  prop_na <- sum(ind_na) / n
  errors <- lapply(list_outputs, FUN = function(x) x$error)
  messages <- lapply(list_outputs, FUN = function(x) x$message)
  warnings <- lapply(list_outputs, FUN = function(x) x$warning)
  error_string <- NULL
  for (i in seq_len(n)) {
    if(!is.null(errors[[i]]) | !is.null(messages[[i]]) | !is.null(warnings[[i]])){
      error_string <-paste(error_string,
                           paste("split", i + ind_start - 1), sep = "\n")
      if (!is.null(errors[[i]])) {
        error_string <- paste(error_string,
                              "The following errors occurred:",
                              errors[[i]],
                              sep = "\n")
      }
      if (!is.null(messages[[i]])) {
        error_string <- paste(error_string,
                              "The following messages were returned:",
                              messages[[i]],
                              sep = "\n")
      }
      if (!is.null(warnings[[i]])) {
        error_string <- paste(error_string,
                              "The following warnings were raised:",
                              warnings[[i]],
                              sep = "\n")
      }
    }
  }

  return(list(prop_na  = prop_na,
              ind_na = ind_na,
              error_string = error_string))
}
