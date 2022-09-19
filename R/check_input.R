#' @noRd
check_input <- function(Y,
                        D,
                        Z,
                        X,
                        W,
                        vio_space,
                        create_nested_sequence,
                        intercept,
                        iv_threshold,
                        threshold_boot,
                        alpha,
                        B,
                        tsci_method,
                        ...
                        ) {
  # This functions checks if the input parameters are in a valid format and
  # if the input values are possible.

  # initializes error message.
  error_message <- NULL

  # checks input parameters shared by all tsci functions (except for X in tsci_secondstage).
  if (!is.numeric(as.matrix(Y)))
    error_message <- paste(error_message, "Y is not numeric.", sep = "\n")
  if (!is.numeric(as.matrix(D)))
    error_message <- paste(error_message, "D is not numeric.", sep = "\n")
  if (!is.numeric(as.matrix(Z)))
    error_message <- paste(error_message, "Z is not numeric.", sep = "\n")
  if (!is.null(X)) {
    if (!is.numeric(as.matrix(X)))
      error_message <- paste(error_message, "X is not numeric.", sep = "\n")
  }
  if (!is.null(W)) {
    if (!is.numeric(as.matrix(W)))
      error_message <- paste(error_message, "W is not numeric.", sep = "\n")
  }
  if (is.null(vio_space) & tsci_method != "poly") {
    error_message <- paste(error_message, "vio_space must be provided.", sep = "\n")
  } else {
    if (!is.null(vio_space) & !is.list(vio_space)) {
      error_message <- paste(error_message, "vio_space is not a list.", sep = "\n")
    } else if (is.list(vio_space)) {
      if (!is.numeric(unlist(vio_space)))
        error_message <- paste(error_message, "vio_space is not numeric.", sep = "\n")
    }
  }
  if (!is.logical(intercept))
    error_message <- paste(error_message, "intercept is neither TRUE nor FALSE.", sep = "\n")
  if (!is.logical(create_nested_sequence))
    error_message <- paste(error_message, "create_nested_sequence is neither TRUE nor FALSE.", sep = "\n")
  if (!is.numeric(iv_threshold))
    error_message <- paste(error_message, "iv_threshold is not numeric.", sep = "\n")
  if (!is.logical(threshold_boot))
    error_message <- paste(error_message, "threshold_boot is neither TRUE nor FALSE.", sep = "\n")
  if (!is.numeric(alpha))
    error_message <- paste(error_message, "alpha is not numeric.", sep = "\n")
  if (!is.numeric(B))
    error_message <- paste(error_message, "B is not numeric.", sep = "\n")

  # checks input parameters of tsci_forest.
  if (tsci_method == "random forest")
    error_message <- check_input_forest(error_message = error_message,
                                        check_values = FALSE,
                                        ...)


  # checks input parameters of tsci_boosting.
  if (tsci_method == "boosting")
  error_message <- check_input_boosting(error_message = error_message,
                                        check_values = FALSE,
                                        ...)

  # checks input parameters of tsci_poly.
  if (tsci_method == "poly")
    error_message <- check_input_poly(error_message = error_message,
                                      check_values = FALSE,
                                      Z = Z,
                                      ...)

  # checks input parameters of tsci_secondstage.
  if (tsci_method == "user defined")
    error_message <- check_input_secondstage(error_message = error_message,
                                             check_values = FALSE,
                                             Y = Y,
                                             ...)

  # Raises error if at least one input parameter was not in the correct format.
  if (!is.null(error_message))
    stop(error_message)

  # if all input parameters are of valid format checks further if the values themselves are possible.

  # checks input parameters shared by all tsci functions.
  if (length(unique(sapply(list(Y, D, Z), FUN = function(variable) NROW(variable)))) > 1) {
    error_message <- paste(error_message, "Y, D and Z have not the same number of observations.", sep = "\n")
  } else {
    n <- NROW(Y)
    if (!is.null(X))
      if(NROW(X) != n)
        error_message <- paste(error_message, "X has not the same number of observations as Y.", sep = "\n")
    if (!is.null(W))
      if(NROW(W) != n)
        error_message <- paste(error_message, "W has not the same number of observations as Y.", sep = "\n")
    if (!is.null(vio_space))
      if(length(unique(sapply(vio_space, FUN = function(variable) NROW(variable)))) > 1)
        error_message <- paste(error_message, "vio_space has not the same number of observations as Y.", sep = "\n")
  }
  if (any(is.na(Y)))
    error_message <- paste(error_message, "There are NA's in Y.", sep = "\n")
  if (any(is.na(D)))
    error_message <- paste(error_message, "There are NA's in D.", sep = "\n")
  if (any(is.na(Z)))
    error_message <- paste(error_message, "There are NA's in Z.", sep = "\n")
  if (!is.null(W))
    if(any(is.na(W)))
      error_message <- paste(error_message, "There are NA's in W.", sep = "\n")
  if (!is.null(vio_space))
    if(any(is.na(unlist(vio_space))))
      error_message <- paste(error_message, "There are NA's in vio_space.", sep = "\n")
  if (alpha > 1)
    error_message <- paste(error_message, "alpha cannot be larger than 1.", sep = "\n")

  # checks input parameters of tsci_forest.
  if (tsci_method == "random forest")
    error_message <- check_input_forest(error_message = error_message,
                                        check_values = TRUE,
                                        ...)


  # checks input parameters of tsci_boosting.
  if (tsci_method == "boosting")
    error_message <- check_input_boosting(error_message = error_message,
                                          check_values = TRUE,
                                          ...)

  # checks input parameters of tsci_poly.
  if (tsci_method == "poly")
    error_message <- check_input_poly(error_message = error_message,
                                      check_values = TRUE,
                                      Z = Z,
                                      ...)

  # checks input parameters used when the user provides the hat matrix.
  if (tsci_method == "user defined")
    error_message <- check_input_secondstage(error_message = error_message,
                                             check_values = TRUE,
                                             Y = Y,
                                             ...)

  # Raises error if at least one input parameter had non possible values.
  if (!is.null(error_message))
    stop(error_message)
}
