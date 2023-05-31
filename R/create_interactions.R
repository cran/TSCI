#' Interactions as Violation Space Candidates
#'
#' @param Z observations of the instrumental variable(s). Either a numeric vector of length n
#' or a numeric matrix with dimension n by s.
#' @param X observations of baseline covariate(s) for which interactions with the instrumental variable(s) should
#' be part of the violation space candidates. Either a numeric vector of length n
#' or a numeric matrix with dimension n by p or \code{NULL}
#' (if only interactions between the instrumental variales itself should be part
#' of the violation space candidates).
#'
#' @return A list. The first element contains the observations of the
#' instrumental variable(s) \code{Z}. The second element contains all interactions between
#' the instrumental variable(s) and the baseline covariate(s) \code{X}.
#'
#' @export
#'
#' @examples
#' Z <- matrix(rnorm(100 * 3), nrow = 100, ncol = 3)
#' X <- matrix(rnorm(100 * 3), nrow = 100, ncol = 3)
#' vio_space <- create_interactions(Z = Z, X = X)
create_interactions <- function(Z, X = NULL) {
  # this function creates an ordered list of size 2. The first element are the instruments Z,
  # the second element are all two-way interactions between the instruments themselves
  # and between the instruments and the baseline covariates X.

  # checks if input is in the correct format and valid.
  error_message <- NULL
  if (!is.numeric(Z))
    error_message <- paste(error_message, "Z is not numeric.", sep = "\n")
  if (!is.numeric(X) & !is.null(X))
    error_message <- paste(error_message, "X is not numeric.", sep = "\n")
  if (!is.null(X)) {
    if (NROW(Z) != NROW(X))
      error_message <-
        paste(error_message, "Z and X don't have the same number of observations.",
              sep = "\n")
  }
  if (!is.null(error_message))
    stop(error_message)

  if (NCOL(Z) + NCOL(X) > 1) {
    V <- cbind(Z, X)
    interactions <- vector("list", length = 2)
    interactions[[1]] <- as.matrix(Z)
    tmp <-
      do.call(cbind, lapply(seq_len(min(c(NCOL(Z), NCOL(V) - 1))),
                            FUN = function(i) V[,i] * V[, (i+1):NCOL(V)]))
    dimnames(tmp) <- NULL
    interactions[[2]] <- tmp
  } else {
    interactions <- list(as.matrix(Z))
  }
  return(interactions)
}
