#' Monomials as Violation Space Candidates
#'
#' @param Z observations of the instrumental variable(s). Either a numeric vector of length n
#' or a numeric matrix with dimension n by s.
#' @param degree The degree up to which monomials should be created. Either a single numeric value or a numeric value of length s.
#' @param type One out of \code{monomials_main} or \code{monomials_full}. \cr
#' \code{monomials_main} creates the monomials for the polynomials of each instrumental variable up to degree \code{degree}. \cr
#' \code{monomials_full} creates the monomials for the polynomials of a combination of all instrumental variables up to degree \code{degree}. \cr
#' Default is \code{monomials_full}.
#'
#' @return A ordered list. Each element is a matrix consisting of the monomials to be added to
#' the next violation space candidate.
#'
#' @details assuming there are 3 instrumental variables Z1, Z2, and Z3 and \code{degree} = c(d1, d2, d3) with d1 < d2 < d3,
#' \code{monomials_main} creates the monomials of the polynomials (Z1 + 1)^d1, (Z2 + 1)^d2, (Z3 + 1)^d3 without the constants and
#' \code{monomials_full} creates the monomials (Z1 + Z2 + Z3), (Z1 + Z2 + Z3)^2, ..., (Z1 + Z2 + Z3)^d3 without the constants and excluding
#' monomials that are products of Z1^d or Z2^d with d > d1 resp. d > d2.
#' Thus \code{type} = \code{monomials_main} does not include interactions between the instrumental variables.
#'
#' @export
#'
#' @examples
#' Z <- matrix(rnorm(100 * 3), nrow = 100, ncol = 3)
#' vio_space <- create_monomials(Z = Z, degree = 4, type = "monomials_full")
create_monomials <- function(Z, degree, type = c("monomials_main", "monomials_full")) {
  # this function creates a ordered list. Each element consists of a matrix where
  # the columns are monomials of the same degree.

  # checks if input is in the correct format and valid.
  error_message <- NULL
  if (!is.numeric(Z))
    error_message <- paste(error_message, "Z is not numeric.", sep = "\n")
  if (!is.numeric(degree))
    error_message <- paste(error_message, "degree is not numeric.", sep = "\n")
  if (length(degree) > 1 & length(degree) != NCOL(Z))
    error_message <- paste(error_message, "degree has invalid length.", sep = "\n")

  if (!is.null(error_message))
    stop(error_message)

  type <- match.arg(type)

  # builds monomials.
  Z <- as.matrix(Z)
  n <- NROW(Z)
  p <- NCOL(Z)
  if(length(degree) == 1) degree <- rep(degree, p)
  monomials <- vector("list", length = max(degree) + 1)
  monomials[[1]] <- matrix(1, nrow = n)
  monomials_degrees <- matrix(0, nrow = p, ncol = 1)
  for (q in seq_len(max(degree))) {
    if (type == "monomials_full") {
      # starts with a constant. multiplies each instrumental variable with all columns
      # obtained from the previous iteration for which 'degree' is not reached.
      current_monomials_degrees <- matrix(NA, nrow = p, ncol = p * NCOL(monomials[[q]]))
      current_monomials <- matrix(NA, nrow = n, ncol = p * NCOL(monomials[[q]]))
      for (i in seq_len(NCOL(monomials[[q]]))) {
        for (j in seq_len(p)) {
          ind <- (i - 1) * p + j
          current_monomials_degrees[, ind] <- monomials_degrees[, i]
          current_monomials_degrees[j, ind] <- current_monomials_degrees[j, ind] + 1
          if (all(current_monomials_degrees[, ind] <= degree)) current_monomials[, ind] <- monomials[[q]][, i] * Z[, j]
        }
      }
    } else if (type == "monomials_main") {
      # works similar as 'monomials_full' but without interactions.
      current_monomials_degrees <- matrix(NA, nrow = p, ncol = NCOL(monomials[[q]]))
      current_monomials <- matrix(NA, nrow = n, ncol = NCOL(monomials[[q]]))
      if (q == 1) {
        current_monomials_degrees <- diag(p)
        current_monomials <- Z
      } else {
        ind_old <- which((q - 1) <= degree)
        ind_new <- which(q <= degree)
        j <- 1
        for (i in seq_len(NCOL(monomials[[q]]))) {
          if (ind_old[i] %in% ind_new) {
            current_monomials_degrees[, i] <- monomials_degrees[, i]
            current_monomials_degrees[ind_new[j], i] <- current_monomials_degrees[ind_new[j], i] + 1
            current_monomials[, i] <- monomials[[q]][, i] * Z[, ind_new[j]]
            j <- j + 1
          }
        }
      }
    }
    # removes duplicate rows.
    pos <- apply(current_monomials, 2, function(x) all(!is.na(x)))
    current_monomials <- current_monomials[, pos, drop = FALSE]
    current_monomials_degrees <- current_monomials_degrees[, pos, drop = FALSE]
    duplicated_columns <- duplicated(t(round(current_monomials, 10)))
    monomials[[q + 1]] <- current_monomials[, !duplicated_columns, drop = FALSE]
    monomials_degrees <- current_monomials_degrees[, !duplicated_columns, drop = FALSE]
  }
  return(monomials[-1])
}
