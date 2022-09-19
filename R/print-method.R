#' @exportS3Method
print.tsci <- function(x, ...) {
  # the same output is printed as when calling summary.tsci.
  stopifnot(inherits(x, "tsci"))

  print.summary.tsci(summary.tsci(object = x, extended_output = FALSE,  ...), ...)
}
