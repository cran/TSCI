#' Print Content of tsci Object.
#'
#' @param x an object of class 'tsci'.
#' @param digits number of significant digits to display.
#' @param ... arguments to be passed to or from other methods.
#'
#' @export
print.tsci <- function(x,
                       digits = max(3, getOption("digits") - 3),
                       ...) {
  # prints invalidity test.
  cat("\nStatistics about the validity of the instrument(s):\n")
  print(x$invalidity)

  # prints treatment effect estimate.
  cat("\nTreatment effect estimate of selected violation space candidate(s):\n")
  print(signif(x$Coef_sel[1], digits))
}
