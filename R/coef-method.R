#' Extract Model Coefficients of TSCI Fits
#'
#' @param object a object of class 'tsci'.
#' @param all logical. If \code{FALSE}, only the treatment effect estimate of the
#' selected violation space candidate(s) is returned. If \code{TRUE}, additionally the treatment
#' effect estimates of each violation space candidate are returned.
#' @param ... arguments to be passed to or from other methods.
#'
#' @return Coefficients extracted form the model object \code{object}.
#' @export
coef.tsci <- function(object,
                      all = FALSE,
                      ...) {
  stopifnot(inherits(object, "tsci"))
  if (all) return(c(object$Coef_sel, object$Coef_all))
  return(object$Coef_sel)
}
