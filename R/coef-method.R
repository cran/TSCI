#' Extract Model Coefficients of TSCI Fits.
#'
#' @param object an object of class 'tsci'.
#' @param parm a specification for which treatment effect estimates should be returned.
#' Either a vector of numbers or a vector of names or 'all'.
#' If missing, the treatment effect estimate by violation space selection is returned.
#' If 'all', the treatment effect estimates for all violation space candidates are returned.
#' @param ... arguments to be passed to or from other methods.
#'
#' @return Coefficients extracted form the model object \code{object}.
#' @export
coef.tsci <- function(object,
                      parm = NULL,
                      ...) {
  coefs <- c(object$Coef_sel, object$Coef_all)
  if (is.character(parm)) {
    stopifnot(all(parm %in% names(coefs)) | parm == "all")
    if (parm[1] != "all") {
      ind <- which(names(coefs) %in% parm)
    } else {
      ind <- seq_len(length(coefs))
    }
  }
  if (is.numeric(parm)) {
    stopifnot(all(parm %in% seq_len(length(coefs))))
    ind <- parm
  }
  if (is.null(parm)) {
    ind <- which(names(coefs) == "TSCI-Estimate")
  }
  return(coefs[ind])
}
