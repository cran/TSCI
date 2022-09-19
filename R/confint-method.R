#' Confidence Intervals of Treatment Effect Estimates for TSCI Fits.
#'
#' @param object a object of class 'tsci'.
#' @param parm a specification of the parameters for which confidence intervals should be calculated.
#' Either a vector of numbers or a vector of names or 'all'.
#' If missing, the confidence interval of treatment effect estimate by violation space selection is returned.
#' If 'all', the confidence intervals for all violation space candidates are returned.
#' @param level the confidence level required.
#' @param ... additional argument(s) for methods.
#'
#' @return a matrix containing the confidence intervals.
#' @export
confint.tsci <- function(object, parm = NULL, level = 0.95, ...) {
  # this functions calculates the confidence intervals for the treatment effect estimates at
  # the desired level.
  stopifnot(inherits(object, "tsci"))
  alpha <- 1 - level
  conf_intervals <- t(cbind(object$CI_sel, object$CI_all))

  # checks if parm is of valid input and extracts the position of the coefficients
  # for which the CIs should be returned
  if (is.character(parm)) {
    stopifnot(all(parm %in% rownames(conf_intervals)) | parm == "all")
    if (parm != "all") {
      ind <- which(rownames(conf_intervals) %in% parm)
    } else {
      ind <- seq_len(NROW(conf_intervals))
    }

  }
  if (is.numeric(parm)) {
    stopifnot(all(parm %in% seq_len(NROW(conf_intervals))))
    ind <- parm
  }
  if (is.null(parm)) {
    ind <- which(rownames(conf_intervals) == "TSCI-Estimate")
  }
  conf_intervals <- conf_intervals[ind, , drop = FALSE]

  # CIs need to be calculated if another confidence level as 1 - alpha is chosen.
  if (level != (1 - object$alpha) & object$mult_split_method == "FWER") {
    if (is.null(object$coef_all_raw) |
        is.null(object$coef_sel_raw) |
        is.null(object$sd_all_raw) |
        is.null(object$sd_sel_raw)) {
      stop("FWER controled confidence intervals cannot be calculated.
             Rerun the tsci fitting function with 'output_raw' = TRUE.")
    } else {
      Coef_matrix <- cbind(object$coef_sel_raw, object$coef_all_raw)[, ind, drop = FALSE]
      sd_matrix <- cbind(object$sd_sel_raw, object$sd_all_raw)[, ind, drop = FALSE]
      conf_intervals <- t(sapply(seq_len(NCOL(Coef_matrix)),
                                 FUN = function(j) {get_FWER_CI(Coef = Coef_matrix[, j],
                                                                SE = sd_matrix[, j],
                                                                level = level)$CI}))
    }
  }
  if (level != (1 - object$alpha) & object$mult_split_method != "FWER") {
    lower <- c(object$Coef_sel, object$Coef_all)[ind] - qnorm(1 - alpha / 2) * c(object$sd_sel, object$sd_all)[ind]
    upper <- c(object$Coef_sel, object$Coef_all)[ind] + qnorm(1 - alpha / 2) * c(object$sd_sel, object$sd_all)[ind]
    conf_intervals <- cbind(lower, upper)
  }
  colnames(conf_intervals) <- c(paste(100 * alpha/2, "%"), paste(100*(1 - alpha/2), "%"))
  rownames(conf_intervals) <- names(c(object$Coef_sel, object$Coef_all)[ind])

  return(conf_intervals)
}
