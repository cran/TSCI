#' Output Aggregation
#'
#' @param output_list a list with four elements: \code{value}, \code{error}, \code{warning} and \code{message}.
#' @param alpha the significance level.
#' @param Q the number of tested violation spaces.
#' @param mult_split_method method used for inference if multi-splitting is performed. Either 'DML' or 'FWER'.
#'
#' @return a list containing the aggregated statistics.
#' @noRd
#' @importFrom stats median
aggregate_output <- function(output_list, alpha, Q,  mult_split_method, raw_output) {

  # creates list with NAs.
  returnList <- tsci_fit_NA_return(Q)


  # extracts coefficient estimates and standard errors of each iteration.
  Coef_all_matrix <-
    matrix(unlist(lapply(output_list, FUN = function(x) x$value$Coef_all), use.names = FALSE),
           ncol = length(returnList$Coef_all), byrow = TRUE)
  sd_all_matrix <-
    matrix(unlist(lapply(output_list, FUN = function(x) x$value$sd_all), use.names = FALSE),
           ncol = length(returnList$sd_all), byrow = TRUE)
  Coef_sel_matrix <-
    matrix(unlist(lapply(output_list, FUN = function(x) x$value$Coef_sel), use.names = FALSE),
                               ncol = length(returnList$Coef_sel), byrow = TRUE)
  sd_sel_matrix <-
    matrix(unlist(lapply(output_list, FUN = function(x) x$value$sd_sel), use.names = FALSE),
           ncol = length(returnList$sd_sel), byrow = TRUE)


  # extracts and aggregates the statistics that do not depend on mult_split_method.
  returnList$Coef_all[] <- apply(Coef_all_matrix, 2, FUN = median)
  returnList$Coef_sel[] <- apply(Coef_sel_matrix, 2, FUN = median)
  returnList$iv_str[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$iv_str), use.names = FALSE),
                 ncol = length(returnList$iv_str), byrow = TRUE), 2, median)
  returnList$iv_thol[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$iv_thol), use.names = FALSE),
                 ncol = length(returnList$iv_thol), byrow = TRUE), 2, median)
  returnList$Qmax[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$Qmax), use.names = FALSE),
                 ncol = length(returnList$Qmax), byrow = TRUE), 2, sum)
  returnList$q_comp[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$q_comp), use.names = FALSE),
                 ncol = length(returnList$q_comp), byrow = TRUE), 2, sum)
  returnList$q_cons[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$q_cons), use.names = FALSE),
                 ncol = length(returnList$q_cons), byrow = TRUE), 2, sum)
  returnList$invalidity[] <-
    apply(matrix(unlist(lapply(output_list, FUN = function(x) x$value$invalidity), use.names = FALSE),
                 ncol = length(returnList$invalidity), byrow = TRUE), 2, sum)
  mse <-
    unlist(lapply(output_list, FUN = function(x) x$value$mse))
  if (any(mse == -1)) {
    returnList$mse[] <- NA
  } else {
      returnList$mse[] <- median(mse)
    }


  if (mult_split_method == "FWER") {
    # calculates the p-values and confidence intervals using the "FWER" method
    # introduced in Meinshausen, Meier, Bühlmann (2009).
    stats_all <-
      lapply(seq_len(NCOL(Coef_all_matrix)),
                           FUN = function(j) {get_FWER_CI(Coef = Coef_all_matrix[, j],
                                                          SE = sd_all_matrix[, j],
                                                          level = 1 - alpha)})
    returnList$pval_all[] <- unlist(lapply(stats_all, FUN = function(x) x$p_value))
    returnList$CI_all[] <- matrix(unlist(lapply(stats_all, FUN = function(x) x$CI)), nrow = 2)

    stats_sel <-
      lapply(seq_len(NCOL(Coef_sel_matrix)),
             FUN = function(j) {get_FWER_CI(Coef = Coef_sel_matrix[, j],
                                            SE = sd_sel_matrix[, j],
                                            level = 1 - alpha)})
    returnList$pval_sel[] <- unlist(lapply(stats_sel, FUN = function(x) x$p_value))
    returnList$CI_sel[] <- matrix(unlist(lapply(stats_sel, FUN = function(x) x$CI)), nrow = 2)

    # this method does not provide standard error estimates.
    returnList$sd_all[] <- NA
    returnList$sd_sel[] <- NA

  } else if (mult_split_method == "DML") {
    # calculates the standard errors, p-values and confidence intervals using the "DML" method
    # introduced in Chernozhukov et al. (2018).
    stats_all <-
      lapply(seq_len(NCOL(Coef_all_matrix)),
             FUN = function(j) {get_DML_CI(Coef = Coef_all_matrix[, j],
                                            SE = sd_all_matrix[, j],
                                            level = 1 - alpha)})
    returnList$sd_all[] <- unlist(lapply(stats_all, FUN = function(x) x$se))
    returnList$pval_all[] <- unlist(lapply(stats_all, FUN = function(x) x$p_value))
    returnList$CI_all[] <- matrix(unlist(lapply(stats_all, FUN = function(x) x$CI)), nrow = 2)

    stats_sel <-
      lapply(seq_len(NCOL(Coef_sel_matrix)),
             FUN = function(j) {get_DML_CI(Coef = Coef_sel_matrix[, j],
                                           SE = sd_sel_matrix[, j],
                                           level = 1 - alpha)})
    returnList$sd_sel[] <- unlist(lapply(stats_sel, FUN = function(x) x$se))
    returnList$pval_sel[] <- unlist(lapply(stats_sel, FUN = function(x) x$p_value))
    returnList$CI_sel[] <- matrix(unlist(lapply(stats_sel, FUN = function(x) x$CI)), nrow = 2)
  } else {
    # if no valid inference method was selected, remove measures of uncertainty. Should never happen.
    returnList$sd_all[] <- rep(NA_real_, times = NCOL(sd_all_matrix))
    returnList$pval_all[] <- rep(NA_real_, times = NCOL(sd_all_matrix))
    returnList$CI_all[] <- matrix(NA_real_, nrow = 2, ncol = NCOL(Coef_all_matrix))
    returnList$sd_sel[] <- rep(NA_real_, times = NCOL(sd_sel_matrix))
    returnList$pval_sel[] <- rep(NA_real_, times = NCOL(sd_sel_matrix))
    returnList$CI_sel[] <- matrix(NA_real_, nrow = 2, ncol = NCOL(Coef_sel_matrix))
  }
  if (raw_output) {
    returnList$coef_all_raw <- Coef_all_matrix
    returnList$sd_all_raw <- sd_all_matrix
    returnList$coef_sel_raw <- Coef_sel_matrix
    returnList$sd_sel_raw <- sd_sel_matrix
  }
  returnList
}
