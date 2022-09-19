#' TSCI with multiple data splitting.
#'
#' @param df_treatment a data frame containing the subset of observations of the treatment model.
#' @param Y outcome vector with dimension n by 1.
#' @param D treatment vector with dimension n by 1.
#' @param Z instrument variable with dimension n by 1.
#' @param W (transformed) baseline covariates with dimension n by p_w used to fit the outcome model.
#' @param vio_space vio_space a matrix or a list.
#' @param create_nested_sequence logical. If \code{TRUE}, a nested sequence of violation space
#' candidates will be created.
#' @param intercept logical, including the intercept or not in the outcome model, default by TRUE.
#' @param sel_method The selection method used to estimate the treatment effect. Either "comparison" or "conservative".
#' @param iv_threshold minimal value of the threshold of IV strength test.
#' @param threshold_boot logical. if \code{TRUE}, it determines the threshold of the IV strength using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param alpha the significance level.
#' @param params_grid a data frame containing the hyperparameter combinations of the treatment model fitting method.
#' @param function_hatmatrix a function to get the hat matrix of the treatment model.
#' @param split_prop numeric, proportion of observations used to fit the outcome model.
#' @param nsplits numeric, number of times the data will be split.
#' @param parallel character, one out of \code{"no"}, \code{"multicore"}, or \code{"snow"} specifying the parallelization method used.
#' @param do_parallel logical, specifies if parallel computing should be performed.
#' @param ncores numeric, the number of cores used if multi_splitting is \code{TRUE}. \code{mclapply} form the package \code{parallel} will be called. Parallelization is not supported for Windows.
#' @param mult_split_method method to for inference if multi-splitting is performed. Either 'DML' or 'FWER'.
#' @param cl Either an parallel or snow cluster or \code{NULL}.
#' @param B number of bootstrap samples.
#'
#' @return
#'     \item{\code{Coef_all}}{the median over the multiple data splits of a series of point estimators of treatment effect corresponding to different violation spaces.}
#'     \item{\code{sd_all}}{standard errors of Coef_all.}
#'     \item{\code{CI_all}}{confidence intervals for the treatment effect corresponding to different violation spaces.}
#'     \item{\code{pval_all}}{p values for the treatment effect corresponding to different violation spaces.}
#'     \item{\code{Coef_sel}}{the median over the multiple data splits of the point estimator of the selected violation space.}
#'     \item{\code{sd_sel}}{the standard error of Coef_sel.}
#'     \item{\code{CI_sel}}{confidence intervals for the treatment effect of the selected violation space.}
#'     \item{\code{pval_sel}}{p value for the treatment effect of the selected violation space.}
#'     \item{\code{iv_str}}{the median over the multiple data splits of IV strength corresponding to different violation spaces.}
#'     \item{\code{iv_thol}}{the median over the multiple data splits of the threshold of IV strength test corresponding to different violation spaces.}
#'     \item{\code{Qmax}}{the median over the multiple data splits of the index of largest violation space selected by IV strength test. If -1, the IV strength test fails for the empty violation space.
#'     If 0, the IV Strength test fails for the first violation space. In other cases, violation space selection is performed.}
#'     \item{\code{q_comp}}{the median over the multiple data splits of the index of the selected violation space candidate by the comparison method.}
#'     \item{\code{q_cons}}{the median over the multiple data splits of the index of the selected violation space candidate by the conservative method.}
#'     \item{\code{invalidity}}{the number of data splits where the instrument was considered valid, invalid or too weak to test for violations.}
#' @noRd
#'
tsci_multisplit <- function(df_treatment,
                            Y,
                            D,
                            Z,
                            W,
                            vio_space,
                            create_nested_sequence,
                            intercept,
                            sel_method,
                            iv_threshold,
                            threshold_boot,
                            alpha,
                            params_grid,
                            function_hatmatrix,
                            split_prop,
                            parallel,
                            do_parallel,
                            nsplits,
                            ncores,
                            mult_split_method,
                            cl,
                            raw_output,
                            B = B) {
  # this function is used to perform multiple data splitting. It sets up a local
  # environment for the calculations for each data splits to handle potential
  # error and warning messages better.

  # merges the list vio_space into a matrix and identifies the columns to include
  # for each violation space candidate.
  list_vio_space <- build_vio_space_candidates(vio_space = vio_space,
                                               create_nested_sequence = create_nested_sequence)
  # if two violation space candidates lead to significant different estimates of
  # the treatment effect the algorithm will select the violation space candidate
  # that is further down the list. However, if the violation space candidates are not nested,
  # it is not clear which of the candidates covers the violation better.
  if (!(list_vio_space$nested_sequence))
    warning("Sequence of violation space candidates is not nested. Results should be interpreted with care.")

  # sets up local environment.
  tsci_parallel <- local({
    df_treatment
    Y
    D
    Z
    W
    list_vio_space
    intercept
    iv_threshold
    alpha
    params_grid
    function_hatmatrix
    ncores
    B
    function(colnames.cluster) {
      tryCatch_WEM(tsci_fit(
        df_treatment = df_treatment,
        Y = Y,
        D = D,
        Z = Z,
        W = W,
        list_vio_space = list_vio_space,
        intercept = intercept,
        sel_method = sel_method,
        iv_threshold = iv_threshold,
        threshold_boot = threshold_boot,
        split_prop = split_prop,
        alpha = alpha,
        params_grid = params_grid,
        function_hatmatrix = function_hatmatrix,
        B = B
      ), tsci_fit_NA_return(Q = list_vio_space$Q))}
  })

  # Performs calculations for each data split, check outputs for NAs and depending on the number of NAs performs a second round of data splits.
  if (do_parallel) {
    if (parallel == "multicore") {
      list_outputs <- parallel::mclapply(seq_len(nsplits), tsci_parallel, mc.cores = ncores)
    } else if (parallel == "snow") {
      if (is.null(cl)) {
        cl <- parallel::makePSOCKcluster(rep("localhost", ncores))
        parallel::clusterExport(cl, varlist = getNamespaceExports("TSML"))
        if (RNGkind()[1L] == "L'Ecuyer-CMRG")
          parallel::clusterSetRNGStream(cl)
        list_outputs <- parallel::parLapply(cl, seq_len(nsplits), tsci_parallel)
        parallel::stopCluster(cl)
        cl <- NULL # overwrite object which is responsible for the connection
      } else list_outputs <- parallel::parLapply(cl, seq_len(nsplits), tsci_parallel)
    }
  } else list_outputs <- lapply(seq_len(nsplits), tsci_parallel)

  check_list_outputs <- check_output(list_outputs = list_outputs, ind_start = 1)
  # if in more than 25% of the data splits the output statistics could not be calculated,
  # then an error is raised as there might be something systematically wrong.
  if (check_list_outputs$prop_na > 0.25) {
    stop(paste0("In more then 25% of the sample splits the output statistics could not be calculated.",
               check_list_outputs$error_string), call. = FALSE)
  }

  # if in less than 25% but at least in one data splits the output statistics could not be calculated,
  # perform another set of 0.5 * nsplits data splits.
  if (check_list_outputs$prop_na > 0) {
    nsplits_new <- ceiling(nsplits * 0.5)
    if (do_parallel) {
      if (parallel == "multicore") {
        list_outputs_new <- parallel::mclapply(seq_len(nsplits_new), tsci_parallel, mc.cores = ncores)
      } else if (parallel == "snow") {
        if (is.null(cl)) {
          cl <- parallel::makePSOCKcluster(rep("localhost", ncores))
          parallel::clusterExport(cl, varlist = getNamespaceExports("TSML"))
          if (RNGkind()[1L] == "L'Ecuyer-CMRG")
            parallel::clusterSetRNGStream(cl)
          list_outputs_new <- parallel::parLapply(cl, seq_len(nsplits_new), tsci_parallel)
          parallel::stopCluster(cl)
          cl <- NULL # overwrite object which is responsible for the connection
        } else list_outputs_new <- parallel::parLapply(cl, seq_len(nsplits_new), tsci_parallel)
      }
    } else list_outputs_new <- lapply(seq_len(nsplits_new), tsci_parallel)
    check_list_outputs_new <- check_output(list_outputs = list_outputs_new, ind_start = nsplits + 1)
    error_string <- paste0(check_list_outputs$error_string, check_list_outputs_new$error_string)
    if ((1 - check_list_outputs_new$prop_na) * nsplits_new < check_list_outputs$prop_na * nsplits) {
      stop(paste("Even after performing ",
                 nsplits + nsplits_new,
                 "sample splits there were still less than",
                 nsplits,
                 "for which the output statistics could be calculated.",
                 error_string), call. = FALSE)
    }
    pos_na <- which(check_list_outputs$ind_na)
    pos_stat <- which(!(check_list_outputs_new$ind_na))
    for(i in seq_len(length(pos_na))) {
      list_outputs[[pos_na[i]]] <- list_outputs_new[[pos_stat[i]]]
    }
    warning(paste0("In ",
                   sum(check_list_outputs$ind_na),
                   " of the ",
                   nsplits,
                   " data splits the output statistics could not be calculated.",
                   " Thus another ",
                   nsplits_new,
                   " data splits were performed.",
                   error_string), call. = FALSE)
  } else if (!is.null(check_list_outputs$error_string)) {
    warning(check_list_outputs$error_string)
  }

  # aggregates outputs of data splits.
  aggregate_output(output_list = list_outputs,
                   alpha = alpha,
                   Q = list_vio_space$Q,
                   mult_split_method = mult_split_method,
                   raw_output = raw_output)
}
