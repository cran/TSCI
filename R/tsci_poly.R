#' Two Stage Curvature Identification with Polynomial Basis Expansion
#' @description \code{tsci_poly} implements Two Stage Curvature Identification
#' (Guo and Buehlmann 2022) with a basis expansion by monomials. Through a data-dependent way it
#' tests for the smallest sufficiently large violation space among a pre-specified
#' sequence of nested violation space candidates. Point and uncertainty estimates
#' of the treatment effect for all violation space candidates including the
#' selected violation space will be returned amongst other relevant statistics.
#'
#' @param Y observations of the outcome variable. Either a numeric vector of length n
#' or a numeric matrix with dimension n by 1.
#' If outcome variable is binary use dummy encoding.
#' @param D observations of the treatment variable. Either a numeric vector of length n
#' or a numeric matrix with dimension n by 1.
#' If treatment variable is binary use dummy encoding.
#' @param Z observations of the instrumental variable(s). Either a vector of length n
#' or a matrix with dimension n by s.
#' If observations are not numeric dummy encoding will be applied.
#' @param X observations of baseline covariate(s). Either a vector of length n
#' or a matrix with dimension n by p or \code{NULL}
#' (if no covariates should be included).
#' If observations are not numeric dummy encoding will be applied.
#' @param W (transformed) observations of baseline covariate(s) used to fit the outcome model. Either a vector of length n
#' or a matrix with dimension n by p_w or \code{NULL}
#' (if no covariates should be included).
#' If observations are not numeric dummy encoding will be applied.
#' @param vio_space either \code{NULL} or a list with numeric vectors of length n and/or numeric matrices with n rows as elements to
#' specify the violation space candidates.
#' If observations are not numeric dummy encoding will be applied.
#' See Details for more information.
#' If \code{NULL}, then the violation space candidates are chosen to be a nested sequence
#' of monomials with degree depending on the orders of the polynomials used to fit
#' the treatment model.
#' @param create_nested_sequence logical. If \code{TRUE}, the violation space candidates (in form of matrices)
#' are defined sequentially starting with an empty violation matrix and subsequently
#' adding the next element of \code{vio_space} to the current violation matrix.
#' If \code{FALSE,} the violation space candidates (in form of matrices) are defined as the empty space and the elements of \code{vio_space}.
#' See Details for more information.
#' @param sel_method The selection method used to estimate the treatment effect. Either "comparison" or "conservative". See Details.
#' @param min_order either a single integer value or a vector of integer values of length s specifying
#' the smallest order of polynomials to use in the selection of the treatment model. If a
#' single integer value is provided, the polynomials of all instrumental variables use this value.
#' @param max_order either a single integer value or a vector of integer values of length s specifying
#' the largest order of polynomials to use in the selection of the treatment model. If a
#' single integer value is provided, the polynomials of all instrumental variables use this value.
#' @param exact_order either a single integer value or a vector of integer values of length s specifying
#' the exact order of polynomials to use in the treatment model. If a
#' single integer value is provided, the polynomials of all instrumental variables use this value.
#' @param order_selection_method method used to select the best fitting order of polynomials
#' for the treatment model. Must be either 'grid search' or 'backfitting'.
#' 'grid search' can be very slow if the number of instruments is large.
#' @param max_iter number of iterations used in the backfitting algorithm if \code{order_selection_method} is 'backfitting'.
#' Has to be a positive integer value.
#' @param conv_tol tolerance of convergence in the backfitting algorithm if \code{order_selection_method} is 'backfitting'.
#' @param gcv logical. If \code{TRUE}, the generalized cross-validation mean squared error is used
#' to determine the best fitting order of polynomials for the treatment model.
#' If \code{FALSE}, k-fold cross-validation is used instead.
#' @param nfolds number of folds used for the k-fold cross-validation if \code{gcv} is \code{FALSE}.
#' Has to be a positive integer value.
#' @param sd_boot logical. if \code{TRUE}, it determines the standard error using a bootstrap approach.
#' @param iv_threshold a numeric value specifying the minimum of the threshold of IV strength test.
#' @param threshold_boot logical. if \code{TRUE}, it determines the threshold of the IV strength using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap. See Details.
#' @param alpha the significance level. Has to be a numeric value between 0 and 1.
#' @param intercept logical. If \code{TRUE}, an intercept is included in the outcome model.
#' @param B number of bootstrap samples. Has to be a positive integer value.
#' Bootstrap methods are used to calculate the iv strength threshold if \code{threshold_boot} is \code{TRUE} and for the violation space selection.
#'
#' @return
#' A list containing the following elements:
#' \describe{
#'     \item{\code{Coef_all}}{a series of point estimates of the treatment effect
#'     obtained by the different violation space candidates.}
#'     \item{\code{sd_all}}{standard errors of the estimates of the treatmnet effect
#'     obtained by the different violation space candidates.}
#'     \item{\code{pval_all}}{p-values of the treatment effect estimates obtained by the
#'     different violation space candidates.}
#'     \item{\code{CI_all}}{confidence intervals for the treatment effect obtained by the
#'     different violation space candidates.}
#'     \item{\code{Coef_sel}}{the point estimator of the treatment effect obtained by
#'     the selected violation space candidate(s).}
#'     \item{\code{sd_sel}}{the standard error of Coef_sel.}
#'     \item{\code{pval_sel}}{p-value of the treatment effect estimate obtained by the
#'     selected violation space candidate(s).}
#'     \item{\code{CI_sel}}{confidence interval for the treatment effect obtained by
#'     the selected violation space candidate(s).}
#'     \item{\code{iv_str}}{IV strength using the different violation space candidates.}
#'     \item{\code{iv_thol}}{the threshold for the IV strength using the different violation space candidates.}
#'     \item{\code{Qmax}}{the violation space candidate that was the largest violation space candidate
#'     for which the IV strength was considered large enough determined by the IV strength test.
#'     If 0, the IV Strength test failed for the first violation space candidate.
#'     Otherwise, violation space selection was performed.}
#'     \item{\code{q_comp}}{the violation space candidate that was selected by the comparison method over the multiple data splits.}
#'     \item{\code{q_cons}}{the violation space candidate that was selected by the conservative method over the multiple data splits.}
#'     \item{\code{invalidity}}{shows whether the instrumental variable(s) were considered valid, invalid or too weak to test for violations.
#'     The instrumental variables are considered too weak to test for violations if the IV strength is already too weak using the first
#'     violation space candidate (besides the empty violation space). Testing for violations is always performed by using the comparison method.}
#'     \item{\code{mse}}{the out-of-sample mean squared error of the treatment model.}
#'}
#'
#' @details The treatment and outcome models are assumed to be of the following forms:
#' \deqn{D_i = f(Z_i, X_i) + \delta_i}
#' \deqn{Y_i = \beta \cdot D_i + h(Z_i, X_i) + \phi(X_i) + \epsilon_i}
#' where \eqn{f(Z_i, X_i)} is estimated using a polynomial basis expansion of the instrumental variables
#' and a linear combination of the baseline covariates,
#' \eqn{h(Z_i X_i)} is approximated using the violation space candidates and \eqn{\phi(X_i)} is approximated by
#' a linear combination of the columns in \code{W}. The errors are allowed to be heteroscedastic. \cr \cr
#' The violation space candidates should be in a nested sequence as the violation space selection is performed
#' by comparing the treatment estimate obtained by each violation space candidate with the estimates of all
#' violation space candidates further down the list \code{vio_space} that provide enough IV strength. Only if no
#' significant difference was found in all of those comparisons, the violation space
#' candidate will be selected. If \code{sel_method} is 'comparison', the treatment effect estimate of this
#' violation space candidate will be returned. If \code{sel_method} is 'conservative', the treatment effect estimate
#' of the successive violation space candidate will be returned provided that the IV strength is large enough.
#' If \code{vio_space} is \code{NULL} the violation space candidates are chosen to be a nested sequence
#' of polynomials of the instrumental variables up to the degrees used to fit the treatment model.
#' This guarantees that the possible spaces of the violation will be tested.
#' If the functional form of the outcome model is not well-known it is advisable to use the default values
#' for \code{W} and \code{vio_space}. \cr \cr
#' The instrumental variable(s) are considered strong enough for violation space candidate \eqn{V_q} if the estimated IV strength using this
#' violation space candidate is larger than the obtained value of the threshold of the IV strength.
#' The formula of the threshold of the IV strength has the form
#' \eqn{\min \{\max \{ 2 \cdot \mathrm{Trace} [ \mathrm{M} (V_q) ], \mathrm{iv{\_}threshold} \} + S (V_q), 40 \}} if \code{threshold_boot} is \code{TRUE}, and
#' \eqn{\min \{\max \{ 2 \cdot \mathrm{Trace} [ \mathrm{M} (V_q) ], \mathrm{iv{\_}threshold} \}, 40 \}} if \code{threshold_boot} is \code{FALSE}. The matrix
#' \eqn{\mathrm{M} (V_q)} depends on the hat matrix obtained from estimating \eqn{f(Z_i, X_i)}, the violation space candidate \eqn{V_q} and
#' the variables to include in the outcome model \code{W}. \eqn{S (V_q)} is obtained using a bootstrap and aims to adjust for the estimation error
#' of the IV strength.
#' Usually, the value of the threshold of the IV strength obtained using the bootstrap approach is larger.
#' Thus, using \code{threshold_boot} equals \code{TRUE} leads to a more conservative IV strength test.
#' For more information see subsection 3.3 in Guo and Buehlmann (2022). \cr \cr
#' See also Carl et al. (2023) for more details.
#'
#' @references
#' \itemize{
#' \item{Zijian Guo, and Peter Buehlmann. Two Stage Curvature Identification with
#' Machine Learning: Causal Inference with Possibly Invalid Instrumental Variables.
#' \emph{arXiv:2203.12808}, 2022}
#' \item{David Carl, Corinne Emmenegger, Peter Buehlmann, and Zijian Guo. TSCI:
#' two stage curvature identification for causal inference with invalid instruments.
#' \emph{arXiv:2304.00513}, 2023}
#' }
#'
#' @seealso
#' \code{\link[TSCI]{tsci_forest}} for TSCI with random forest. \cr \cr
#' \code{\link[TSCI]{tsci_boosting}} for TSCI with boosting. \cr \cr
#' \code{\link[TSCI]{tsci_secondstage}} for TSCI with user provided hat matrix. \cr \cr
#'
#' @export
#'
#' @examples
#' ### a small example without baseline covariates
#' if (require("MASS")) {
#'   # sample size
#'   n <- 100
#'   # the IV strength
#'   a <- 1
#'   # the violation strength
#'   tau <- 1
#'   # true effect
#'   beta <- 1
#'   # treatment model
#'   f <- function(x) {1 + a * (x + x^2)}
#'   # outcome model
#'   g <- function(x) {1 + tau * x}
#'
#'   # generate data
#'   mu_error <- rep(0, 2)
#'   Cov_error <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#'   Error <- MASS::mvrnorm(n, mu_error, Cov_error)
#'   # instrumental variable
#'   Z <- rnorm(n)
#'   # treatment variable
#'   D <- f(Z) + Error[, 1]
#'   # outcome variable
#'   Y <- beta * D + g(Z) + Error[, 2]
#'
#'   # Two Stage Polynomials
#'   output_PO <- tsci_poly(Y, D, Z, max_order = 3, max_iter = 20, B = 100)
#'   summary(output_PO)
#' }
tsci_poly <- function(Y,
                      D,
                      Z,
                      X = NULL,
                      W = X,
                      vio_space = NULL,
                      create_nested_sequence = TRUE,
                      sel_method = c("comparison", "conservative"),
                      min_order = 1,
                      max_order = 10,
                      exact_order = NULL,
                      order_selection_method = c("grid search", "backfitting"),
                      max_iter = 100,
                      conv_tol = 10^-6,
                      gcv = FALSE,
                      nfolds = 5,
                      sd_boot = TRUE,
                      iv_threshold = 10,
                      threshold_boot = TRUE,
                      alpha = 0.05,
                      intercept = TRUE,
                      B = 300) {
  # encodes categorical variables to dummy variables.
  ls_encoded <- dummy_encoding(Y = Y,
                               D = D,
                               Z = Z,
                               X = X,
                               W = W,
                               vio_space = vio_space)
  Y <- ls_encoded$Y
  D <- ls_encoded$D
  Z <- ls_encoded$Z
  X <- ls_encoded$X
  W <- ls_encoded$W
  vio_space <- ls_encoded$vio_space
  # checks that input is in the correct format.
  check_input(Y = Y,
              D = D,
              Z = Z,
              X = X,
              W = W,
              vio_space = vio_space,
              create_nested_sequence = create_nested_sequence,
              intercept = intercept,
              min_order = min_order,
              max_order = max_order,
              exact_order = exact_order,
              max_iter = max_iter,
              conv_tol = conv_tol,
              gcv = gcv,
              nfolds = nfolds,
              iv_threshold = iv_threshold,
              sd_boot = sd_boot,
              threshold_boot = threshold_boot,
              alpha = alpha,
              B = B,
              tsci_method = "poly")
  order_selection_method <- match.arg(order_selection_method)
  sel_method <- match.arg(sel_method)

  n <- NROW(Y)
  p <- NCOL(Z) + ifelse(is.null(X), 0, NCOL(X))

  # grid search.
  if (is.null(exact_order)) {
    if (length(max_order) == 1) max_order <- rep(max_order, NCOL(Z))
    if (length(min_order) == 1) min_order <- rep(min_order, NCOL(Z))
  } else if (!is.null(exact_order)) {
    if (length(exact_order) == 1) {
      max_order <- rep(exact_order, NCOL(Z))
      min_order <- rep(exact_order, NCOL(Z))
    } else {
      max_order <- exact_order
      min_order <- exact_order
    }
  }

  if (!is.null(X)) {
    max_order <- c(max_order, rep(1, NCOL(X)))
    min_order <- c(min_order, rep(1, NCOL(X)))
  }

  params_list <- lapply(seq_len(p),
                        FUN = function(i) seq(as.integer(min_order[i]), as.integer(max_order[i]), by = 1))


  # creates dataframe for the treatment model.
  df_treatment <- data.frame(cbind(D, Z, X))
  names(df_treatment) <- c("D", paste("B", seq_len(p), sep = ""))

  # selection of the best fitting order of the polynomials. For tsci_poly this step
  # is already performed here as it influences the choice of the violation space candidates.
  poly_CV <- get_poly_parameters(df_treatment = df_treatment,
                                 params_list = params_list,
                                 order_selection_method = order_selection_method,
                                 max_iter = as.integer(max_iter),
                                 conv_tol = conv_tol,
                                 gcv = gcv,
                                 nfolds = as.integer(nfolds))


  if (is.null(vio_space)) {
    vio_space <- create_monomials(Z = Z,
                                  degree = unlist(poly_CV$params)[seq_len(NCOL(Z))],
                                  type = "monomials_main")
    create_nested_sequence <- TRUE
  }

  list_vio_space <- build_vio_space_candidates(vio_space = vio_space,
                                               create_nested_sequence = create_nested_sequence)

  # if two violation space candidates lead to significant different estimates of
  # the treatment effect the algorithm will select the violation space candidate
  # that is further down the list. However, if the violation space candidates are not nested
  # it is not clear which of the candidates covers the violation better. Thus, a warning is raised.
  if (!(list_vio_space$nested_sequence))
    warning("Sequence of violation space candidates is not nested. Results should be interpreted with care.")

  # calculates the hat matrix for A1, performs violation space selection and estimates
  # the treatment effect.
  outputs <- tsci_fit(df_treatment = df_treatment,
                      Y = Y,
                      D = D,
                      Z = Z,
                      W = W,
                      list_vio_space = list_vio_space,
                      intercept = intercept,
                      sel_method = sel_method,
                      sd_boot = sd_boot,
                      iv_threshold = iv_threshold,
                      threshold_boot = threshold_boot,
                      split_prop = 1,
                      alpha = alpha,
                      params_grid = poly_CV$params,
                      function_hatmatrix = get_poly_hatmatrix,
                      B = as.integer(B),
                      mse = poly_CV$mse)

  # returns output.
  outputs <- append(outputs,
                    list(FirstStage_model = "OLS with Polynomials",
                         n_A1 = n,
                         n_A2 = 0,
                         nsplits = 0,
                         mult_split_method = "No sample splitting was performed",
                         alpha = alpha,
                         sel_method = sel_method))
  class(outputs) <- c("tsci", "list")
  return(outputs)
}
