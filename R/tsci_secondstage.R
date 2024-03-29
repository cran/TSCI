#' Two Stage Curvature Identification with User Provided Hat Matrix
#' @description \code{tsci_secondstage} implements Two Stage Curvature Identification
#' (Guo and Buehlmann 2022) for a user-provided hat matrix. Through a data-dependent way it
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
#' @param W (transformed) observations of baseline covariate(s) used to fit the outcome model. Either a vector of length n
#' or a matrix with dimension n by p_w or \code{NULL}
#' (if no covariates should be included).
#' If observations are not numeric dummy encoding will be applied.
#' @param vio_space  list with vectors of length n and/or matrices with n rows as elements to
#' specify the violation space candidates.
#' If observations are not numeric dummy encoding will be applied.
#' See Details for more information.
#' @param create_nested_sequence logical. If \code{TRUE}, the violation space candidates (in form of matrices)
#' are defined sequentially starting with an empty violation matrix and subsequently
#' adding the next element of \code{vio_space} to the current violation matrix.
#' If \code{FALSE,} the violation space candidates (in form of matrices) are defined as the empty space and the elements of \code{vio_space}.
#' See Details for more information.
#' @param weight the hat matrix of the treatment model.
#' @param A1_ind indices of the observations that wil be used to fit the outcome model.
#' Must be of same length as the number of rows and columns of \code{weight}.
#' If \code{NULL}, all observations will be used.
#' @param sel_method The selection method used to estimate the treatment effect. Either "comparison" or "conservative". See Details.
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
#'}
#'
#' @details The treatment and outcome models are assumed to be of the following forms:
#' \deqn{D_i = f(Z_i, X_i) + \delta_i}
#' \deqn{Y_i = \beta \cdot D_i + h(Z_i, X_i) + \phi(X_i) + \epsilon_i}
#' where \eqn{f(Z_i, X_i)} is estimated using a random forest,
#' \eqn{h(Z_i X_i)} is approximated using the hat matrix \code{weight} provided by the user and
#' \eqn{\phi(X_i)} is approximated by a linear combination of the columns in \code{W}.
#' The errors are allowed to be heteroscedastic. \eqn{A1} is used to perform violation space selection
#' and to estimate the treatment effect. \cr \cr
#' The violation space candidates should be in a nested sequence as the violation space selection is performed
#' by comparing the treatment estimate obtained by each violation space candidate with the estimates of all
#' violation space candidates further down the list \code{vio_space} that provide enough IV strength. Only if no
#' significant difference was found in all of those comparisons, the violation space
#' candidate will be selected. If \code{sel_method} is 'comparison', the treatment effect estimate of this
#' violation space candidate will be returned. If \code{sel_method} is 'conservative', the treatment effect estimate
#' of the successive violation space candidate will be returned provided that the IV strength is large enough.
#' The specification of suitable violation space candidates is a crucial step because a poor approximation
#' of \eqn{g(Z_i, X_i)} might not address the bias caused by the violation of the IV assumption sufficiently well.
#' The function \code{\link[TSCI]{create_monomials}} can be used to create a predefined sequence of
#' violation space candidates consisting of monomials.
#' The function \code{\link[TSCI]{create_interactions}} can be used to create a predefined sequence of
#' violation space candidates consisting of two-way interactions between the instrumens themselves and between
#' the instruments and the instruments and baseline covariates.  \cr \cr
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
#' For more information see subsection 3.3 in Guo and Buehlmann (2022).\cr \cr
#' See also Carl et al. (2023) for more details.
#'
#' @references
#' \itemize{
#' \item{Zijian Guo, and Peter Buehlmann. Two Stage Curvature Identification with
#' Machine Learning: Causal Inference with Possibly Invalid Instrumental Variables.
#' \emph{arXiv:2203.12808}, 2022}
#' \item{Nicolai Meinshausen, Lukas Meier, and Peter Buehlmann. P-values for high-dimensional
#' regression. \emph{Journal of the American Statistical Association},
#' 104(488):1671-1681, 2009. 16, 18}
#' \item{Victor Chernozhukov, Denis Chetverikov, Mert Demirer, Esther Duflo, Christian Hansen,
#' Whitney Newey, and James Robins. Double/debiased machine learning for treatment
#' and structural parameters: Double/debiased machine learning.
#' \emph{The Econometrics Journal}, 21(1), 2018. 4, 16, 18}
#' \item{David Carl, Corinne Emmenegger, Peter Buehlmann, and Zijian Guo. TSCI:
#' two stage curvature identification for causal inference with invalid instruments.
#' \emph{arXiv:2304.00513}, 2023}
#' }
#'
#' @seealso
#' \code{\link[TSCI]{tsci_boosting}} for TSCI with boosting. \cr \cr
#' \code{\link[TSCI]{tsci_forest}} for TSCI with random forest. \cr \cr
#' \code{\link[TSCI]{tsci_poly}} for TSCI with polynomial basis expansion. \cr \cr
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
#'   # Two Stage User Defined
#'   # get hat matrix of treatment model
#'   A <- cbind(1, Z, Z^2, Z^3)
#'   weight <- A %*% chol2inv(chol(t(A) %*% A)) %*% t(A)
#'   # create violation space candidates
#'   vio_space <- create_monomials(Z, 2, "monomials_main")
#'   # perform two stage curvature identification
#'   output_UD <- tsci_secondstage(Y, D, Z, vio_space = vio_space, weight = weight,
#'                                 B = 100)
#'   summary(output_UD)
#' }
#' @importFrom stats coef lm qnorm quantile resid rnorm
tsci_secondstage <- function(Y,
                             D,
                             Z,
                             W = NULL,
                             vio_space,
                             create_nested_sequence = TRUE,
                             weight,
                             A1_ind = NULL,
                             sel_method = c("comparison", "conservative"),
                             sd_boot = TRUE,
                             iv_threshold = 10,
                             threshold_boot = TRUE,
                             alpha = 0.05,
                             intercept = TRUE,
                             B = 300) {
  sel_method <- match.arg(sel_method)
  # encodes categorical variables to dummy variables.
  ls_encoded <- dummy_encoding(Y = Y,
                               D = D,
                               Z = Z,
                               X = NULL,
                               W = W,
                               vio_space = vio_space)
  Y <- ls_encoded$Y
  D <- ls_encoded$D
  Z <- ls_encoded$Z
  W <- ls_encoded$W
  vio_space <- ls_encoded$vio_space
  # check that input is in the correct format.
  check_input(Y = Y,
              D = D,
              Z = Z,
              X = NULL,
              W = W,
              vio_space = vio_space,
              create_nested_sequence = create_nested_sequence,
              weight = weight,
              A1_ind = A1_ind,
              intercept = intercept,
              sd_boot = sd_boot,
              iv_threshold = iv_threshold,
              threshold_boot = threshold_boot,
              alpha = alpha,
              B = B,
              tsci_method = "user defined")

  if (is.null(A1_ind)) A1_ind <- seq_len(NROW(Y))

  n <- NROW(Y)
  n_A1 <- length(A1_ind)

  list_vio_space <- build_vio_space_candidates(vio_space = vio_space,
                                               create_nested_sequence = create_nested_sequence)

  # if two violation space candidates lead to significant different estimates of
  # the treatment effect the algorithm will select the violation space candidate
  # that is further down the list. However, if the violation space candidates are not nested,
  # it is not clear which of the candidates covers the violation better.
  if (!(list_vio_space$nested_sequence))
    warning("Sequence of violation space candidates is not nested. Results should be interpreted with care.")

  vio_space <- list_vio_space$vio_space[A1_ind, ]
  vio_ind <- list_vio_space$vio_ind
  Q <- list_vio_space$Q

  Y_A1 <- Y[A1_ind, ]
  D_A1 <- D[A1_ind, ]
  if (is.null(W)){
    W_A1 <- NULL
  } else {
    W_A1 <- W[A1_ind, ]
  }

  outputs <- tsci_selection(
    Y = Y,
    D = D,
    W = W,
    Y_A1 = Y_A1,
    D_A1 = D_A1,
    W_A1 = W_A1,
    vio_space = vio_space,
    vio_ind = vio_ind,
    Q = Q,
    weight = weight,
    intercept = intercept,
    sel_method = sel_method,
    sd_boot = sd_boot,
    iv_threshold = iv_threshold,
    threshold_boot = threshold_boot,
    alpha = alpha,
    B = B
  )
  outputs$mse[] <- NA
  outputs <- append(outputs,
                    list(FirstStage_model = "Specified by User",
                         n_A1 = n_A1,
                         n_A2 = n - n_A1,
                         nsplits = 0,
                         mult_split_method = "No sample splitting was performed",
                         alpha = alpha,
                         sel_method = sel_method
                         ))
  class(outputs) <- c("tsci", "list")
  return(outputs)
}
