#' Two Stage Curvature Identification
#'
#' @description \code{tsci_fit} performs two stage curvature identification.
#' The function randomly splits the data in two sets \code{A1} and \code{A2}.
#' \code{A2} is used to fit the treatment model given the hyperparameters.
#' \code{A1} is used to fit the outcome model, identify the violation space and to estimate the treatment effect.
#'
#' @param df_treatment a data frame containing the subset of observations of the treatment model.
#' @param Y outcome vector with dimension n by 1.
#' @param D treatment vector with dimension n by 1.
#' @param Z instrument variable with dimension n by 1.
#' @param W (transformed) baseline covariates with dimension n by p_w used to fit the outcome model.
#' @param vio_space vio_space a matrix or a list.
#' @param intercept logical, including the intercept or not in the outcome model, default by TRUE.
#' @param sel_method The selection method used to estimate the treatment effect. Either "comparison" or "conservative".
#' @param sd_boot logical. if \code{TRUE}, it determines the standard error using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param split_prop split_prop numeric, proportion of observations used to fit the outcome model.
#' @param iv_threshold minimal value of the threshold of IV strength test, default by 10.
#' @param threshold_boot logical. if \code{TRUE}, it determines the threshold of the IV strength using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param alpha the significance level, default by 0.05.
#' @param params_grid a data frame containing the hyperparameter combinations of the treatment model fitting method.
#' @param function_hatmatrix a function to get the hat matrix of the treatment model.
#' @param B number of bootstrap samples.
#' @param ... additional arguments passed to \code{function_hatmatrix}.
#'
#' @return The output of \code{tsci_selection}
#'
#' @noRd
#'
tsci_fit <- function(df_treatment,
                     Y,
                     D,
                     Z,
                     W,
                     list_vio_space,
                     intercept,
                     sel_method,
                     sd_boot,
                     iv_threshold,
                     threshold_boot,
                     split_prop,
                     alpha,
                     params_grid,
                     function_hatmatrix,
                     B = B,
                     ...) {
  # this function randomly splits the data in two sets A1 and A2 and performs
  # two stage curvature identification.

  # splits data.
  n <- NROW(df_treatment)
  n_A1 <- round(split_prop * n)
  A1_ind <- sample(seq_len(n), n_A1)
  df_treatment_A1 <- df_treatment[A1_ind, ]
  df_treatment_A2 <- df_treatment[-A1_ind, ]

  # fits treatment model and gets hat matrix.
  model_treatment <- function_hatmatrix(
    df_treatment_A1 = df_treatment_A1,
    df_treatment_A2 = df_treatment_A2,
    params_grid = params_grid,
    ...
  )

  Y_A1 <- Y[A1_ind, ]
  D_A1 <- D[A1_ind, ]
  if (is.null(W)){
    W_A1 <- NULL
  } else {
    W_A1 <- W[A1_ind, ]
  }

  vio_space <- list_vio_space$vio_space[A1_ind, ]
  vio_ind <- list_vio_space$vio_ind
  Q <- list_vio_space$Q

  # estimates treatment effect using TSCI.
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
    weight = model_treatment$weight,
    intercept = intercept,
    sel_method = sel_method,
    sd_boot = sd_boot,
    iv_threshold = iv_threshold,
    threshold_boot = threshold_boot,
    alpha = alpha,
    B = B
  )
  outputs$mse[] <- model_treatment$mse

  return(outputs)
}
