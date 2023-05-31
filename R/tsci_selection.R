
#' Violation Space Selection
#'
#' @param Y outcome with dimension n by 1.
#' @param D treatment with dimension n by 1.
#' @param W (transformed) baseline covariates with dimension n by p_w used to fit the outcome model.
#' @param Y_A1 outcome with dimension n_A1 by 1.
#' @param D_A1 treatment with dimension n_A1 by 1.
#' @param W_A1 (transformed) baseline covariates with dimension n_A1 by p_w used to fit the outcome model.
#' @param vio_space the \code{matrix} of the largest violation space.
#' @param vio_ind a \code{list} containing the indices to obtain the violation space candidates
#' to test for (including the empty space).
#' @param Q the number of violation spaces (including the empty space).
#' @param weight n_A1 by n_A1 weight matrix.
#' @param intercept logic, to include an intercept in the outcome model or not.
#' @param sel_method The selection method used to estimate the treatment effect.
#' Either "comparison" or "conservative".
#' @param sd_boot logical. if \code{TRUE}, it determines the standard error using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param iv_threshold the minimal value of the threshold of IV strength test.
#' @param threshold_boot logical. if \code{TRUE}, it determines the threshold of the IV strength using a bootstrap approach.
#' If \code{FALSE}, it does not perform a bootstrap.
#' @param alpha alpha the significance level.
#' @param B number of bootstrap samples.
#'
#' @return
#' \describe{
#'     \item{\code{Coef_all}}{a series of point estimates of the treatment effect
#'     for the different violation space candidates.}
#'     \item{\code{sd_all}}{standard errors of Coef_all.}
#'     \item{\code{pval_all}}{p-values of the treatment effect estimates for the
#'     different violation space candidates.}
#'     \item{\code{CI_all}}{confidence intervals for the treatment effect for the
#'     different violation space candidates.}
#'     \item{\code{Coef_sel}}{the point estimator of the treatment effect for
#'     the selected violation space.}
#'     \item{\code{sd_sel}}{the standard error of Coef_sel.}
#'     \item{\code{pval_sel}}{p-value of the treatment effect estimate for the
#'     selected violation space.}
#'     \item{\code{CI_sel}}{confidence interval for the treatment effect for
#'     the selected violation space.}
#'     \item{\code{iv_str}}{IV strength for the different violation space candidates.}
#'     \item{\code{iv_thol}}{the threshold for the IV strength for the different violation space candidates.}
#'     \item{\code{Qmax}}{a named vector containing the number of times the
#'     violation space candidates were the largest acceptable violation space by
#'     the IV strength test.
#'     The value of the element named "weak_IV" is the number of times the instrument
#'     was too weak even for the empty violation space.}
#'     \item{\code{q_comp}}{a named vector containing the number of times the
#'     violation space candidates were the selected violation space by the comparison method.
#'     The value of the element named "weak_IV" is the number of times the instrument
#'     was too weak even for the empty violation space.}
#'     \item{\code{q_cons}}{a named vector containing the number of times the
#'     violation space candidates were the selected violation space by the conservative method.
#'     The value of the element named "weak_IV" is the number of times the instrument
#'     was too weak even for the empty violation space.}
#'     \item{\code{invalidity}}{a named vector containing the number of times
#'     the instrument was considered valid, invalid or too weak to test for violations.
#'     The instrument is considered invalid if the selected violation space is larger
#'     than the empty space.}
#' }
#' @noRd
#'
#' @importFrom stats coef lm.fit qnorm quantile resid rnorm var
tsci_selection <- function(Y,
                           D,
                           W,
                           Y_A1,
                           D_A1,
                           W_A1,
                           vio_space,
                           vio_ind,
                           Q,
                           weight,
                           intercept,
                           sel_method,
                           sd_boot,
                           iv_threshold,
                           threshold_boot,
                           alpha,
                           B) {
  # this function performs violation space selection and calculates the output statistics.
  # For better understanding what certain parts in the codes do (x) will refer to the
  # corresponding equation in Guo and Bühlmann (2022, https://doi.org/10.48550/arXiv.2203.12808)

  # adding a column to W ensures that W is always a matrix even if no covariates should
  # be included. This avoids case distinctions further down in the code.
  if (intercept) {
    W <- cbind(rep(1, NROW(Y)), W)
    W_A1 <- cbind(rep(1, NROW(Y_A1)), W_A1)
  } else {
    W <- cbind(rep(0, NROW(Y)), W)
    W_A1 <- cbind(rep(0, NROW(Y_A1)), W_A1)
  }

  # Cov_aug_A1 contains the columns that are used to approximate g(Z_i, X_i) in the outcome model (1).
  Cov_aug_A1 <- cbind(vio_space, W_A1)
  # this is needed to estimate the treatment effect (11).
  Y_rep <- as.matrix(weight %*% Y_A1)
  D_rep <- as.matrix(weight %*% D_A1)
  Cov_rep <- as.matrix(weight %*% Cov_aug_A1)
  n_A1 <- NROW(Y_rep)

  # initializes output list.
  output <- tsci_fit_NA_return(Q = Q)

  # initializes a vector for the treatment effect estimates for each violation space candidate.
  Coef_all <- rep(NA_real_, Q)

  # the noise of treatment model. Needed for the bias correction (12) and to estimate
  # the iv strength (17) and iv threshold (18).
  delta_hat <- D_A1 - D_rep
  SigmaSqD <- mean(delta_hat^2)

  # the noise of outcome model. Needed for the bias correction (12) and to calculate the
  # standard error of the treatment effect estimate (14).
  eps_hat <- vector(mode = "list", length = Q)

  # the position of the columns of W in Cov_aug_A1.
  pos_W <- seq(NCOL(vio_space) + 1, NCOL(Cov_aug_A1))
  ### fixed violation space, compute necessary inputs of selection part.
  D_resid <- diag_M_list <- rep(list(NA_real_), Q)
  for (index in seq_len(Q)) {
    # the first violation space candidate is always the empty space (i.e. assuming no violation).
    if (index == 1) pos_VW <- pos_W else pos_VW <- c(vio_ind[[index - 1]], pos_W)
    # the initial treatment effect estimate (11).
    reg_ml <- lm.fit(x = cbind(D_rep, Cov_rep[, pos_VW]), y = Y_rep)
    betaHat <- coef(reg_ml)[1]
    Coef_all[index] <- betaHat
    eps_hat[[index]] <- resid(lm.fit(x = as.matrix(Cov_aug_A1[, pos_VW]), y = Y_A1 - D_A1 * betaHat))
    # tsci_selection_stats returns the standard error of the trace of the
    # treatment effect estimate (14), D_resid used for the violation space selection (20, 23),
    # the estimated iv strength (17), the iv strength threshold (18) and the trace of M (11).
    stat_outputs <- tsci_selection_stats(D_rep = D_rep,
                                         Cov_rep = Cov_rep[, pos_VW],
                                         weight = weight,
                                         eps_hat = eps_hat[[index]],
                                         delta_hat = delta_hat,
                                         sd_boot = sd_boot,
                                         iv_threshold = iv_threshold,
                                         threshold_boot = threshold_boot,
                                         B = B)

    # the necessary statistics.
    output$sd_all[index] <- stat_outputs$sd
    output$iv_str[index] <- stat_outputs$iv_str
    output$iv_thol[index] <- stat_outputs$iv_thol
    D_resid[[index]] <- stat_outputs$D_resid
    diag_M_list[[index]] <- stat_outputs$diag_M
  }
  # residual sum of squares of D_rep~Cov_rep.
  # the denominator of the initial treatment effect estimator (11) used for several equations.
  D_RSS <- output$iv_str * SigmaSqD

  # violation space selection.
  # all of the q below are from 0 to (Q-1), so use q+1 to index the columns
  # checks for which violation space candidates the instruments are strong enough.
  ivtest_vec <- (output$iv_str >= output$iv_thol)
  if (sum(ivtest_vec) == 0) {
    # if even for the empty space the instruments are too weak than raise a warning.
    warning("Weak IV, even if the IV is assumed to be valid.")
    Qmax <- -1
  } else {
    Qmax <- sum(ivtest_vec) - 1
    if (Qmax == 0) {
      # if only for the empty space the instruments are strong enough than raise a warning as well.
      warning("Weak IV, if the IV is invalid. Testing for violations not possible.")
    }
  }

  # computes bias-corrected estimators (12).
  for (i in seq_len(Q)) {
    output$Coef_all[i] <- Coef_all[i] - sum(diag_M_list[[i]] * delta_hat * eps_hat[[i]]) / D_RSS[i]
  }

  # if IV test fails at q0 (empty space) or q1, we do not need to do selection.
  if (Qmax >= 1) {
    eps_Qmax <- eps_hat[[Qmax + 1]]
    Coef_Qmax <- rep(NA_real_, Q)
    for (i in seq_len(Q)) {
      # corresponds to (19)
      Coef_Qmax[i] <- Coef_all[i] - sum(diag_M_list[[i]] * delta_hat * eps_Qmax) / D_RSS[i]
    }

    ### Selection
    # defines comparison matrix (20).
    H <- beta_diff <- matrix(0, Qmax, Qmax)
    if (!sd_boot) {
      for (q1 in seq_len(Qmax) - 1) {
        for (q2 in (q1 + 1):(Qmax)) {
          H[q1 + 1, q2] <-
            as.numeric(sum((weight %*% D_resid[[q1 + 1]])^2 * eps_Qmax^2) / (D_RSS[q1 + 1]^2) +
                         sum((weight %*% D_resid[[q2 + 1]])^2 * eps_Qmax^2) / (D_RSS[q2 + 1]^2) -
                         2 * sum(eps_Qmax^2 * (weight %*% D_resid[[q1 + 1]]) * (weight %*% D_resid[[q2 + 1]])) /
                         (D_RSS[q1 + 1] * D_RSS[q2 + 1]))
        }
      }
      # computes beta difference matrix, uses Qmax.
      for (q in seq_len(Qmax) - 1) {
        beta_diff[q + 1, (q + 1):(Qmax)] <- abs(Coef_Qmax[q + 1] - Coef_Qmax[(q + 2):(Qmax + 1)]) # use bias-corrected estimator
      }
      # bootstrap for the quantile of the differences (23).
      eps_Qmax_cent <- as.vector(eps_Qmax - mean(eps_Qmax))
      eps_rep_matrix <- weight %*% (eps_Qmax_cent * matrix(rnorm(n_A1 * B), ncol = B))
      diff_mat <- matrix(0, Qmax, Qmax)
      max_val <-
        apply(eps_rep_matrix, 2,
              FUN = function(eps_rep) {
                for (q1 in seq_len(Qmax) - 1) {
                  for (q2 in (q1 + 1):(Qmax)) {
                    diff_mat[q1 + 1, q2] <- sum(D_resid[[q2 + 1]] * eps_rep) /
                      (D_RSS[q2 + 1]) - sum(D_resid[[q1 + 1]] * eps_rep) / (D_RSS[q1 + 1])
                  }
                }
                diff_mat <- abs(diff_mat) / sqrt(H)
                max(diff_mat, na.rm = TRUE)
              })
    } else {
      diff_mat_boo <- matrix(0, 0, B)
      u_matrix <- matrix(rnorm(n_A1 * B), ncol = B)
      eps_Qmax_cent <- as.vector(eps_Qmax - mean(eps_Qmax))
      eps_Qmax_boo <- u_matrix * eps_Qmax_cent
      for (q1 in seq_len(Qmax) - 1) {
        for (q2 in (q1 + 1):(Qmax)) {
          beta_q1_term1 <- t(D_resid[[q1 + 1]]) %*% weight %*% eps_Qmax_boo / D_RSS[q1 + 1]
          beta_q2_term1 <- t(D_resid[[q2 + 1]]) %*% weight %*% eps_Qmax_boo / D_RSS[q2 + 1]
          H[q1 + 1, q2] <- var(as.numeric(beta_q2_term1 - beta_q1_term1))
          # test: try to merge the calculation of diff_mat
          one_diff_boo <- abs(as.numeric(beta_q2_term1 - beta_q1_term1))
          diff_mat_boo <- rbind(diff_mat_boo, one_diff_boo)
        }
      }
      H_vec <- c(t(H))[c(t(H)) != 0]
      diff_mat_boo <- diff_mat_boo / sqrt(H_vec)
      max_val <- apply(diff_mat_boo, 2, max, na.rm=TRUE)
      # computes beta difference matrix, uses Qmax.
      for (q in seq_len(Qmax) - 1) {
        beta_diff[q + 1, (q + 1):(Qmax)] <- abs(Coef_Qmax[q + 1] - Coef_Qmax[(q + 2):(Qmax + 1)]) # use bias-corrected estimator
      }
    }
    # corresponds to (24).
    z_alpha <- 1.01 * quantile(max_val, 0.975)
    diff_thol <- z_alpha * sqrt(H)
    # comparison matrix (22).
    C_alpha <- ifelse(beta_diff <= diff_thol, 0, 1)


    # vector indicating the selection of each layer.
    sel_vec <- apply(C_alpha, 1, sum)
    if (all(sel_vec != 0)) {
      q_comp <- Qmax
    } else {
      q_comp <- min(which(sel_vec == 0)) - 1
    }
  } else {
    q_comp <- Qmax
  }

  # invalidity of TSLS.
  output$invalidity[] <- 0
  if (Qmax >= 1) {
    if (q_comp >= 1) {
      output$invalidity[2] <- 1
    } else {
      output$invalidity[1] <- 1
    }
  } else {
    q_comp <- Qmax
    output$invalidity[3] <- 1
  }

  # confidence intervals and p values for all violation spaces.
  output$CI_all[] <-
    rbind(output$Coef_all + qnorm(alpha / 2) * output$sd_all,
          output$Coef_all + qnorm(1 - alpha / 2) * output$sd_all)
  output$pval_all[] <-
    sapply(seq_len(length(output$Coef_all)),
      FUN = function(i) p_val(Coef = output$Coef_all[i], SE = output$sd_all[i], beta_test = 0)
    )

  # Even if the instrument is too weak, we still select the empty violation space.
  if (Qmax == -1) Qmax <- q_comp <- q_cons <- 0

  ### estimated violation space and corresponding estimator.
  output$Qmax[] <- 0
  output$q_comp[] <- 0
  output$q_cons[] <- 0
  q_cons <- min(q_comp + 1, Qmax)
  if (sel_method == "comparison") {
    output$Coef_sel[] <- output$Coef_all[q_comp + 1]
    output$sd_sel[] <- output$sd_all[q_comp + 1]
  } else if (sel_method == "conservative") {
    output$Coef_sel[] <- output$Coef_all[q_cons + 1]
    output$sd_sel[] <- output$sd_all[q_cons + 1]
  }

  output$Qmax[Qmax + 1] <- 1
  output$q_comp[q_comp + 1] <- 1
  output$q_cons[q_cons + 1] <- 1

  output$CI_sel[] <- rbind(
    output$Coef_sel + qnorm(alpha / 2) * output$sd_sel,
    output$Coef_sel + qnorm(1 - alpha / 2) * output$sd_sel
  )
  output$pval_sel[] <-
    sapply(seq_len(length(output$Coef_sel)),
      FUN = function(i) p_val(Coef = output$Coef_sel[i], SE = output$sd_sel[i], beta_test = 0)
    )
  output
}
