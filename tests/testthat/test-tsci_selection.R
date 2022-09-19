test_that("correct output for valid input", {
  df <- readRDS("df_tsci_selection.rds")
  Y <- df$Y
  D <- df$D
  Z <- df$Z
  X <- df$X
  mu_D <- df$mu_D
  fit <- lm(Y ~ mu_D + Z + X - 1)
  vio_space <- cbind(Z, Z^2, Z^3)
  vio_ind <- list(1, c(1:2), c(1:3))
  Q <- 4
  A <- cbind(Z, Z^2, X)
  weight <- A %*% chol2inv(chol(t(A) %*% A)) %*% t(A)
  output <- tsci_selection(Y = Y,
                           D = D,
                           W = X,
                           Y_A1 = Y,
                           D_A1 = D,
                           W_A1 = X,
                           vio_space = vio_space,
                           vio_ind = vio_ind,
                           Q = Q,
                           weight = weight,
                           intercept = FALSE,
                           sel_method = "comparison",
                           iv_threshold = 10,
                           threshold_boot = FALSE,
                           alpha = 0.05,
                           B = 300)
  expect_lt(abs(output$Coef_sel - fit$coefficients[1]), 0.1)
  expect_lt(mean(abs(output$CI_sel[, 1] - confint(fit, parm = "mu_D"))), 0.1)
  expect_equal(unname(output$Qmax), c(0, 1, 0, 0))
  expect_equal(unname(output$q_comp), c(0, 1, 0, 0))
})
