test_that("snapshot tsci_secondstage", {
  tol <- 10^-4
  df <- readRDS("df_tsci_functions.rds")
  Y <- df$Y
  D <- df$D
  Z <- df$Z
  X <- df$X
  vio_space <- create_monomials(Z, degree = 4, type = "monomials_main")
  A <- cbind(Z, Z^2, Z^3, Z^4, X)
  weight <- A %*% chol2inv(chol(t(A) %*% A)) %*% t(A)
  output <- withr::with_seed(seed = 1,
                             code = tsci_secondstage(Y = Y,
                                                     D = D,
                                                     Z = Z,
                                                     W = X,
                                                     vio_space = vio_space,
                                                     weight = weight,
                                                     B = 10),
                             .rng_kind = "L'Ecuyer-CMRG")
  expect_snapshot(output)
})
