test_that("snapshot tsci_boosting", {
  tol <- 10^-4
  df <- readRDS("df_tsci_functions.rds")
  Y <- df$Y
  D <- df$D
  Z <- df$Z
  X <- df$X
  vio_space <- create_monomials(Z, degree = 4, type = "monomials_main")
  output <- withr::with_seed(seed = 1,
                             code = tsci_boosting(Y = Y,
                                                  D = D,
                                                  Z = Z,
                                                  X = X,
                                                  vio_space = vio_space,
                                                  nsplits = 2),
                             .rng_kind = "L'Ecuyer-CMRG")
  expect_snapshot(output)

})
