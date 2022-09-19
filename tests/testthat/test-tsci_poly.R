test_that("snapshot tsci_poly", {
  tol <- 10^-4
  df <- readRDS("df_tsci_functions.rds")
  Y <- df$Y
  D <- df$D
  Z <- df$Z
  X <- df$X
  output <- withr::with_seed(seed = 1,
                   code = tsci_poly(Y = Y,
                                    D = D,
                                    Z = Z,
                                    X = X),
                   .rng_kind = "L'Ecuyer-CMRG")
  expect_snapshot(output)

})
