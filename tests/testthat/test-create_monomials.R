test_that("violation space candidates are correct for 'monomials_main' for s = 1", {
  n <- 10
  s <- 1
  df <- readRDS("df_create_vio_space_candidates.rds")
  Z <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  degree <- 3
  vio_space <- create_monomials(Z = Z, degre = degree, type = "monomials_main")
  expect_equal(vio_space[[1]], Z)
  expect_equal(vio_space[[2]], Z^2)
  expect_equal(vio_space[[3]], Z^3)
})

test_that("violation space candidates are correct for 'monomials_full' for s = 1", {
  n <- 10
  s <- 1
  df <- readRDS("df_create_vio_space_candidates.rds")
  Z <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  degree <- 3
  vio_space <- create_monomials(Z = Z, degre = degree, type = "monomials_full")
  expect_equal(vio_space[[1]], Z)
  expect_equal(vio_space[[2]], Z^2)
  expect_equal(vio_space[[3]], Z^3)
})

test_that("violation space candidates are correct for 'monomials_main' for s > 1", {
  n <- 10
  s <- 3
  df <- readRDS("df_create_vio_space_candidates.rds")
  Z <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  degree <- c(3, 1, 2)
  vio_space <- create_monomials(Z = Z, degre = degree, type = "monomials_main")
  expect_equal(vio_space[[1]], Z)
  expect_equal(vio_space[[2]], cbind(Z[, 1]^2, Z[, 3]^2))
  expect_equal(vio_space[[3]], Z[, 1, drop = FALSE]^3)
})

test_that("violation space candidates are correct for 'monomials_full' for s > 1", {
  n <- 10
  s <- 3
  df <- readRDS("df_create_vio_space_candidates.rds")
  Z <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  degree <- c(3, 1, 2)
  vio_space <- create_monomials(Z = Z, degre = degree, type = "monomials_full")
  actual <- vio_space[[1]][, order(colMeans(vio_space[[1]]))]
  expected <- Z
  expected <- expected[, order(colMeans(expected))]
  expect_equal(actual, expected)
  actual <- vio_space[[2]][, order(colMeans(vio_space[[2]]))]
  expected <- cbind(Z[, 1]^2, Z[, 1]*Z[, 2], Z[, 1]*Z[, 3], Z[, 2]*Z[, 3], Z[, 3]^2)
  expected <- expected[, order(colMeans(expected))]
  expect_equal(actual, expected)
  actual <- vio_space[[3]][, order(colMeans(vio_space[[3]]))]
  expected <- cbind(Z[, 1]^3, Z[, 1]^2 * Z[, 2], Z[, 1]^2 * Z[, 3],
                    Z[, 1] * Z[, 2] * Z[, 3], Z[, 1] * Z[, 3]^2, Z[, 2] * Z[, 3]^2)
  expected <- expected[, order(colMeans(expected))]
  expect_equal(actual, expected)
})

