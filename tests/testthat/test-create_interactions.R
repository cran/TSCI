test_that("violation space candidates are correct if X = NULL", {
  n <- 10
  s <- 3
  df <- readRDS("df_create_vio_space_candidates.rds")
  Z <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  vio_space <- create_interactions(Z = Z)
  expect_equal(vio_space[[1]], Z)
  expect_equal(vio_space[[2]], cbind(Z[, 1] * Z[, 2],
                                     Z[, 1] * Z[, 3],
                                     Z[, 2] * Z[, 3]))
})

test_that("violation space candidates are correct if X not NULL", {
  n <- 10
  s <- 3
  df <- readRDS("df_create_vio_space_candidates.rds")
  V <- matrix(df$Z[seq_len(n * s)], nrow = n, ncol = s)
  Z <- V[, 1]
  X <- V[, -1]
  vio_space <- create_interactions(Z = Z, X = X)
  expect_equal(vio_space[[1]], as.matrix(Z))
  expect_equal(vio_space[[2]], cbind(Z * X[, 1],
                                     Z * X[, 2]))
})
