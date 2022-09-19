test_that("vector containing NA's results in error", {
  expect_error(get_hat_matrix_of_tree(c("A", NA, "A")))
})
