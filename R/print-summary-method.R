#' @exportS3Method
print.summary.tsci <- function(x, ...) {
  # this functions prints the most relevant statistics of ''summary.tsci' objects.

  # prints multi-splitting statistics.
  cat("\nStatistics about the data splitting procedure:\n")
  if (x$sample_size_A2 == 0) {
    cat(paste("Sample size:", x$sample_size_A1, "\n"))
    cat("No sample splitting was performed.\n")
  } else {
    cat(paste("Sample size A1:", x$sample_size_A1, "\n"))
    cat(paste("Sample size A2:", x$sample_size_A2, "\n"))
    cat(paste("Number of data splits:", x$n_splits, "\n"))
    cat(paste("Aggregation method:", x$mult_split_method, "\n"))
  }

  # prints invalidity test.
  cat("\nStatistics about the validity of the instrument(s):\n")
  print(x$invalidity)

  # prints treatment effect estimate.
  cat("\nTreatment effect estimate of selected violation space candidate(s):\n")
  coefficient_df <- data.frame(lapply(x$coefficient, FUN = function(y) if (is.numeric(y)) {round(y, 5)}  else {y}))
  rownames(coefficient_df) <- rownames(x$coefficient)
  colnames(coefficient_df) <- colnames(x$coefficient)
  print(coefficient_df)
  cat(paste("Selection method:", x$sel_method, "\n"))

  # if extended_output is TRUE, prints also the treatment effect estimates for each violation space candidate.
  if (x$extended_output) {
    cat("\nTreatment effect estimates of all violation space candidates:\n")
    coefficients_all_df <- data.frame(lapply(x$coefficients_all, FUN = function(y) if (is.numeric(y)) {round(y, 5)}  else {y}))
    rownames(coefficients_all_df) <- rownames(x$coefficients_all)
    colnames(coefficients_all_df) <- colnames(x$coefficients_all)
    print(coefficients_all_df)
  }

  # prints treatment model statistics.
  cat("\nStatistics about the treatment model:\n")
  cat(paste("Estimation method:", x$treatment_model$Estimation_Method, "\n"))

  # prints violation space selection statistics.
  cat("\nStatistics about the violation space selection:\n")
  print(x$viospace_selection)

  # if extended_output is TRUE; prints also IV strength statistics.
  if (x$extended_output) {
    cat("\nStatistics about the IV strength:\n")
    print(round(x$iv_strength, 2))
  }
  invisible(x)
}
