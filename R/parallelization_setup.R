#' Parallelization Setup
#'
#' @description This function pre-processes information about parallel computing
#' and outputs an error if the computing platform does not support the parallelization method.
#'
#' @param parallel One out of \code{"no"}, \code{"multicore"}, or \code{"snow"} specifying the parallelization method used.
#' @param ncpus An integer specifying the number of cores used if \code{parallel} is not set to \code{"no"}.
#' @param cl Either an parallel or snow cluster or \code{NULL}.
#'
#' @return logical, an input of the function \code{multi_split}. Specifies if the outputs for each data split should be calculated in parallel or serial.
#' @noRd
parallelization_setup <- function(parallel, ncpus, cl) {
  # this function checks if parallel computing should be performed.
  do_parallel <- ((parallel != "no" && ncpus > 1L) ||
    (parallel == "snow" && !is.null(cl)))
  if (do_parallel &&
    parallel == "multicore" &&
    .Platform$OS.type == "windows") {
    stop("The argument parallel = 'multicore' is not available for windows.
    Use parallel = 'snow' for parallel execution or
         parallel = 'no' for serial execution of the code.")
  }

  do_parallel
}
