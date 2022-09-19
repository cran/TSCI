#' Error and warning message handling for parallel computation
#'
#' @description concept found on
#' - the help file of tryCatch; see demo(error.catching).
#' - R help: https://stat.ethz.ch/pipermail/r-help/2010-December/262626.html
#' - package simsalapar: R/tryCatchWE.R function tryCatch.W.E() and R/doCallWE.R
#'   with function doCallWE()
#' - https://stackoverflow.com/questions/4948361
#' We would also like to thank Martin Maechler -- who also added 'message handling'
#' to be used e.g. for catching  lmer() messages {for C.Emmenegger's dblmlMixed pkg}
#'
#' @param expr expression to be evaluated.
#' @param ret.obj ret.obj return argument ret.obj (input) if an error occcurs.
#'
#' @return a list with value, error warning and message.
#' @noRd
#'
tryCatch_WEM <- function(expr, ret.obj) {
  # warning handler
  warhandler <- function(w) {
    warn <<- append(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  # message handler
  msghandler <- function(m) {
    msg <<- append(msg, conditionMessage(m))
    invokeRestart("muffleMessage")
  }

  # error handler
  errhandler <- function(e) {
    err <<- conditionMessage(e)
    ret.obj # Return argument ret.obj if an error occcurs.
  }

  # evaluates the expression
  warn <- err <- msg <- NULL
  value <- withCallingHandlers(tryCatch(expr,
                                        error = errhandler),
                               message = msghandler,
                               warning = warhandler)

  # returns a list with value, error warning and message:
  list(value = value, error = err, warning = warn, message = msg)
}
