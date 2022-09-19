#' @noRd
tsci_fit_NA_return <- function(Q) {
  # this function returns an object consisting of NA-entries only but of the same dimension as
  # the return object of the function tsci_selection except for the element 'mse'.
  Coef_all <- sd_all <- pval_all <- rep(NA, Q)
  names(Coef_all) <- names(sd_all) <- names(pval_all) <- paste("TSCI-q", seq(0, Q - 1), sep = "")
  CI_all <- matrix(NA, nrow = 2, ncol = Q)
  rownames(CI_all) <- c("lower", "upper")
  colnames(CI_all) <- names(Coef_all)
  Coef_sel <- sd_sel <- pval_sel <- rep(NA, 1)
  names(Coef_sel) <- names(sd_sel) <- names(pval_sel) <- c("TSCI-Estimate")
  CI_sel <- matrix(NA, nrow = 2, ncol = 1)
  rownames(CI_sel) <- c("lower", "upper")
  colnames(CI_sel) <- names(Coef_sel)
  iv_str <- iv_thol <- rep(NA, Q)
  names(iv_str) <- names(iv_thol) <- paste("q", seq(0, Q - 1), sep = "")
  Qmax <- q_comp <- q_cons <- rep(NA, Q)
  names(Qmax) <- names(q_comp) <- names(q_cons) <- paste("q", seq(0, Q - 1), sep = "")
  invalidity <- rep(NA, 3)
  names(invalidity) <- c("valid", "invalid", "non_testable")
  mse <- -1
  names(mse) <- "mse"

  list(
    Coef_all = Coef_all,
    sd_all = sd_all,
    pval_all = pval_all,
    CI_all = CI_all,
    Coef_sel = Coef_sel,
    sd_sel = sd_sel,
    pval_sel = pval_sel,
    CI_sel = CI_sel,
    iv_str = iv_str,
    iv_thol = iv_thol,
    Qmax = Qmax,
    q_comp = q_comp,
    q_cons = q_cons,
    invalidity = invalidity,
    mse = mse
  )
}
