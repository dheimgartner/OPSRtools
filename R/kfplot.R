## TODO
## also use weighted mean (as in opsr_select()) in kfplot??
#' @export
kfplot <- function(x, i, FUN = mean, ...) {
  is_list_of_opsr_kfold <- all(
    c(is.list(x),
      sapply(x, function(x) is(x, "opsr.kfold")))
  )
  if (!(is_list_of_opsr_kfold)) {
    stop("'x' must be a list of objects of class 'opsr.kfold'")
  }
  x. <- t(sapply(x, function(x) x[i, MARGIN = 2, FUN]))
  matplot(t(x.), ylab = "Loss", xaxt = "n", xlab = "Sample", ...)
  axis(1, at = 1:ncol(x.), labels = c("Total", paste0("Reg.", 1:(ncol(x.)-1))))
}
