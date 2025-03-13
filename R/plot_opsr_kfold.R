#' @method plot opsr.kfold
#' @export
plot.opsr.kfold <- function(x, i = "ll_mean", FUN = mean, main = NULL, ...) {
  x. <- x[i = i, MARGIN = 2, FUN = FUN]
  plot(x., ylab = "Loss", xaxt = "n", xlab = "Sample", main = main %||% i, ...)
  axis(1, at = 1:length(x.), labels = c("Total", paste0("Reg.", 1:(length(x.)-1))))
}
