#' Plot Method for OPSR kfold Objects
#'
#' @param x an object of class `"opsr.kfold"`.
#' @param i the loss to extract (see 'Value' in [`loss`]). One of `"ll_mean"`,
#'   `"ll_p_mean"` or `"r2"`.
#' @param main a main title for the plot, see also [`title`].
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... further arguments passed to [`boxplot`].
#'
#' @return See 'Value' in [`boxplot`].
#'
#' @method plot opsr.kfold
#' @seealso [`loss`]
#' @export
plot.opsr.kfold <- function(x, i = c("ll_mean", "ll_p_mean", "r2"),
                            main = NULL, xlab = NULL, ylab = NULL, ...) {
  i <- match.arg(i)
  df <- Reduce(rbind, x[i = i])
  boxplot(df, ylab = ylab %||% "Loss", xlab = xlab %||% "Sample",
          main = main %||% i, xaxt = "n", ...)
  axis(1, at = 1:ncol(df), labels = c("Total", paste0("G", 1:(ncol(df)-1))))
}
