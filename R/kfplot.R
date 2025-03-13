#' Comparison Plot Method for List of OPSR kfold Objects
#'
#' @param x a list of objects of class `"opsr.kfold"`.
#' @param i the loss to extract (see 'Value' in [`loss`]). One of `"ll_mean"`,
#'   `"ll_p_mean"` or `"r2"`.
#' @param main a main title for the plot, see also [`title`].
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... further arguments passed to [`boxplot`].
#'
#' @return See 'Value' in [`boxplot`].
#'
#' @seealso [`loss`]
#' @export
kfplot <- function(x, i = c("ll_mean", "ll_p_mean", "r2"), main = NULL,
                   xlab = NULL, ylab = NULL, ...) {
  is_list_of_opsr_kfold <- all(
    c(is.list(x),
      sapply(x, function(x) is(x, "opsr.kfold")))
  )
  if (!(is_list_of_opsr_kfold)) {
    stop("'x' must be a list of objects of class 'opsr.kfold'")
  }
  i <- match.arg(i)
  loss <- lapply(x, function(x) x[i])
  loss. <- lapply(seq_along(loss), function(i) {
    df <- data.frame(Reduce(rbind, loss[[i]]))
    df$sample <- i
    df
  })
  df <- Reduce(rbind, loss.)
  rownames(df) <- NULL

  dfl <- stack(df, select = -sample)
  dfl$sample <- rep(df$sample, times = ncol(df) - 1)
  dfl$ind <- factor(dfl$ind, labels = c("Total", paste0("G", 1:(ncol(df)-2))))

  bp <- boxplot(values ~ ind:sample, data = dfl, lex.order = TRUE,
                sep = ":", ylab = ylab %||% "Loss", xlab = xlab %||% "Sample",
                main = main %||% i, ...)

  means <- aggregate(values ~ ind + sample, data = dfl, FUN = mean)
  means <- means[order(means$ind), ]
  points(1:length(bp$names), means$values, col = "red")
  invisible(bp)
}
