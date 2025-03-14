## creds to stats::printCoefmat
to.signif.codes <- function(pv) {
  Signif <- symnum(pv, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  Signif
}

#' Extract Method for OPSR kfold Object
#'
#' @param x an object of class `"opsr.kfold"`.
#' @param i the loss to extract (see 'Value' in [`loss`]).
#'
#' @return A list of length k (where k is the number of folds).
#' @seealso [`loss`]
#' @export
`[.opsr.kfold` <- function(x, i) {
  x. <- lapply(x, function(x) x[[i]])
  x.
}

#' List to Data Frame
#'
#' Converts a list of vectors to a data frame.
#'
#' @param x a list of vectors.
#'
#' @returns A data frame.
#' @export
list2df <- function(x) {
  df <- data.frame(Reduce(rbind, x), row.names = NULL)
  df
}
