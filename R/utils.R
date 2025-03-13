## creds to stats::printCoefmat
to.signif.codes <- function(pv) {
  Signif <- symnum(pv, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  Signif
}

## use MARGIN = 2 for ll_mean
#' @export
`[.opsr.kfold` <- function(x, i, MARGIN, FUN = function(x) x, .align = TRUE) {
  x. <- sapply(x, function(x) x[[i]])
  if (.align & !is.matrix(x.)) {
    l <- sapply(x., length)
    x. <- sapply(x., function(x) x[1:min(l)])
  }
  x. <- apply(t(x.), MARGIN = MARGIN, FUN = FUN)
  dimnames(x.) <- names(x.) <- NULL
  x.
}

#' @importFrom methods setMethod
#' @importFrom texreg extract
methods::setMethod("extract", signature = className("opsr.select", "TWTE"),
                   definition = OPSR:::extract.opsr)
