#' Print Method for OPSR ATE Objects
#'
#' @param x an object of class `"opsr.ate"`.
#' @param ... further arguments passed to [`summary.opsr.ate`].
#'
#' @return Returns `x` invisibly.
#'
#' @details
#' This is just a wrapper around [`summary.opsr.ate`] and a subsequent call to
#' [`print.summary.opsr.ate`].
#'
#' @seealso [`print.summary.opsr.ate`]
#'
#' @method print opsr.ate
#' @export
print.opsr.ate <- function(x, ...) {
  sx <- summary(x, ...)
  sx$call <- match.call()
  print(sx)
  invisible(x)
}
