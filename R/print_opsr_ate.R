#' @method print opsr.ate
#' @export
print.opsr.ate <- function(x, ...) {
  sx <- summary(x)
  sx$call <- match.call()
  print(sx)
  invisible(x)
}
