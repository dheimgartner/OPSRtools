#' Print Method for Summary OPSR ATE Objects
#'
#' @param x an object of class `"summary.opsr.ate"`.
#' @param digits minimum number of significant digits to be used for most numbers (passed to [`stats::printCoefmat`]).
#' @param print.call if `TRUE`, prints the underlying call.
#'
#' @return Prints `x` in 'pretty' form and returns it invisibly.
#'
#' @method print summary.opsr.ate
#' @export
print.summary.opsr.ate <- function(x, digits = max(3L, getOption("digits") - 3L),
                                   print.call = FALSE, ...) {
  heading <- "Treatment Effects"

  cat(heading, "\n\n")
  if (print.call) {
    cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat("TE\n\n")
  print(x$te, digits, signif.legend = FALSE)

  cat("\n\nATE\n\n")
  print(x$ate, digits, signif.legned = TRUE)
  invisible(x)
}
