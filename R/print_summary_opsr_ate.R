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
