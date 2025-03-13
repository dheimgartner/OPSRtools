#' @method print te
#' @export
print.te <- function(x, digits = max(3L, getOption("digits") - 3L),
                     signif.legend = TRUE, ...) {
  nReg <- ncol(x)
  p.vals <- to.signif.codes(attr(x, "p.vals"))
  legend <- attr(p.vals, "legend")
  te <- format(x, digits = digits)
  p.vals <- format(p.vals, digits = 3)
  mat <- matrix(nrow = nrow(x), ncol = 2*ncol(x))
  fill.in.2nd <- function(mat, x, start) {
    j <- start
    for (i in 1:ncol(x)) {
      mat[, j] <- x[, i]
      j <- j + 2
    }
    mat
  }
  mat <- fill.in.2nd(mat, te, 1)
  mat <- fill.in.2nd(mat, p.vals, 2)
  df <- as.data.frame(mat)
  rownames(df) <- rownames(x)
  make.col.names <- function(nReg) {
    nm <- lapply(seq_len(nReg), function(i) {
      c(paste0("G", i), "")
    })
    Reduce(c, nm)
  }
  colnames(df) <- make.col.names(nReg)
  print.data.frame(df)
  if (signif.legend) {
    cat("---\nSignif. codes:  ", legend, sep = "")
  }
  invisible(df)
}
