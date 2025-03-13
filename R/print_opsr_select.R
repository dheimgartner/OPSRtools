#' @method print opsr.select
#' @export
print.opsr.select <- function(x, digits = max(3L, getOption("digits") - 3L),
                              print.call = TRUE, print.elim.hist = TRUE, ...) {
  info <- x$opsr.select
  heading <- "Stepwise Model Path"
  model.initial <- info$formula.initial
  model.final <- x$formula

  make.res.df <- function(x, ...) {
    loss_res <- x$opsr.select$loss_res
    df <- lapply(loss_res, as.data.frame)
    df <- Reduce(rbind, df)
    df$winner_hist <- as.character(x$opsr.select$winner_hist)
    colnames(df) <- c("Current", "Opponent", "Test", "Winner", "Current winner")
    format.data.frame(df, digits = digits, ...)
  }

  make.hist.df <- function(x, ...) {
    to.df <- function(step) {
      elim <- lapply(step$eliminate, function(x) {
        out.1 <- if (is.null(x)) "" else x
        out.2 <- paste(out.1, collapse = " + ")
        out.2
      })
      as.data.frame(elim)
    }
    log <- lapply(x$opsr.select$log, to.df)
    df <- Reduce(rbind, log)
    format.data.frame(df, digits = digits, ...)
  }

  res <- make.res.df(x)
  hist <- make.hist.df(x)

  cat(heading, "\n\n")
  if (print.call) {
    cat("Call:\n", paste(deparse(info$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat("Initial model:\n", paste(deparse(model.initial), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("Final model:\n", paste(deparse(model.final), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("Runtime: ", format(unclass(info$runtime), digits = 3), " ", attr(info$runtime, "units"), "\n\n", sep = "")
  if (print.elim.hist) {
    cat("Elimination history:\n")
    print(hist)
    cat("\n")
  }
  cat("Model comparison:\n")
  print(res)
  invisible(x)
}
