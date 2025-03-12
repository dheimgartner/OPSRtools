## ... passed to predict.opsr
opsr_ate <- function(object, type, weights = NULL, ...) {
  if (is.null(weights)) {
    weights <- object$weights
  }

  ce.group <- function(object, group, type, ...) {
    p <- lapply(seq_len(object$nReg), function(x) {
      predict(object, group = group, counterfact = x, type = type, ...)
    })
    p <- Reduce(cbind, p)
    dimnames(p) <- NULL
    attr(p, "group") <- group
    class(p) <- c("ce.group", class(p))
    p
  }

  ce.treatment <- function(object, treatment, type, ...) {
    p <- lapply(seq_len(object$nReg), function(x) {
      predict(object, group = x, counterfact = treatment, type = type, ...)
    })
    p <- Reduce(cbind, p)
    dimnames(p) <- NULL
    attr(p, "treatment") <- treatment
    class(p) <- c("ce.treatment", class(p))
    p
  }

  ce.by.groups <- function(object, type, ...) {
    p <-
      lapply(seq_len(object$nReg), function(x) {
        ce.group(object, group = x, type = type, ...)
      })
    class(p) <- c("ce.by.groups", class(p))
    p
  }

  ce.by.treatments <- function(object, type, ...) {
    p <-
      lapply(seq_len(object$nReg), function(x) {
        ce.treatment(object, treatment = x, type = type, ...)
      })
    class(p) <- c("ce.by.treatments", class(p))
    p
  }

  average <- function(x, weights) {
    apply(x, 2, function(x) {
      stats::weighted.mean(x, w = weights, na.rm = TRUE)
    })
  }

  ## rows = group, cols = treatment
  ce.mean <- function(object, type, weights, ...) {
    nReg <- object$nReg
    ap <- lapply(seq_len(nReg), function(x) {
      pg <- ce.group(object, group = x, type = type, ...)
      average(pg, weights = weights)
    })
    ap <- Reduce(cbind, ap)
    dimnames(ap) <- NULL
    class(ap) <- c("ce.mean", class(ap))
    ap
  }

  ## apply funcs above
  pbg <- ce.by.groups(object, type = type, ...)
  pbt <- ce.by.treatments(object, type = type, ...)
  pm <- ce.mean(object, weights = weights, type = type, ...)

  ## structure output
  out <- list()
  out$fit <- object
  out$call <- match.call()
  out$ce.by.groups <- pbg
  out$ce.by.treatments <- pbt
  out$ce.mean <- pm
  out$nReg <- object$nReg
  out$weights <- weights

  class(out) <- c("opsr.ate", class(out))
  out
}

#' @method print opsr.ate
#' @export
print.opsr.ate <- function(x, ...) {
  sx <- summary(x)
  sx$call <- match.call()
  print(sx)
  invisible(x)
}

## TODO
## I think pairs is the only plot method that should get part of OPSRtools

#' @method pairs opsr.ate
#' @export
pairs.opsr.ate <- function(x, pch = 21, labels.diag = paste0("T", 1:x$nReg),
                           labels.reg = paste0("G", 1:x$nReg), col = 1:x$nReg,
                           add.rug = TRUE, lower.digits = 0, diag.digits = 0,
                           lwd.dens = 1.5, diag.cex.text = 1, upper.digits = 2,
                           upper.cex.text = 2, prefix = "", postfix = "",
                           lty.diag = 1, ...) {
  ## x is opsr.ate
  preprocess <- function() {
    dat <-
      lapply(x$ce.by.groups, function(x) {
        group <- attr(x, "group")
        df <- as.data.frame(x)
        df$group <- group
        df
      })

    dat <- Reduce(rbind, dat)
    names(dat) <- c(labels.diag, "group")
    dat$group <- factor(dat$group, labels = labels.reg)
    dat
  }

  ## g is group, w is weight
  panel.lower <- function(x, ...) {
    dots <- list(...)
    y <- dots[[1]]
    n <- length(unique(g))
    points(x, ...)
    if (add.rug) {
      rug(x, ticksize = 0.02)
      rug(y, ticksize = 0.02, side = 2)
    }
    xw <- as.numeric(
      by(x, g, function(x) stats::weighted.mean(x, w = w, na.rm = TRUE))
    )
    yw <- as.numeric(
      by(y, g, function(y) stats::weighted.mean(y, w = w, na.rm = TRUE))
    )
    abline(a = 0, b = 1, col = "red", lty = lty.diag, lwd = 2)
    points(xw, yw, pch = 15, col = "red", cex = 2 * par("cex"))
    tmp <- legend("topleft", legend = unique(g), pch = dots$pch, pt.bg = unique(dots$bg), bty = "n")
    xwr <- round(xw, lower.digits)
    ywr <- round(yw, lower.digits)
    text(tmp$rect$left + tmp$rect$w, tmp$text$y, paste0("(", xwr, ", ", ywr, ")"), col = "red", pos = 4)
  }

  ## x are actually the treatment effects
  panel.diag <- function(x, ...) {
    dots <- list(...)
    usr <- par("usr")
    par(usr = c(usr[1:2], 0, 1.5))  # sets height of ploting area!
    preprocess <- function() {
      i <- 1
      by(x, g, function(x) {
        df <- data.frame(x = na.omit(x))
        df$group <- i
        df$col <- col[i]
        i <<- i + 1
        df
      })
    }
    p.density <- function(dl) {
      dfl <-
        lapply(dl, function(x) {
          d <- density(x$x)
          df <- data.frame(x = d$x, y = d$y, col = unique(x$col))
        })
      maxy <- max(Reduce(rbind, dfl)$y)
      lapply(dfl, function(x) {
        ## rescale (ploting area was set via usr)
        x$y <- x$y / maxy
        lines(x$x, x$y, col = x$col, lwd = lwd.dens, ...)
      })
    }
    p.markers <- function() {
      xw <- as.numeric(
        by(x, g, function(x) stats::weighted.mean(x, w = w, na.rm = TRUE))
      )
      # points(xw, rep(0, length(xw)), pch = "|", col = "red", cex = cex.marker)
      labels <- round(xw, diag.digits)
      text(xw, y = rep(0, length(xw)), labels = labels, srt = 90, col = "red",
           cex = diag.cex.text, adj = c(-0.1, 0.5))
    }
    dl <- preprocess()
    p.density(dl)
    p.markers()
  }

  panel.upper <- function(x, y, ...) {
    par(usr = c(0, 1, 0, 1))
    ## careful: weights do not align and need to be prepared like below
    xo <- na.omit(x)
    xi <- attr(xo, "na.action")
    yo <- na.omit(y)
    yi <- attr(yo, "na.action")
    x.weights <- rep(w, object$nReg)[-xi]
    y.weights <- rep(w, object$nReg)[-yi]
    xw <- stats::weighted.mean(xo, w = x.weights)
    yw <- stats::weighted.mean(yo, w = y.weights)
    te <- xw - yw
    txt <- format(c(te, 0.123456789), digits = upper.digits)[1]
    txt <- paste0(prefix, txt, postfix)
    text(0.5, 0.5, txt, cex = upper.cex.text)
  }

  dat <- preprocess()
  w <- x$weights
  g <- dat$group
  object <- x
  pairs(dat[, -ncol(dat)], pch = pch, bg = col[dat$group],
        lower.panel = panel.lower,
        diag.panel = panel.diag,
        upper.panel = panel.upper,
        ...)

  invisible(x)
}

#' @method summary opsr.ate
#' @export
summary.opsr.ate <- function(object, ...) {  # ... passed to paired.t.test
  make.dim.names <- function(object) {
    nm <- paste0("T", 1:object$nReg)
    list(nm, nm)
  }

  wtd.paired.t.test <- function(x, y, weights = NULL) {
    diff <- y - x
    fit <- lm(diff ~ 1, weights = weights)
    s <- summary(fit)
    t.test <- s$coefficients[1, c("t value", "Pr(>|t|)")]
    class(t.test) <- c("wtd.paired.t.test", class(t.test))
    t.test
  }

  papply <- function(mat, FUN, ...) {
    cbn <- combn(ncol(mat), 2)
    out <- lapply(1:ncol(cbn), function(i) {
      idx <- cbn[, i]
      x <- mat[, idx[1]]
      y <- mat[, idx[2]]
      res <- FUN(x, y, ...)
      attr(res, "idx.papply") <- idx
      res
    })
    out
  }

  ## te
  ## for each group pairwise diff when switching regime
  te.group <- function(object, group) {
    dat <- object$ce.by.groups[[group]]
    w <- object$weights
    t.test <- papply(dat, wtd.paired.t.test, weights = w)
    te <- apply(dat, 2, function(x) {
      stats::weighted.mean(x, w = w, na.rm = TRUE)
    })
    te <- outer(te, te, "-")
    dimnames(te) <- make.dim.names(object)
    attr(te, "group") <- group
    attr(te, "t.test") <- t.test
    class(te) <- c("te.group", class(te))
    te
  }

  te.mat <- function(object) {
    te <- lapply(seq_len(object$nReg), function(i) {
      te.group(object, group = i)
    })
    class(te) <- c("te.mat", class(te))
    te
  }

  vec.from.mat <- function(x) {
    idx <- which(lower.tri(x), arr.ind = TRUE)
    df <- data.frame(
      from = rownames(x)[idx[, 2]],
      to = rownames(x)[idx[, 1]],
      te = x[idx]
    )
    nm <- paste(df$from, df$to, sep = "->")
    setNames(df$te, nm)
  }

  t.test.from.mat <- function(x) {
    t.test <- attr(x, "t.test")
    df <- as.data.frame(Reduce(rbind, t.test))
    from_to <- sapply(t.test, function(x) attr(x, "idx.papply"))
    nm <- paste0("T", from_to[1, ], "->T", from_to[2, ])
    setNames(df[, "Pr(>|t|)"], nm)
  }

  te <- function(object) {
    x <- te.mat(object)
    te <- sapply(x, vec.from.mat)
    p.vals <- sapply(x, t.test.from.mat)
    cn <- paste0("G", 1:object$nReg)
    colnames(te) <- cn
    colnames(p.vals) <- cn
    attr(te, "p.vals") <- p.vals
    class(te) <- c("te", class(te))
    te
  }

  ## ate = E[Y(1) - Y(0)]
  ate.mat <- function(object) {
    dat <- Reduce(rbind, object$ce.by.groups)
    w <- rep(object$weights, object$nReg)
    t.test <- papply(dat, wtd.paired.t.test, weights = w)
    ate <- apply(dat, 2, function(x) {
      x. <- na.omit(x)
      w. <- w[-attr(x., "na.action")]
      stats::weighted.mean(x., w = w.)
    })
    ate <- outer(ate, ate, "-")
    dimnames(ate) <- make.dim.names(object)
    attr(ate, "t.test") <- t.test
    class(ate) <- c("ate.mat", class(ate))
    ate
  }

  ate <- function(object) {
    x <- ate.mat(object)
    ate <- vec.from.mat(x)
    p.vals <- t.test.from.mat(x)
    attr(ate, "p.vals") <- p.vals
    class(ate) <- c("ate", class(ate))
    ate
  }

  ## TODO
  ## atet = E[Y(1) - Y(0) | Treated]
  ## atent = E[Y(1) - Y(0) | Not-treated]
  ## I think it is more reasonable in OPSR to assume any selection outcome as
  ## form of treatment => treatment = the ones who were observed having that
  ## selection outcome level ("received" the xs) and the untreated are all
  ## the others. Xinyi defined the untreated as NTWers...

  ## opsr.ate
  out <- list()
  out$call <- match.call()
  out$fit <- object$fit
  out$te.mat <- te.mat(object)
  out$te <- te(object)
  out$ate.mat <- ate.mat(object)
  out$ate <- ate(object)
  class(out) <- c("summary.opsr.ate", class(out))
  out
}

## creds to stats::printCoefmat
to.signif.codes <- function(pv) {
  Signif <- symnum(pv, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  Signif
}

#' @method print te
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

#' @method print ate
print.ate <- function(x, digits = max(3L, getOption("digits") - 3L),
                      signif.legend = TRUE, ...) {
  p.vals <- to.signif.codes(attr(x, "p.vals"))
  legend <- attr(p.vals, "legend")
  te <- format(x, digits = digits)
  p.vals <- format(p.vals, digits = 3)
  mat <- matrix(nrow = 1, ncol = 2*length(x))
  fill.in.2nd <- function(mat, x, start) {
    j <- start
    for (i in 1:length(x)) {
      mat[, j] <- x[i]
      j <- j + 2
    }
    mat
  }
  mat <- fill.in.2nd(mat, te, 1)
  mat <- fill.in.2nd(mat, p.vals, 2)
  df <- as.data.frame(mat)
  make.col.names <- function(x, n) {
    nm <- lapply(seq_len(n), function(i) {
      c(x[i], "")
    })
    Reduce(c, nm)
  }
  colnames(df) <- make.col.names(names(x), length(x))
  print.data.frame(df)
  if (signif.legend) {
    cat("---\nSignif. codes:  ", legend, sep = "")
  }
  invisible(df)
}

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
