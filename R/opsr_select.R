## TODO
## impelement keep (vars which should never be dropped)
opsr_step <- function(object, pval, log = new.env(), keep = NULL, .step = 1, ...) {
  partial_match <- function(x, y) {
    sapply(x, function(v) sum(grepl(v, y, fixed = TRUE)))
  }

  vars_from_f <- function(formula, rhs) {
    attr(terms(formula(formula, rhs = rhs)), "term.labels")
  }

  get_factors <- function(object) {
    mf <- model.frame(object)
    fcts <- sapply(mf, is.factor)
    fcts <- names(fcts)[fcts]
    sapply(fcts, function(x) length(levels(mf[[x]])))
  }

  get_terms2drop <- function(object, pval, keep = NULL) {
    coef_table <- summary(object)$coef_table
    candi <- row.names(subset(coef_table, subset = `Pr(> t)` > pval))
    candi <- candi[grepl("^s_|^o[0-9]_", candi)]
    nm <- sub("^((s|o[0-9])).*", "\\1", candi)
    candi <- sub("^s_|^o[0-9]_", "", candi)
    names(candi) <- nm

    fcts <- get_factors(object)

    reg <- c("s", paste0("o", 1:object$nReg))
    terms2drop <- lapply(seq_along(reg), function(i) {
      vars. <- vars_from_f(object$formula, rhs = i)
      candi. <- candi[nm == reg[i]]
      if (length(candi.) == 0) {
        return(NULL)
      }
      ## identify elems in vars. that are partial matches in candi.
      pm <- partial_match(vars., candi.)
      ## if any factor level is significant => don't drop the term
      ## check for factors whether all levels (n-1) are in candi.
      pm. <- sapply(seq_along(pm), function(i) {
        x <- pm[i]
        nx <- names(x)
        if (names(pm)[i] %in% names(fcts)) {
          if (fcts[nx]-1 != x) {  # don't drop it
            x[1] <- 0
          }
        }
        out <- x
        out
      })

      names(pm.)[pm > 0]
    })
    names(terms2drop) <- reg
    terms2drop
  }

  update_formula <- function(object, terms2drop) {
    form <- object$formula
    for (i in seq_along(terms2drop)) {
      if (is.null(terms2drop[[i]]))
        next
      rhs <- paste(terms2drop[[i]], collapse = " + ")
      u <- paste0(". -(", rhs, ")")
      fs <- paste0(". ~ ", paste(rep(". | ", i-1), collapse = ""), u)
      fu <- as.formula(fs)
      form <- update(form, fu)
    }
    form
  }

  t2d <- get_terms2drop(object, pval)
  fu <- update_formula(object, t2d)

  ## keep track
  if (!is.null(log)) {
    key <- paste0("step.", .step)
    to.log <- list()
    to.log$call <- match.call()
    to.log$formula <- fu
    to.log$pval <- pval
    to.log$eliminate <- t2d
    assign(key, to.log, envir = log)
  }

  ## don't fit the same model again...
  if (all(sapply(t2d, is.null))) {
    return(object)
  }

  fit <- update(object, fu, ...)
  fit
}

NO_TERMS_DROPPED <- 0
NO_MODEL_COMPARISON <- list(model.1 = 0, model.2 = 0, test = 0, winner = 1)

opsr_select <- function(object, pseq = seq(0.9, 0.1, by = -0.1), log = new.env(),
                        keep = NULL, verbose = TRUE, verbose.kfold = FALSE,
                        loss = c("kfold", "aic", "bic", "lrt"),
                        ...) {
  start_time <- Sys.time()
  cache <- new.env()  # tmp store for loss computations in each iteration
  loss_cache <- list()  # stores loss computations of each step
  loss_res <- list()  # stores formatted loss results for print
  winner_hist <- list()  # stores sequence of winners

  kfold <- function(model.1, model.2) {
    ## TODO
    ## here the costly cross-validation gets performed for a model
    ## already cross-validated... => cache (and retrieve winner)
    models <- list(model.1, model.2)
    comp <- lapply(seq_along(models), function(i) {
      kf <- opsr_kfold(models[[i]], printLevel = 0, verbose = verbose.kfold)
      cache[[paste0("model.", i)]] <- kf
      ## use weighted mean here as otherwise if nobs per regime greatly
      ## differs, the fit of some regimes does barely matter...
      wm <- kf["ll", MARGIN = 1, FUN = function(x) {
        tab <- table(names(x))
        weights <- 1 / tab
        weights <- weights / sum(weights)
        weights <- weights[names(x)]
        weighted.mean(x, w = weights)
      }]
      mean(wm)  # average over folds
    })

    ll <- Reduce(c, comp)
    out <- list()
    if (ll[1] > ll[2]) out$winner <- 1 else out$winner <- 2
    out$loss_res <- list(  # for print
      model.1 = ll[1],
      model.2 = ll[2],
      test = ll[1] - ll[2],
      winner = out$winner
    )
    out
  }

  aic <- function(model.1, model.2) {
    aic.1 <- summary(model.1)$GOF$AIC
    aic.2 <- summary(model.2)$GOF$AIC
    out <- list()
    if (aic.1 < aic.2) out$winner <- 1 else out$winner <- 2
    out$loss_res <- list(
      model.1 = aic.1,
      model.2 = aic.2,
      test = aic.1 - aic.2,
      winner = out$winner
    )
    out
  }

  bic <- function(model.1, model.2) {
    bic.1 <- summary(model.1)$GOF$BIC
    bic.2 <- summary(model.2)$GOF$BIC
    out <- list()
    if (bic.1 < bic.2) out$winner <- 1 else out$winner <- 2
    out$loss_res <- list(
      model.1 = bic.1,
      model.2 = bic.2,
      test = bic.1 - bic.2,
      winner = out$winner
    )
    out
  }

  lrt <- function(model.1, model.2) {
    at <- anova(model.2, model.1)  # model.2 should be sparser
    chi <- at$table$`Pr(>Chi)`[2]
    out <- list()
    if (chi < 0.1) out$winner <- 1 else out$winner <- 2
    out$loss_res <- list(  # for print
      model.1 = at$table[1, "logLik"],
      model.2 = at$table[2, "logLik"],
      test = at$table[2, "Test"],
      winner = out$winner
    )
    out
  }

  loss <- match.arg(loss)
  fashion_show <- switch(loss,
                         "kfold" = kfold,
                         "aic" = aic,
                         "bic" = bic,
                         "lrt" = lrt)

  if (verbose) {
    cat(sprintf("%-5s %-5s %14s\n", "Step", "pval", "Current winner"))
  }

  winner <- object
  cat_winner <- "initial model"
  for (i in seq_along(pseq)) {
    pval <- pseq[i]
    log_key <- paste0("step.", i)
    winner_hist[[log_key]] <- cat_winner
    if (verbose) {
      cat(sprintf("%-5s %-5s %14s\n", i, pval, cat_winner))
    }
    fit.step <- opsr_step(object, pval = pval, log = log, .step = i, ...)
    logi <- get(log_key, envir = log)
    if (all(sapply(logi$eliminate, is.null))) {  # no terms are dropped
      loss_cache[[log_key]] <- NO_TERMS_DROPPED
      loss_res[[log_key]] <- NO_MODEL_COMPARISON
      next
    }
    w <- fashion_show(winner, fit.step)
    ## keep track
    loss_cache[[log_key]]$model.1 <- cache$model.1
    loss_cache[[log_key]]$model.2 <- cache$model.2
    loss_res[[log_key]] <- w$loss_res
    if (w$winner == 1)  # old winner
      next
    else {
      winner <- fit.step
      cat_winner <- log_key
    }
  }

  runtime <- Sys.time() - start_time

  if (verbose) {
    cat("\nFinal model:", cat_winner, "\n")
  }

  ## return some useful information
  ui <- list()
  ui$call <- match.call()
  ui$call.initial <- object$call
  ui$formula.initial <- object$formula
  ui$runtime <- runtime
  ui$log <- as.list(log)
  ui$loss <- loss
  ui$loss_res <- loss_res
  ui$loss_cache <- loss_cache
  ui$winner <- cat_winner
  ui$winner_hist <- winner_hist

  winner$opsr.select <- ui
  class(winner) <- c("opsr.select", class(winner))
  winner
}

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

#' @importFrom methods setMethod
#' @importFrom texreg extract
methods::setMethod("extract", signature = className("opsr.select", "TWTE"),
                   definition = OPSR:::extract.opsr)
