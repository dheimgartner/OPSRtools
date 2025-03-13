#' Select a Model in a Stepwise Algorithm
#'
#' Iteratively calls [`opsr_step`] and compares the resulting model to the
#' current winner (as evaluated by a selected loss function).
#'
#' @param object an object of class `"opsr"`.
#' @param pseq sequence of p-value thresholds (each of which is passed as `pval`
#'   to [`opsr_step`]).
#' @param log environment to keep track of changes to `object` (in particular
#'   variables being eliminated).
#' @param verbose if `TRUE`, prints working information during computation.
#' @param verbose.kfold passed as `verbose` to [`opsr_kfold`].
#' @param loss The loss function for model comparison. Can be abbreviated.
#'   See 'Details' section for more information.
#' @param ... additional arguments passed to [`opsr_select`]
#'
#' @return An object of class `"opsr.kfold"`.
#'
#' @details
#' Currently four loss functions are available which can be selected via the
#' `loss` argument. The loss is then computed for the two models to be compared
#' and a winner is selected. Can be one of `"kfold"` (see also [`opsr_kfold`]),
#' `"aic"` for AIC, `"bic"` for BIC and `"lrt"` for a likelihood ratio test.
#'
#' @seealso [`opsr_select`], [`opsr_kfold`]
#' @export
opsr_select <- function(object, pseq = seq(0.9, 0.1, by = -0.1), log = new.env(),
                        verbose = TRUE, verbose.kfold = FALSE,
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
