# opsr_from_opsr <- function(object, data, ...) {
#   if (OPSR:::is_opsr_null(object)) {
#     stop("Null model not supported")
#   }
#   dots <- list(...)
#   oc <- object$call
#   for (i in names(dots)) {
#     di <- dots[[i]]
#     oc[[i]] <- substitute(di)
#   }
#   oc$data <- substitute(data)
#   fit <- eval(oc, parent.frame())
#   fit
# }

opsr_from_opsr <- function(object, data, ...) {
  fit <- opsr(object$formula, data = data, fixed = object$fixed, start = object$start,
              ...)
  fit
}

## out-of-sample gofs
loss <- function(object, data, test_ind, ...) {
  mf <- model.frame(object, data = data)
  mf_test <- mf[test_ind, ]
  y <- OPSR:::get_y(object, data = data)
  y_test <- y[test_ind]
  z <- OPSR:::get_z(object, data = data)
  z_test <- z[test_ind]
  nm <- c("Total", paste0("o", sort(unique(z))))
  n_test <- setNames(c(length(test_ind), as.numeric(table(z_test))), nm)

  ## ll vectors
  ll_all <- opsr_from_opsr(object, data = data, .loglik = TRUE, ...)
  ll <- setNames(ll_all[test_ind], paste0("o", z_test))

  ## mean ll
  from_vector <- function(x, FUN = mean) {
    x_tot <- FUN(x)
    x_reg <- aggregate(x, by = list(z = z_test), FUN = FUN)$x
    x_out <- setNames(c(x_tot, x_reg), nm)
    x_out
  }

  ll_mean <- from_vector(ll)

  ## ll probit vectors
  ll_probit <- function() {
    probs <- lapply(seq_len(object$nReg), function(i) {
      predict(object, newdata = data, group = i, type = "prob")
    })
    probs <- rowSums(Reduce(cbind, probs), na.rm = TRUE)
    log(probs)
  }

  ll_p_all <- ll_probit()
  ll_p <- setNames(ll_p_all[test_ind], paste0("o", z_test))

  ## mean ll probit
  ll_p_mean <- from_vector(ll_p)

  ## R2
  residuals <- function(object, data_test) {
    y <- OPSR:::get_y(object, data_test)
    p <- lapply(seq_len(object$nReg), function(j) {
      predict(object, group = j, type = "response", newdata = data_test)
    })
    p_red <- Reduce(cbind, p)
    fitted <- rowSums(p_red, na.rm = TRUE)
    y - fitted
  }

  r2 <- function(object, data_test) {
    z <- OPSR:::get_z(object, data_test)
    y <- OPSR:::get_y(object, data_test)
    RS <- residuals(object, data_test)^2
    TS <- (y - mean(y))^2
    R2o <- unlist(lapply(seq_len(object$nReg), function(i) {
      RSS <- sum(RS[z == i])
      yo <- y[z == i]
      TSS <- sum((yo - mean(yo))^2)
      1 - RSS/TSS
    }))
    R2total <- 1 - sum(RS)/sum(TS)
    R2 <- c(R2total, R2o)
    names(R2) <- c("Total", paste0("o", 1:object$nReg))
    R2
  }

  R2 <- r2(object, mf_test)

  ## out
  out <- list(
    z = z_test,
    ll = ll,
    ll_mean = ll_mean,
    ll_p = ll_p,
    ll_p_mean = ll_p_mean,
    r2 = R2
  )

  out
}

opsr_kfold <- function(object, k = 10, verbose = TRUE, ...) {
  if (!is(object, "opsr")) stop("'object' has to be of class 'opsr'.")
  n <- object$nObs[["Total"]]
  mf <- model.frame(object)

  out <- vector("list", length = k)
  folds <- sample(cut(seq(1, n), breaks = k, labels = FALSE), replace = FALSE)
  for (i in 1:k) {
    tryCatch({
      if (verbose) cat(sprintf("Iteration fold %2d:%2d\n", i, k))
      test_ind <- which(folds == i)
      dat_train <- mf[-test_ind, ]
      dat_test <- mf[test_ind, ]
      fit <- opsr_from_opsr(object, data = dat_train, ...)
      regimes <- Formula::model.part(fit$formula, data = dat_test, lhs = 1, drop = TRUE)
      regimes <- paste0("reg", regimes)
      loss_ <- loss(fit, mf, test_ind)
      out[[i]] <- loss_
    },
    error = function(e) {
      out[[i]] <<- e
    })
  }

  class(out) <- c("opsr.kfold", class(out))
  out
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


#' @method plot opsr.kfold
#' @export
plot.opsr.kfold <- function(x, i = "ll_mean", FUN = mean, main = NULL, ...) {
  x. <- x[i = i, MARGIN = 2, FUN = FUN]
  plot(x., ylab = "Loss", xaxt = "n", xlab = "Sample", main = main %||% i, ...)
  axis(1, at = 1:length(x.), labels = c("Total", paste0("Reg.", 1:(length(x.)-1))))
}

## TODO
## also use weighted mean (as in opsr_select()) in kfplot??
#' @export
kfplot <- function(x, i, FUN = mean, ...) {
  is_list_of_opsr_kfold <- all(
    c(is.list(x),
      sapply(x, function(x) is(x, "opsr.kfold")))
  )
  if (!(is_list_of_opsr_kfold)) {
    stop("'x' must be a list of objects of class 'opsr.kfold'")
  }
  x. <- t(sapply(x, function(x) x[i, MARGIN = 2, FUN]))
  matplot(t(x.), ylab = "Loss", xaxt = "n", xlab = "Sample", ...)
  axis(1, at = 1:ncol(x.), labels = c("Total", paste0("Reg.", 1:(ncol(x.)-1))))
}
