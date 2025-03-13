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
