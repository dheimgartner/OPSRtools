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
