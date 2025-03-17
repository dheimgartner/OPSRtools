devtools::load_all()

library(OPSR)

## minimal example for each "functionality"
## data prep

## TODO
## come up with simulation specification that shows treatment effects...

sim_dat <- opsr_simulate()
dat <- sim_dat$data

dat$xo3 <- runif(n = nrow(dat))
dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))



## opsr_select
devtools::load_all()
fit <- opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2, dat)
summary(fit)

fit_step <- opsr_step(fit, pval = 0.1)
summary(fit_step)
anova(fit_step, fit)
texreg::screenreg(list(fit, fit_step), include.R2 = TRUE, include.pseudoR2 = TRUE)

fit_select <- opsr_select(fit, loss = "aic", printLevel = 0)
print(fit_select)
summary(fit_select)


## opsr_kfold
devtools::load_all()
kfold <- opsr_kfold(fit, printLevel = 0)
kfold_select <- opsr_kfold(fit_select, printLevel = 0)
kfold[i = "ll_mean"]
kfold[i = "ll"]
print(kfold)  # no print method for opsr.kfold... (maybe add summyr.opsr.kfold and then print method?)
plot(kfold)

devtools::load_all()
plot.it <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1, 3))
  for (i in c("ll_mean", "ll_p_mean", "r2")) {
    kfplot(list(kfold, kfold_select), i = i, col = "white")
  }
}
plot.it()


## opsr_ate
devtools::load_all()
ate <- opsr_ate(fit_select, type = "response")
print(ate)
sry_ate <- summary(ate)
print(sry_ate)
test <- print(sry_ate$ate)
print(sry_ate$te)



## pairs
devtools::load_all()
pairs(ate, xlim = c(-10, 10), ylim = c(-7, 12))




## text
plot(x = 1:10, y = 1:10)
points(2, 6)
text(2, 6, "topright", adj = c(-0.1, -0.1))
text(2, 6, "bottomleft", adj = c(1.1, 1.1))




## plot conditional expectation of fit_10 vs. fit_aic
devtools::load_all()
library(OPSR)
library(maxLik)

colvec <- c("#4c5760","#ff8811","#48a9a6")
timeuse <- OPSRtools::timeuse
weights <- timeuse$weight
cache <- readRDS("./vignettes/cache.rds")
fit <- cache$fit

fit_10 <- opsr_step(fit, pval = 0.1)
fit_aic <- opsr_step(fit, pval = 0.4)
# texreg::screenreg(list(cache$fit_aic, fit_aic))

rm(fit)

opsr_ate(fit_10, type = "unlog-response")
opsr_ate(fit_aic, type = "unlog-response")

plot.it <- function(type) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      p1 <- predict(fit_10, group = i, counterfact = j, type = type)
      p2 <- predict(fit_aic, group = i, counterfact = j, type = type)
      plot(x = p1, y = p2,
           main = paste(group[i], counterfact[j]),
           xlab = "fit_10", ylab = "fit_aic")
      abline(a = 0, b = 1, col = "red", lty = 3)
    }
  }
}

plot.it(type = "unlog-response")

## plot fit_10 Xb vs. fit_aic Xb and same for heckman correction
heckman_ij <- function(fit, i, j) {
  correction <- predict(fit, type = "correction", group = i, counterfact = j)
  correction
}

xb_ij <- function(fit, i, j) {
  xb <- predict(fit, type = "Xb", group = i, counterfact = j)
  xb
}

plot.values <- function(fit, i, j, digits = 3) {
  h <- heckman_ij(fit, i, j)
  x <- xb_ij(fit, i, j)
  mh <- mean(h, na.rm = TRUE)
  mx <- mean(x, na.rm = TRUE)
  ratio <- round(mh / mx, digits = digits)
  list(h = h, x = x, mh = mh, mx = mx, ratio = ratio)
}

plot.it <- function(what = c("Xb", "correction"), ...) {
  what <- match.arg(what)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      pv1 <- plot.values(fit_10, i, j)
      pv2 <- plot.values(fit_aic, i, j)
      x <- if (what == "Xb") pv1$x else pv1$h
      y <- if (what == "Xb") pv2$x else pv2$h
      plot(x, y,
           main = paste(group[i], counterfact[j]),
           xlab = "fit_10", ylab = "fit_aic",
           ...)
      abline(v = pv1$mx, lty = 2, lwd = 2)
      abline(h = pv2$mx, lty = 2, lwd = 2)
      abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
    }
  }
}

plot.it("Xb")
plot.it("correction", xlim = c(-1, 1), ylim = c(-1, 1))



## fit_aic vs. rho fixed => heckman-correction plot
start <- coef(fit_aic)
fixed <- c("rho1", "rho2", "rho3")
start[fixed] <- 0

fit_nocor <- opsr(fit_aic$formula, timeuse, weights = timeuse$weights,
                  start = start, fixed = fixed)

opsr_ate(fit_aic, type = "unlog-response")
opsr_ate(fit_nocor, type = "unlog-response")

heckman_ij <- function(fit, i, j) {
  correction <- predict(fit, type = "correction", group = i, counterfact = j)
  correction
}

xb_ij <- function(fit, i, j) {
  xb <- predict(fit, type = "Xb", group = i, counterfact = j)
  xb
}

plot.values <- function(fit, i, j, digits = 3) {
  h <- heckman_ij(fit, i, j)
  x <- xb_ij(fit, i, j)
  mh <- mean(h, na.rm = TRUE)
  mx <- mean(x, na.rm = TRUE)
  ratio <- round(mh / mx, digits = digits)
  list(h = h, x = x, mh = mh, mx = mx, ratio = ratio)
}

plot.it <- function(fit1, fit2) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      pv1 <- plot.values(fit1, i, j)
      pv2 <- plot.values(fit2, i, j)
      plot(pv1$h, pv1$x, xlim = c(-0.7, 0.7), ylim = c(3, 7),
           xlab = expression(rho[j]*sigma[j]*IMR), ylab = expression(X*beta),
           main = paste(group[i], counterfact[j]), type = "n")
      points(pv1$h, pv1$x, col = scales::alpha(colvec[1], 0.3), pch = 19)
      points(pv2$h, pv2$x, col = scales::alpha(colvec[2], 0.3), pch = 19)
      abline(v = pv1$mh, col = colvec[1], lty = 2, lwd = 2)
      abline(h = pv1$mx, col = colvec[1], lty = 2, lwd = 2)
      abline(v = pv2$mh, col = colvec[2], lty = 2, lwd = 2)
      abline(h = pv2$mx, col = colvec[2], lty = 2, lwd = 2)
      points(x = pv1$mh, y = pv1$mx, pch = 15)
      points(x = pv2$mh, y = pv2$mx, pch = 15)
      text(x = pv1$mh, y = pv1$mx, labels = pv1$ratio, adj = c(-0.1, -0.1), font = 2)
      text(x = pv2$mh, y = pv2$mx, labels = pv2$ratio, adj = c(1.1, 1.1), font = 2)
      legend("bottomleft", legend = c(substitute(fit1), substitute(fit2)), pch = 19,
             col = colvec[1:2], bty = "n")
    }
  }
}

plot.it(fit_aic, fit_nocor)

plot.it <- function(type) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      p1 <- predict(fit_aic, group = i, counterfact = j, type = type)
      p2 <- predict(fit_nocor, group = i, counterfact = j, type = type)
      plot(x = p1, y = p2,
           main = paste(group[i], counterfact[j]),
           xlab = "fit_aic", ylab = "fit_nocor")
      abline(a = 0, b = 1, col = "red", lty = 3)
    }
  }
}

plot.it(type = "unlog-response")




## same as above but with fit_10 fixed rho
start <- coef(fit_10)
fixed <- c("rho1", "rho2", "rho3")
start[fixed] <- 0

fit_nocor <- opsr(fit_10$formula, timeuse, weights = timeuse$weights,
                  start = start, fixed = fixed)

opsr_ate(fit_10, type = "unlog-response")
opsr_ate(fit_nocor, type = "unlog-response")

heckman_ij <- function(fit, i, j) {
  correction <- predict(fit, type = "correction", group = i, counterfact = j)
  correction
}

xb_ij <- function(fit, i, j) {
  xb <- predict(fit, type = "Xb", group = i, counterfact = j)
  xb
}

plot.values <- function(fit, i, j, digits = 3) {
  h <- heckman_ij(fit, i, j)
  x <- xb_ij(fit, i, j)
  mh <- mean(h, na.rm = TRUE)
  mx <- mean(x, na.rm = TRUE)
  ratio <- round(mh / mx, digits = digits)
  list(h = h, x = x, mh = mh, mx = mx, ratio = ratio)
}

plot.it <- function(fit1, fit2) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      pv1 <- plot.values(fit1, i, j)
      pv2 <- plot.values(fit2, i, j)
      plot(pv1$h, pv1$x, xlim = c(-0.7, 0.7), ylim = c(3, 7),
           xlab = expression(rho[j]*sigma[j]*IMR), ylab = expression(X*beta),
           main = paste(group[i], counterfact[j]), type = "n")
      points(pv1$h, pv1$x, col = scales::alpha(colvec[1], 0.3), pch = 19)
      points(pv2$h, pv2$x, col = scales::alpha(colvec[2], 0.3), pch = 19)
      abline(v = pv1$mh, col = colvec[1], lty = 2, lwd = 2)
      abline(h = pv1$mx, col = colvec[1], lty = 2, lwd = 2)
      abline(v = pv2$mh, col = colvec[2], lty = 2, lwd = 2)
      abline(h = pv2$mx, col = colvec[2], lty = 2, lwd = 2)
      points(x = pv1$mh, y = pv1$mx, pch = 15)
      points(x = pv2$mh, y = pv2$mx, pch = 15)
      text(x = pv1$mh, y = pv1$mx, labels = pv1$ratio, adj = c(-0.1, -0.1), font = 2)
      text(x = pv2$mh, y = pv2$mx, labels = pv2$ratio, adj = c(1.1, 1.1), font = 2)
      legend("bottomleft", legend = c(substitute(fit1), substitute(fit2)), pch = 19,
             col = colvec[1:2], bty = "n")
    }
  }
}

plot.it(fit_10, fit_nocor)

plot.it <- function(type) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(3, 3))

  group <- c("NTWers", "NUTWers", "UTWers")
  counterfact <- c("NTWing", "NUTWing", "UTWing")

  for (i in 1:3) {
    for (j in 1:3) {
      p1 <- predict(fit_10, group = i, counterfact = j, type = type)
      p2 <- predict(fit_nocor, group = i, counterfact = j, type = type)
      plot(x = p1, y = p2,
           main = paste(group[i], counterfact[j]),
           xlab = "fit_10", ylab = "fit_nocor")
      abline(a = 0, b = 1, col = "red", lty = 3)
    }
  }
}

plot.it(type = "unlog-response")





## fit_10 maxlik vs. 2step
fit_2step <- opsr(fit_10$formula, data = timeuse, weights = timeuse$weight, .get2step = TRUE)
fit_10$estimate <- fit_2step
pairs(opsr_ate(fit_10, type = "unlog-response"))


## TODO
## maybe adjust/fix rho3 in fit_10 and check what happens

