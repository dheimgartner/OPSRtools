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



