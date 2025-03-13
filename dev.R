devtools::load_all()

library(OPSR)

## minimal example for each "functionality"
## data prep
sim_dat <- opsr_simulate()
dat <- sim_dat$data

dat$xo3 <- runif(n = nrow(dat))
dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))

## opsr_select
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
kfold <- opsr_kfold(fit, printLevel = 0)
kfold_select <- opsr_kfold(fit_select, printLevel = 0)
kfold[i = "ll_mean", MARGIN = 2]
print(kfold)  # no print method for opsr.kfold... (maybe add summyr.opsr.kfold and then print method?)
plot(kfold)
kfplot(list(kfold, kfold_select), i = "ll_mean")
kfplot(list(kfold, kfold_select), i = "ll_p_mean")
kfplot(list(kfold, kfold_select), i = "r2")

## opsr_ate
ate <- opsr_ate(fit_select, type = "response")
print(ate)
sry_ate <- summary(ate)
print(sry_ate)
print(sry_ate$ate)
print(sry_ate$te)

## pairs
pairs(ate, xlim = c(-10, 10), ylim = c(-7, 12))
