sim_dat <- OPSR::opsr_simulate()
dat <- sim_dat$data
weights <- runif(nrow(dat))
fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat = dat, weights = weights,
                  printLevel = 0)
ate <- opsr_ate(fit, type = "response")
print(ate)
summary(ate)

ate_w <- opsr_ate(fit, type = "response", weights = rep(1, nrow(dat)))
summary(ate_w)

pairs(ate)
