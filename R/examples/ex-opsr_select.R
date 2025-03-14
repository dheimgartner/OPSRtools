sim_dat <- OPSR::opsr_simulate()
dat <- sim_dat$data
dat$xo3 <- runif(n = nrow(dat))
dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
f <- ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2
fit <- OPSR::opsr(f, dat, printLevel = 0)
fit_select <- opsr_select(fit, loss = "aic", printLevel = 0)
print(fit_select)
