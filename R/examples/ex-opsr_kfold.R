sim_dat <- OPSR::opsr_simulate()
dat <- sim_dat$data
dat$xo3 <- runif(n = nrow(dat))
dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
f <- ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2
fit <- OPSR::opsr(f, dat, printLevel = 0)
fit_select <- opsr_select(fit, loss = "bic", printLevel = 0)
kfold <- opsr_kfold(fit, printLevel = 0)
kfold_select <- opsr_kfold(fit_select, printLevel = 0)

## extract
ll_mean <- kfold["ll_mean"]
list2df(ll_mean)

## plot
colvec <- c("#ff8811","#48a9a6")
kfplot(list(kfold, kfold_select), col = colvec)
legend("bottomleft", legend = c("fit", "fit_select"), fill = colvec,
       title = "Model", title.font = 2, bty = "n")
