test_that("runs without error and selects the correct model on aic", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  pseq <- c(seq(0.9, 0.1, by = -0.1), 0.01)
  expect_no_error({
    dat <- dat
    fit_select <- opsr_select(fit, pseq = pseq, loss = "aic", verbose = FALSE, printLevel = 0)
  })
  expect_identical(fit_select$formula, Formula::Formula(ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2))
})

test_that("runs without error and selects the correct model on bic", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  pseq <- c(seq(0.9, 0.1, by = -0.1), 0.01)
  expect_no_error({
    dat <- dat
    fit_select <- opsr_select(fit, pseq = pseq, loss = "bic", verbose = FALSE, printLevel = 0)
  })
  expect_identical(fit_select$formula, Formula::Formula(ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2))
})

test_that("runs without error on lrt", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  expect_no_error({
    dat <- dat
    fit_select <- opsr_select(fit, loss = "lrt", verbose = FALSE, printLevel = 0)
  })
})
