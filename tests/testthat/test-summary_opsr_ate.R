test_that("runs without error and can be printed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  ate <- opsr_ate(fit, type = "response")
  expect_no_error(sry_ate <- summary(ate))
  expect_output(print(sry_ate))
})

test_that("runs with weights and produces different results", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  weights <- runif(nrow(dat))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  fit_w <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, weights = weights, printLevel = 0)
  ate <- opsr_ate(fit, type = "response")
  ate_w <- opsr_ate(fit_w, type = "response")
  sry_ate <- summary(ate)
  sry_ate_w <- summary(ate_w)
  expect_true(any(sry_ate_w$te != sry_ate$te))  # maybe even all
})

test_that("can pass weights which then produces different results", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  weights <- runif(nrow(dat))
  fit_w <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, weights = weights, printLevel = 0)
  ate_w <- opsr_ate(fit_w, type = "response")
  ate <- opsr_ate(fit_w, type = "response", weights = rep(1, nrow(dat)))
  sry_ate <- summary(ate)
  sry_ate_w <- summary(ate_w)
  expect_true(any(sry_ate_w$te != sry_ate$te))  # maybe even all
})
