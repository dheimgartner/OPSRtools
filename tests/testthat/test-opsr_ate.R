test_that("runs without error", {
  sim_dat <- OPSR::opsr_simulate()
  dat <- sim_dat$data
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  expect_no_error(ate <- opsr_ate(fit, type = "response"))
})
