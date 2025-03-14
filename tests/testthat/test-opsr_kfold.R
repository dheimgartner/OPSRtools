test_that("runs without error and with different k", {
  sim_dat <- OPSR::opsr_simulate()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  k <- 5
  expect_no_error(kfold <- opsr_kfold(fit, k = k, verbose = FALSE, printLevel = 0))
  expect_true(length(kfold) == k)

  k <- 9
  expect_no_error(kfold <- opsr_kfold(fit, k = k, verbose = FALSE, printLevel = 0))
  expect_true(length(kfold) == k)
})

test_that("extract method runs without error and produces list of length k", {
  sim_dat <- OPSR::opsr_simulate()
  dat <- sim_dat$data
  dat$xo3 <- runif(n = nrow(dat))
  dat$xo4 <- factor(sample(c("level1", "leve2", "level3"), nrow(dat), replace = TRUE))
  fit <- OPSR::opsr(ys | yo ~ xs1 + xs2 + log(xo3) | xo1 + xo2 + xo3 + xo4 | xo1 + xo2 + xo3 | xo1 + xo2,
                    dat, printLevel = 0)
  k <- 10
  kfold <- opsr_kfold(fit, k = k, verbose = FALSE, printLevel = 0)
  expect_no_error(x <- kfold["z"])
  expect_true(length(x) == k)
  expect_no_error(x <- kfold["ll"])
  expect_true(length(x) == k)
  expect_no_error(x <- kfold["ll_mean"])
  expect_true(length(x) == k)
  expect_no_error(x <- kfold["ll_p"])
  expect_true(length(x) == k)
  expect_no_error(x <- kfold["ll_p_mean"])
  expect_true(length(x) == k)
  expect_no_error(x <- kfold["r2"])
  expect_true(length(x) == k)
})
