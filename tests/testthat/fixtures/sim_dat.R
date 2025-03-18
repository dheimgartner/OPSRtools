set.seed(123)
sim_dat <- OPSR::opsr_simulate()
saveRDS(sim_dat, "./tests/testthat/fixtures/sim_dat.rds")
