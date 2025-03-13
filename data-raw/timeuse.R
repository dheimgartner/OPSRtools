## code to prepare `timeuse` dataset goes here

timeuse <- readRDS("./data-raw/timeuse.rds")

usethis::use_data(timeuse, overwrite = TRUE)
