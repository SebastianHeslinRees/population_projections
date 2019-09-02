context("rates_from_file")
library(popmodules)
library(testthat)

test_data_pass <- data.frame(year = rep(c(2001, 2002),2), gss_code = rep("E09000001", 4), age = rep(20, 4), sex = rep(c("female", "male"), each = 2), rate = c(0.1, 0.2))
saveRDS(test_data_pass, file = "test_data/test_rates_from_file_pass.rds")

# tests for the filename validation

test_that("a .csv file should fail with a message", {
  expect_error(rates_from_file("popmodules/tests/testthat/test_data/test_rates_from_file.csv"),"rates file must be a .rds file")
})

test_that("a .rds file should read in OK", {
  expect_equivalent(rates_from_file("test_data/test_rates_from_file_pass.rds"), test_data_pass)
})

# TODO add tests for the dataframe validation
