context("fert_from_file")
library(popmodules)
library(testthat)

test_data_pass <- data.frame(year = c(2001, 2002), gss_code = rep("E09000001", 2), age = rep(20, 2), sex = rep("female", 2), rate = c(0.1, 0.2))
saveRDS(test_data_pass, file = "test_data/test_fert_from_file_pass.rds")

# tests for the filename validation

test_that("a .csv file should fail with a message", {
  expect_error(fert_from_file("popmodules/tests/testthat/test_data/test_fert_from_file.csv"),"fertility file must be a .rds file")
})

test_that("a .rds file should read in OK", {
  expect_equivalent(fert_from_file("test_data/test_fert_from_file_pass.rds"), test_data_pass)
})

# TODO add tests for the dataframe validation
