context("component_proj_from_file")
library(popmodules)
library(testthat)

test_data_pass <- data.frame(year = rep(c(2001, 2002),2), gss_code = rep("E09000001", 4),
                             age = rep(20, 4),
                             sex = rep(c("female", "male"), each = 2),
                             int_in = c(0.1, 0.2))
saveRDS(test_data_pass, file = "test_data/test_component_proj_from_file_pass.rds")

# tests for the filename validation

test_that("a .csv file should fail with a message", {
  expect_error(component_proj_from_file("test_data/test_component_proj_from_file.csv",
                                        proj_yrs = 2001:2002,
                                        col_data = "int_in",
                                        col_year = "year",
                                        col_sex = "sex",
                                        col_aggregation = c("year", "gss_code", "age", "sex")),"component projection file must be a .rds file")
})

test_that("a .rds file should read in OK", {
  expect_equivalent(component_proj_from_file("tests/testthat/test_data/test_component_proj_from_file_pass.rds",
                                             proj_yrs = 2001:2002,
                                             col_data = "int_in",
                                             col_year = "year",
                                             col_sex = "sex",
                                             col_aggregation = c("year", "gss_code", "age", "sex")), test_data_pass)
})

test_that("error should be thrown if first proj year is not in component df", {
  expect_error(component_proj_from_file("test_data/test_component_proj_from_file_pass.rds", proj_yrs = 2002:2003), "the component projection data does not contain all the projection years")
})

test_that("the df should be filtered to only projection years", {
  expect_equivalent(component_proj_from_file("test_data/test_component_proj_from_file_pass.rds", proj_yrs = 2002), filter(test_data_pass, year >= 2002))
})

# TODO add tests for the dataframe validation
