library(testthat)

test_df_1 <- data.frame(gss = c("a","b","c"), value = 1:3)
test_df_2 <- data.frame(gss = c("a","b","c"), value = 1:3)
test_df_3 <- data.frame(code = c("a","b","c"), value = 1:3)
test_df_4 <- data.frame(gss = c("c","b","a"), value = 1:3)
test_df_5 <- data.frame(gss = c("a","a","a"), value = 1:3)

test_that("validate_same_geography basic operation", {
  expect_silent(validate_same_geog(test_df_1, test_df_2, col_1="gss"))
  expect_silent(validate_same_geog(test_df_1, test_df_3, col_1="gss", col_2 = "code"))
  expect_silent(validate_same_geog(test_df_1, test_df_4, col_1="gss"))
})

test_that("validate_same_geography throws errors as expected", {
  expect_error(validate_same_geog(test_df_1, test_df_5, col_1="gss"))
  expect_error(validate_same_geog(test_df_5, test_df_1, col_1="gss"))
  expect_error(validate_same_geog(test_df_1, test_df_2, col_1="gss", error_or_warn = "dave grohl"))
  expect_error(validate_same_geog(test_df_1, test_df_2, col_1="kurt cobain"))
  expect_error(validate_same_geog(test_df_1, test_df_2, col_2="krist novoselic"))
})

test_that("validate_same_geography throws warnings as expected", {
  expect_warning(validate_same_geog(test_df_1, test_df_5, col_1="gss", error_or_warn = "warn"))
  expect_warning(validate_same_geog(test_df_5, test_df_1, col_1="gss", error_or_warn = "warn"))
})
