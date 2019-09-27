context("project_forward_flat")
library(popmodules)
library(testthat)
library(dplyr)
library(tidyr)

df_w_yr <- data.frame(year = c(2001, 2002),
                      gss_code = c("E0901", "E0902"),
                      age = c(2,3),
                      sex = c("male", "female"))

df_w_yr <- as.data.frame(tidyr::complete(df_w_yr, year, gss_code, age, sex))
df_w_yr$rate <- (1:nrow(df_w_yr))/(5 * nrow(df_w_yr))

df_no_yr <- df_w_yr[, !names(df_w_yr) %in% "year"]

proj_df <- df_w_yr %>% filter(year == 2001) %>%
  tidyr::expand(year = c(2001, 2002, 2003), tidyr::nesting(gss_code, age, sex, rate)) %>%
  as.data.frame()

first_proj_yr <- 2002
hold_yr <- 2001
n_proj_yr <- 2

# project_forward_flat <- function(df, first_proj_yr, n_proj_yr, hold_yr)

test_that("project_forward_flat produces the expected output", {
  expect_equivalent(project_forward_flat(df_w_yr, first_proj_yr, n_proj_yr, hold_yr), proj_df)
})

test_that("error if df doesn't contain hold_yr", {
  expect_error(project_forward_flat(df_w_yr, first_proj_yr, n_proj_yr, 2003), "dataframe must contain the hold_year")
})

test_that("error if df doesn't contain hold_yr", {
  expect_error(project_forward_flat(df_w_yr, first_proj_yr, n_proj_yr, 2003), "dataframe must contain the hold_year")
})

test_that("error if hold_yr < first_proj_yr", {
  expect_error(project_forward_flat(df_w_yr, 2000, n_proj_yr, hold_yr), "first projection year must be later than the hold year")
})

