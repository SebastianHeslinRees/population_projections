context("project_rates_npp")
library(popmodules)
library(testthat)
library(dplyr)

rate_trajectory_filepath <- "test_data/test_project_rates_npp_data.rds"

trend <- expand.grid(sex = c("female","male"), age = 21:22, year = 2010:2020, variant = "2018_principal") %>%
  arrange(sex, age, variant) %>%
  mutate(change = 0.1 * (year - 2009))
saveRDS(trend, rate_trajectory_filepath)

jump_off <- expand.grid(year = 2012, gss_code = c("E01","E02"), sex = c("female","male"), age = 21:22, test_rate = 0.1)

proj <- filter(trend, year > 2012) %>%
  arrange(year) %>%
  group_by(sex, age) %>%
  mutate(change = 1+change,
         cumprod = cumprod(change)) %>%
  ungroup() %>%
  left_join(select(jump_off, -year), by=c("sex","age")) %>%
  mutate(test_rate = test_rate * cumprod) %>%
  select(names(jump_off)) %>%
  rbind(jump_off) %>%
  arrange(gss_code, sex, age, year) %>%
  select(gss_code, sex, age, year, test_rate) %>%
  data.frame()

rate_trajectory_filepath <- "test_data/test_ptoject_rates_npp_data.rds"
x <- project_rates_npp(jump_off, rate_col="test_rate", rate_trajectory_filepath, first_proj_yr=2012, n_proj_yr=9, npp_var="2018_principal")

testthat::test_that("project_rates_npp produces the expected output",
                    expect_equal(x, proj))

file.remove(rate_trajectory_filepath)






