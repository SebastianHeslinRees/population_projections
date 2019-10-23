# context("project_mortality_probs")
# library(popmodules)
# library(testthat)
# library(dplyr)
# library(tidyr)
# 
# first_proj_yr <- 2019
# n_proj_yr <- 5
# npp_var <- "2018_principal"
# npp_path <- "M:/projects/population_projections/input_data/mortality/npp_mortality_trend.rds"
# 
# jump <- data.frame(year = 2019,
#                       gss_code = c("E0901", "E0902"),
#                       age = c(2,3),
#                       sex = c("male", "female"),
#                    stringsAsFactors = F)
# 
# jump <- as.data.frame(tidyr::complete(jump, year, gss_code, age, sex))
# jump$death_rate <- (1:nrow(jump))/(5 * nrow(jump))
# 
# trend <- readRDS(npp_path) %>%
#   filter(year != 2001) %>%
#   filter(variant == npp_var) %>%
#   arrange(year) %>%
#   group_by(sex, age) %>%
#   mutate(change = change + 1,
#          cumprod = cumprod(change))%>%
#   ungroup() %>%
#   arrange(age,sex,year)%>%
#   left_join(select(jump, -year), by=c("sex","age")) %>%
#   mutate(death_rate = cumprod*death_rate) %>%
#   select(gss_code, sex, age, year, death_rate)%>%
#   arrange(gss_code,sex,age,year) %>%
#   filter(year <= first_proj_yr+n_proj_yr-1)
# 
# 
# 
# test_that("project_rates_npp produces the expected output", {
#   expect_equivalent(project_rates_npp(jump, "death_rate", npp_path, first_proj_yr, n_proj_yr, npp_var),
#                     trend)
# })
# 
# x <- project_rates_npp(jump, "death_rate", npp_path, first_proj_yr, n_proj_yr, npp_var)%>%
#   arrange(gss_code,sex,age,year) 
