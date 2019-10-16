context("project_mortality_probs")
library(popmodules)
library(testthat)
library(dplyr)
library(tidyr)

jump <- data.frame(year = 2001,
                      gss_code = c("E0901", "E0902"),
                      age = c(2,3),
                      sex = c("male", "female"),
                   stringsAsFactors = F)

jump <- as.data.frame(tidyr::complete(jump, year, gss_code, age, sex))
jump$death_rate <- (1:nrow(jump))/(5 * nrow(jump))

trend <- expand.grid(year = c(2001:2005),
                    age = c(2,3),
                    sex = c("male", "female"),
                    variant = "2016_principal",
                    stringsAsFactors = F)
x <- seq(0,1,(1/nrow(trend)))[-1]
trend$change <- x

proj_df <- filter(trend, year != 2001) %>%
  arrange(year) %>%
  group_by(sex, age) %>%
  mutate(change = change + 1,
         cumprod = cumprod(change))%>%
  ungroup() %>%
  arrange(age,sex,year)%>%
  left_join(select(jump, -year), by=c("sex","age")) %>%
  mutate(rate = cumprod*death_rate) %>%
  select(gss_code, sex, age, year, rate)%>%
  arrange(gss_code,sex,age,year)

first_proj_yr <- 2001
n_proj_yr <- 5
npp_var <- "2016_principal"

test_that("project_rates produces the expected output", {
  expect_equivalent(project_rates(jump, "death_rate", trend, first_proj_yr, n_proj_yr, npp_var),
                    proj_df)
})
