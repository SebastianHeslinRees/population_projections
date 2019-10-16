context("average_mortality_rates")
library(popmodules)
library(testthat)
library(dplyr)
library(tidyr)
library(purrr)


regression <- function(df){
  lm(scaling ~ year, data=df)
}

get_coef <- function(df){
  coef(df)
}

pop_data <- expand.grid(year = c(2001:2005),
                        gss_code = c("E0901", "E0902"),
                        age = c(0:90),
                        sex = c("male", "female"),
                        stringsAsFactors = F)

pop_data$popn <- sample(c(20:100),nrow(pop_data), replace=T)

births <- expand.grid(year = c(2001:2005),
                      gss_code = c("E0901", "E0902"),
                      age = c(0:90),
                      sex = c("male", "female"),
                      stringsAsFactors = F)

births$births <- sample(c(20:100),nrow(births), replace=T)
births <- mutate(births, births = ifelse(age==0, births, 0))

deaths <- expand.grid(year = c(2001:2005),
                      gss_code = c("E0901", "E0902"),
                      age = c(0:90),
                      sex = c("male", "female"),
                      stringsAsFactors = F)
deaths$deaths <- sample(c(5:15),nrow(deaths),replace=T)

curves <- expand.grid(gss_code = c("E0901", "E0902"),
                      age = c(0:90),
                      sex = c("male", "female"),
                      stringsAsFactors = F)
curves$death_rate <- sample(seq(0.001:0.01, by=0.01),nrow(curves),replace=T)

aged <- pop_data %>%
  mutate(year = year + 1) %>%
  mutate(age = ifelse(age == 90, 90, age + 1)) %>%
  group_by(gss_code, age, sex, year) %>%
  summarise(popn = sum(popn)) %>%
  data.frame() %>%
  filter(year != max(year)) %>%
  rbind(filter(births, age==0) %>% select(gss_code, age, sex, year, popn = births))

scaling_backseries <- left_join(aged, curves, by = c("gss_code", "age", "sex")) %>%
  mutate(curve_deaths = death_rate * popn) %>%
  left_join(deaths, by = c("gss_code", "age", "sex", "year")) %>%
  group_by(gss_code, year, sex) %>%
  summarise(actual_deaths = sum(deaths),
            curve_deaths = sum(curve_deaths)) %>%
  mutate(scaling = ifelse(actual_deaths == 0,
                          0,
                          actual_deaths / curve_deaths)) %>%
  select(gss_code, year, sex, scaling) %>%
  data.frame()


last_data_year <- 2005
years_to_avg <- 3

back_years <- c((last_data_year - years_to_avg + 1):last_data_year)


mean <- filter(scaling_backseries, year %in% back_years) %>%
  group_by(gss_code, sex) %>%
  summarise(scaling = sum(scaling)/years_to_avg) %>%
  data.frame() %>%
  mutate(year = last_data_year+1) %>%
  select(gss_code, sex, year, scaling)

mean <- curves %>%
  left_join(mean, by = c("gss_code", "sex")) %>%
  mutate(death_rate = scaling * death_rate)

trend <- scaling_backseries %>%
  filter(year %in% back_years) %>%
  mutate(year = years_to_avg - last_data_year + year)%>%
  group_by(gss_code, sex)%>%
  tidyr::nest() %>%
  mutate(
    Model = map(data, regression),
    Coef = Model %>% map(get_coef),
    Intercept = Coef %>% map_dbl("(Intercept)"),
    Slope = Coef %>% map_dbl("year"),
    scaling = Slope * (years_to_avg+1) + Intercept,
    scaling = ifelse(scaling < 0, 0, scaling)) %>%
  as.data.frame()  %>%
  mutate(year = last_data_year + 1) %>%
  select(gss_code, sex, year, scaling)

trend <- curves %>%
  left_join(trend, by = c("gss_code", "sex")) %>%
  mutate(death_rate = scaling * death_rate)


test_that("project_mortality_rates produces the expected output", {
  expect_equivalent(
    initial_year_mortality_rate(pop_data, births, deaths, curves,
                                last_data_year, years_to_avg, avg_or_trend="average"),
    mean)
})


test_that("project_mortality_rates produces the expected output", {
  expect_equivalent(
    initial_year_mortality_rate(pop_data, births, deaths, curves,
                                last_data_year, years_to_avg, avg_or_trend="trend"),
    trend)
})

