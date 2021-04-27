library(popmodules)
library(testthat)
library(dplyr)
library(tidyr)
library(purrr)

# setwd("model_code/popmodules/tests/testthat")
#
popn_mye_path <- "test_data/test-popn-scaled-mort-curve.rds"
births_mye_path <- "test_data/test-births-scaled-mort-curve.rds"
deaths_mye_path <- "test_data/test-deaths-scaled-mort-curve.rds"
target_curves_filepath <- "test_data/test-curves-scaled-mort-curve.rds"

last_data_year <- 2005
years_to_avg <- 3

# #------------------------------------------------

regression <- function(df){
  lm(scaling ~ year, data=df)
}

get_coef <- function(df){
  coef(df)
}

# pop_data <- expand.grid(year = c(2001:2005),
#                         gss_code = c("E0901", "E0902"),
#                         age = c(0:90),
#                         sex = c("male", "female"),
#                         stringsAsFactors = F)
#
# pop_data$popn <- sample(c(20:100),nrow(pop_data), replace=T)
#
# births <- expand.grid(year = c(2001:2005),
#                       gss_code = c("E0901", "E0902"),
#                       age = c(0:90),
#                       sex = c("male", "female"),
#                       stringsAsFactors = F)
#
# births$births <- sample(c(20:100),nrow(births), replace=T)
# births <- mutate(births, births = ifelse(age==0, births, 0))
#
# deaths <- expand.grid(year = c(2001:2005),
#                       gss_code = c("E0901", "E0902"),
#                       age = c(0:90),
#                       sex = c("male", "female"),
#                       stringsAsFactors = F)
# deaths$deaths <- sample(c(5:15),nrow(deaths),replace=T)
#
# curves <- expand.grid(gss_code = c("E0901", "E0902"),
#                       age = c(0:90),
#                       sex = c("male", "female"),
#                       year = 2005,
#                       stringsAsFactors = F)
# curves$rate <- sample(seq(0.001:0.01, by=0.01),nrow(curves),replace=T)
#
# saveRDS(pop_data, popn_mye_path)
# saveRDS(births, births_mye_path)
# saveRDS(deaths, deaths_mye_path)
# saveRDS(curves, target_curves_filepath)

pop_data <- readRDS(popn_mye_path)
births <- readRDS(births_mye_path)
deaths <- readRDS(deaths_mye_path)
curves <- readRDS(target_curves_filepath)

aged <- pop_data %>%
  mutate(year = year + 1) %>%
  mutate(age = ifelse(age == 90, 90, age + 1)) %>%
  group_by(gss_code, age, sex, year) %>%
  summarise(popn = sum(popn), .groups = 'drop_last') %>%
  data.frame() %>%
  filter(year != max(year))

aged <- filter(births, age==0, year %in% aged$year) %>%
  select(gss_code, age, sex, year, popn = births) %>%
  rbind(aged) %>%
  select(gss_code, year, sex, age, popn) %>%
  arrange(gss_code, year, sex, age)

curves <- select(curves, -year)

scaling_backseries <- left_join(aged, curves, by = c("gss_code", "age", "sex")) %>%
  mutate(curve_deaths = rate * popn) %>%
  left_join(deaths, by = c("gss_code", "age", "sex", "year")) %>%
  group_by(gss_code, year, sex) %>%
  summarise(actual_deaths = sum(deaths),
            curve_deaths = sum(curve_deaths),
            .groups = 'drop_last') %>%
  mutate(scaling = actual_deaths / curve_deaths) %>%
  select(gss_code, year, sex, scaling) %>%
  data.frame()

back_years <- c((last_data_year - years_to_avg + 1):last_data_year)

mean <- filter(scaling_backseries, year %in% back_years) %>%
  group_by(gss_code, sex) %>%
  summarise(scaling = sum(scaling)/years_to_avg, .groups = 'drop_last') %>%
  data.frame() %>%
  mutate(year = last_data_year+1) %>%
  select(gss_code, sex, year, scaling) %>%
  rbind(scaling_backseries) %>%
  filter(year %in% (pop_data$year + 1))

mean <- curves %>%
  left_join(mean, by = c("gss_code", "sex")) %>%
  mutate(rate = scaling * rate) %>%
  select(gss_code, year, sex, age, rate) %>%
  arrange(gss_code, year, sex, age)

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
  select(gss_code, sex, year, scaling) %>%
  rbind(scaling_backseries) %>%
  filter(year %in% (pop_data$year + 1))

trend <- curves %>%
  left_join(trend, by = c("gss_code", "sex")) %>%
  mutate(rate = scaling * rate) %>%
  select(gss_code, year, sex, age, rate) %>%
  arrange(gss_code, year, sex, age)


#-----------------------------------------------------------
#
# test_that("deaths_denominator in scaled_mortality_curve function",{
#   expect_equivalent(
#     deaths_denominator(pop_data, births), aged)
# })

# test_that("births_denominator produces the expected output",{
#   expect_equivalent(
#     births_denominator(pop_data), birth_dom)
# })


test_that("scaled_mortality_curve function: test with mean", {
  expect_equivalent(
    scaled_mortality_curve(popn_mye_path, births_mye_path, deaths_mye_path,
                           target_curves_filepath, last_data_year,
                           years_to_avg, avg_or_trend="average",
                           data_col = "deaths", output_col = "rate"),
    mean)
})


test_that("scaled_mortality_curve function: test with regression", {
  expect_equivalent(
    scaled_mortality_curve(popn_mye_path, births_mye_path, deaths_mye_path,
                      target_curves_filepath, last_data_year,
                      years_to_avg, avg_or_trend="trend",
                      data_col = "deaths", output_col = "rate"),
    trend)
})

