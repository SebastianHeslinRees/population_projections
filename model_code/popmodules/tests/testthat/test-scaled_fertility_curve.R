library(popmodules)
library(testthat)
library(dplyr)
library(tidyr)
library(purrr)

# setwd("..")
# setwd("..")
# setwd("..")
# setwd("..")
# setwd("model_code/popmodules/tests/testthat")

popn_mye_path <- "test_data/test-popn-scaled-fert-curve.rds"
births_mye_path <- "test_data/test-births-scaled-fert-curve.rds"
target_curves_filepath <- "test_data/test-curves-scaled-fert-curve.rds"
birth_dom_filepath <- "test_data/test-birth_dom-scaled-fert-curve.rds"

last_data_year <- 2005
years_to_avg <- 3

# #------------------------------------------------
#
# #Data Creation
#
create_input_data <- function() {
  pop_data <- expand.grid(year = c(2001:2005),
                          gss_code = c("E0901", "E0902"),
                          age = c(0:90),
                          sex = c("male", "female"),
                          stringsAsFactors = F)

  pop_data$popn <- sample(c(20:100),nrow(pop_data), replace=T)

  birth_dom <- popn_age_on(pop_data) %>%
    filter(year != max(year)) %>%
    left_join(pop_data, by=c("gss_code","year","sex","age")) %>%
    mutate(popn = (popn.x + popn.y)/2)       %>%
    mutate(popn = ifelse(is.na(popn),0,popn)) %>%
    select(gss_code, year, sex, age, popn) %>%
    arrange(gss_code, year, sex, age)

  births <- expand.grid(year = c(2001:2005),
                        gss_code = c("E0901", "E0902"),
                        age = c(0:90),
                        sex = c("male", "female"),
                        stringsAsFactors = F)

  births$births <- sample(c(20:100),nrow(births), replace=T)
  births <- mutate(births, births = ifelse(age==0, births, 0))

  curves <- expand.grid(gss_code = c("E0901", "E0902"),
                        age = c(0:90),
                        sex = c("male", "female"),
                        year = 2006,
                        stringsAsFactors = F)
  curves$rate <- sample(seq(0.001:0.01, by=0.01),nrow(curves),replace=T)
  curves$rate <- ifelse(curves$age %in% c(0,90), 0, curves$rate)

  saveRDS(pop_data, popn_mye_path)
  saveRDS(births, births_mye_path)
  saveRDS(curves, target_curves_filepath)
  saveRDS(birth_dom, birth_dom_filepath)
}

# if(!file.exists(popn_mye_path)) {
#   message("Generating input files for first run")
#   create_input_data()
# }


regression <- function(df){
  lm(scaling ~ year, data=df)
}

get_coef <- function(df){
  coef(df)
}

pop_data <- readRDS(popn_mye_path)
births_data <- readRDS(births_mye_path)
curves <- readRDS(target_curves_filepath) %>%
  select(-year)

birth_dom <- filter(pop_data, sex == "female")

births_data <- group_by(births_data, year, gss_code) %>%
  summarise(births = sum(births), .groups = 'drop_last')

scaling_backseries <- popn_age_on(birth_dom, births = 0) %>%
  inner_join(curves, by = c("gss_code", "age", "sex")) %>%
  mutate(curve_births = rate * popn) %>%
  group_by(gss_code, year) %>%
  summarise(curve_births = sum(curve_births), .groups = 'drop_last') %>%
  ungroup() %>%
  inner_join(births_data, by = c("gss_code", "year")) %>%
  mutate(scaling = births / curve_births) %>%
  select(gss_code, year, scaling) %>%
  data.frame()

back_years <- c((last_data_year - years_to_avg + 1):last_data_year)

mean <- filter(scaling_backseries, year %in% back_years) %>%
  group_by(gss_code) %>%
  summarise(scaling = sum(scaling)/years_to_avg, .groups = 'drop_last') %>%
  data.frame() %>%
  mutate(year = last_data_year+1) %>%
  select(gss_code, year, scaling) %>%
  rbind(scaling_backseries)

mean <- curves %>%
  left_join(mean, by = c("gss_code")) %>%
  mutate(rate = scaling * rate) %>%
  select(gss_code, year, sex, age, rate) %>%
  arrange(gss_code, year, sex, age)

trend <- scaling_backseries %>%
  filter(year %in% back_years) %>%
  mutate(year = years_to_avg - last_data_year + year)%>%
  group_by(gss_code)%>%
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
  select(gss_code, year, scaling)  %>%
  rbind(scaling_backseries)

trend <- curves %>%
  left_join(trend, by = c("gss_code")) %>%
  mutate(rate = scaling * rate) %>%
  select(gss_code, year, sex, age, rate)  %>%
  arrange(gss_code, year, sex, age)

#-----------------------------------------------------------

test_that("scaled_fertility_curve function: test with mean", {
  expect_equivalent(
    scaled_fertility_curve(popn_mye_path, births_mye_path,
                           target_curves_filepath, last_data_year,
                           years_to_avg, avg_or_trend="average",
                           data_col = "births", output_col = "rate") %>%
      arrange(gss_code, year, sex, age),
    mean)
})


test_that("scaled_fertility_curve function: test with regression", {
  expect_equivalent(
    scaled_fertility_curve(popn_mye_path, births_mye_path,
                      target_curves_filepath, last_data_year,
                      years_to_avg, avg_or_trend="trend",
                      data_col = "births", output_col = "rate") %>%
      arrange(gss_code, year, sex, age),
    trend)
})

