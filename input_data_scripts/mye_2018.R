library(tidyverse)

coc_file <- "Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2018/detailed_mye_coc.rds"

mye_coc <- readRDS(coc_file) %>%
  rename(gss_code = ladcode18, gss_name = laname18) %>%
  mutate(geography = "LAD18", sex = case_when(sex == 1 ~ "male", sex == 2 ~ "female"), year = as.integer(year)) %>%
  as.data.frame()

births <- filter(mye_coc, component == "births") %>% select(-component)
deaths <- filter(mye_coc, component == "deaths") %>% select(-component)
population <- filter(mye_coc, component == "population") %>% select(-component)
international_in <- filter(mye_coc, component == "interational_in") %>% select(-component)
international_out <- filter(mye_coc, component == "international_out") %>% select(-component)
international_net <- filter(mye_coc, component == "international_net") %>% select(-component)
domestic_in <- filter(mye_coc, component == "internal_in") %>% select(-component)
domestic_out <- filter(mye_coc, component == "internal_out") %>% select(-component)
domestic_net <- filter(mye_coc, component == "internal_net") %>% select(-component)


# interpolate points where deaths == -1
# FIXME this is a clumsy way to do it but everything with grouping was taking a million years, and no 2018 data are missing
deaths <- arrange(deaths, gss_code, age, sex, year)
ix <- deaths$value < 0
deaths$value[ix] <- NA
missing_deaths <- approx(x = 1:nrow(deaths), y = deaths$value, xout = (1:nrow(deaths))[ix])
deaths$value[ix] <- missing_deaths$y
deaths <- arrange(deaths, gss_code, year, sex, age)


# TODO: CALL SOME DATA CHECKING FUNCTIONS HERE

datestamp <- Sys.Date()

saveRDS(births, file = paste0("input_data/mye/2018/births_ons_", datestamp, ".rds"))
saveRDS(deaths, file = paste0("input_data/mye/2018/deaths_ons_", datestamp, ".rds"))
saveRDS(population, file = paste0("input_data/mye/2018/population_ons_", datestamp, ".rds"))
saveRDS(international_in, file = paste0("input_data/mye/2018/international_in_ons_", datestamp, ".rds"))
saveRDS(international_out, file = paste0("input_data/mye/2018/international_out_ons_", datestamp, ".rds"))
saveRDS(international_net, file = paste0("input_data/mye/2018/international_net_ons_", datestamp, ".rds"))
saveRDS(domestic_in, file = paste0("input_data/mye/2018/domestic_in_ons_", datestamp, ".rds"))
saveRDS(domestic_out, file = paste0("input_data/mye/2018/domestic_out_ons_", datestamp, ".rds"))
saveRDS(domestic_net, file = paste0("input_data/mye/2018/domestic_net_ons_", datestamp, ".rds"))

