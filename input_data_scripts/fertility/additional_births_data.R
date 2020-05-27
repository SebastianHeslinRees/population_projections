# Script to incorporate the 2019 births data in the model.
# This exists because we have 2019 births data before any of the other 2019 MYE data.
# The script uses this to create a UK-wide fertility trajectory to 2050 with the 2019
# rates incuded in the calculations (for London only).

# The output can then be used as model input by pointing the additional_births_path
# parameter to it in the housing led config script

# TODO adapt this for the trend model as well

library(dplyr)
devtools::load_all('model_code/popmodules')

popn <- readRDS("input_data/mye/2018/population_gla_2019-11-13.rds")

ons_births <- readRDS("input_data/mye/2018/births_ons.rds")

additional_births <- data.table::fread("Q:/Teams/D&PA/Data/births_and_deaths/births_borough_mid_2019.csv") %>%
  as.data.frame() %>%
  select(gss_code, sex, births) %>%
  # recode_gss_codes(col_geog = "gss_code",
  #                  data_cols = "births",
  #                  fun = list(sum),
  #                  recode_to_year = 2018) %>%
  filter(gss_code %in% ons_births$gss_code) %>%
  validate_same_geog(ons_births) %>%
  filter(substr(gss_code,1,3)=="E09")

#Use 2018 births for city
city <- data.frame(gss_code = "E09000001", sex = c("male","female"), births = c(38,28))

#Hackney
hackney <- filter(additional_births, gss_code == "E09000012,E09000001") %>% 
  mutate(births = ifelse(sex == "male", births - 38, births - 28),
         gss_code = "E09000012")

#Finish
additional_births <- filter(additional_births, gss_code != "E09000012,E09000001") %>%
  rbind(city, hackney) %>%
  mutate(age = 0,
         year = 2019) %>%
  select(year, gss_code, age, sex, births)

rm(city, hackney)
saveRDS(additional_births, "input_data/fertility/births_2019.rds")

london_backseries_births <- filter(ons_births, gss_code %in% unique(additional_births$gss_code)) %>%
  filter(age == 0) %>%
  select(names(additional_births)) %>%
  rbind(additional_births)

not_london_backseries_births <- filter(ons_births, !gss_code %in% unique(additional_births$gss_code)) %>%
  filter(age == 0) %>%
  select(names(additional_births)) 

for(method in c("average", "trend")) {
  london_fert_rates <- scaled_fertility_curve(popn_mye_path = popn,
                                              births_mye_path = london_backseries_births,
                                              target_curves_filepath = "input_data/fertility/ons_asfr_curves_2018.rds",
                                              last_data_year = 2019,
                                              n_years_to_avg = 5,
                                              avg_or_trend = method,
                                              data_col = "births",
                                              output_col = "rate") %>%
    project_rates_npp(rate_col = "rate",
                      rate_trajectory_filepath = "input_data/fertility/npp_fertility_trend.rds",
                      first_proj_yr = 2020,
                      n_proj_yr = 31,
                      npp_var = "2018_principal")
  
  not_london_fert_rates <- scaled_fertility_curve(popn_mye_path = popn,
                                                  births_mye_path = not_london_backseries_births,
                                                  target_curves_filepath = "input_data/fertility/ons_asfr_curves_2018.rds",
                                                  last_data_year = 2018,
                                                  n_years_to_avg = 5,
                                                  avg_or_trend = method,
                                                  data_col = "births",
                                                  output_col = "rate") %>%
    project_rates_npp(rate_col = "rate",
                      rate_trajectory_filepath = "input_data/fertility/npp_fertility_trend.rds",
                      first_proj_yr = 2019,
                      n_proj_yr = 32,
                      npp_var = "2018_principal")
  
  fertility_rates <- rbind(london_fert_rates, not_london_fert_rates) %>% 
    complete_fertility(popn)
  
  filename_suffix <- ifelse(method == "trend", "_5yr_trend", "")
  saveRDS(fertility_rates, paste0("input_data/fertility/fertility_rates_inc_2019_in_london",filename_suffix,".rds"))
}