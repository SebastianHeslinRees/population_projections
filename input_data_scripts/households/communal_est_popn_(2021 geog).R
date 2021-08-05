library(dplyr)

message('communal establishment populations')

ons_data_location <- "Q:/Teams/D&PA/Data/household_projections/ONS_data/2018_based/2018_principal/csv/"

ce <- rbind(data.table::fread(paste0(ons_data_location, "ce_population_female.csv"), header = T),
            data.table::fread(paste0(ons_data_location, "ce_population_male.csv"), header = T)) %>%
  tibble() %>%
  tidyr::pivot_longer(5:47, names_to = "year", values_to = "ce_pop") %>%
  mutate(year = as.numeric(year),
         sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male")) %>%
  select(-district) %>%
  filter(age_group != "All ages") %>% 
  popmodules::recode_gss_codes(data_cols = "ce_pop",
                               col_geog="gss_code",
                               recode_to_year = 2021)

hh_pop <- rbind(data.table::fread(paste0(ons_data_location, "hh_population_female.csv"), header = T),
                data.table::fread(paste0(ons_data_location, "hh_population_male.csv"), header = T)) %>%
  tibble() %>%
  tidyr::pivot_longer(5:47, names_to = "year", values_to = "hh_pop") %>%
  mutate(year = as.numeric(year),
         sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male")) %>%
  select(-district) %>%
  filter(age_group != "All ages") %>% 
  popmodules::recode_gss_codes(data_cols = "hh_pop",
                               col_geog="gss_code",
                               recode_to_year = 2021)

communal_est <- left_join(ce, hh_pop, by = c("gss_code", "age_group", "sex", "year")) %>%
  mutate(ce_rate = ifelse(age_group %in% c("75_79","80_84","85_89","90+"), ce_pop/(ce_pop+hh_pop), NA)) %>%
  select(-hh_pop)

saveRDS(communal_est,"input_data/household_model/ons_communal_establishment_population_(2021_geog).rds")

