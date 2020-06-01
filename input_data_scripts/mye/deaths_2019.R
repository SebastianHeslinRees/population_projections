library(dplyr)
devtools::load_all("model_code/popmodules")

deaths_2018 <- data.table::fread("Q:/Teams/D&PA/Data/births_and_deaths/deaths_calendar_2018.csv",
                                 header = TRUE) %>%
  data.frame() %>%
  select(gss_code, Jul, Aug, Sep, Oct, Nov, Dec) %>%
  tidyr::pivot_longer(2:7, "month")

deaths_2019 <- data.table::fread("Q:/Teams/D&PA/Data/births_and_deaths/deaths_calendar_2019.csv",
                                 header = TRUE) %>%
  data.frame() %>%
  select(gss_code, Jan, Feb, Mar, Apr, May, Jun) %>%
  tidyr::pivot_longer(2:7, "month")

total_deaths <- rbind(deaths_2018, deaths_2019) %>% 
  filter(substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06")) %>%
  group_by(gss_code) %>%
  summarise(total_deaths = sum(value)) %>% 
  as.data.frame() %>%
  recode_gss_codes(data_cols="deaths", recode_to_year = 2020) %>%
  rbind(data.frame(gss_code = c("N92000002","S92000003"),
                   total_deaths = c(15349, 56209)))
	

deaths_backseries <- readRDS("input_data/mye/2018/deaths_ons.rds") %>%
  select(gss_code, year, sex, age, deaths) %>% 
  recode_gss_codes(data_cols="deaths", recode_to_year = 2020) 

sya_deaths_2018 <- deaths_backseries %>%
  filter(year == 2018) 

scaled_deaths <- sya_deaths_2018 %>% 
  group_by(gss_code) %>%
  mutate(proportion = deaths / sum(deaths)) %>%
  data.frame() %>% 
  left_join(total_deaths, by="gss_code") %>%
  mutate(deaths = proportion * total_deaths) %>%
  mutate(year = 2019) %>%
  select(gss_code, year, sex, age, deaths)

deaths_final <- rbind(deaths_backseries, scaled_deaths) %>%
  select(year, gss_code, sex, age, deaths)


saveRDS(deaths_final, "input_data/mye/2019/temp_deaths.rds")

rm(list=ls())