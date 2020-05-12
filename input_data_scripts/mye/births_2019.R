library(data.table)
library(dplyr)
devtools::load_all("model_code/popmodules")

#2019 Births

births_1 <- fread("Q:/Teams/D&PA/Data/births_and_deaths/births_sept2017_aug2018.csv") %>%
  data.frame() %>% 
  select(gss_code, sex, Jul_2018, Aug_2018) %>%
  tidyr::pivot_longer(cols = c(Jul_2018, Aug_2018), names_to = "month", values_to = "births")
births_2 <- fread("Q:/Teams/D&PA/Data/births_and_deaths/births_sept2018_aug2019.csv")%>%
  data.frame() %>% 
  select(-Jul_2019, -Aug_2019) %>%
  tidyr::pivot_longer(cols = 3:12, names_to = "month", values_to = "births")

births_2019 <- rbind(births_1, births_2) %>%
  group_by(gss_code, sex) %>%
  summarise(births = sum(births)) %>%
  data.frame() %>%
  popmodules::recode_gss_to_2011(col_aggregation = c("gss_code","sex")) %>%
  filter(substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06","N92","S92"))

hackney_city <- filter(births_2019, gss_code == "E09000012, E09000001")
hackney <- hackney_city %>%
  mutate(gss_code = "E09000012",
         births = ifelse(sex == "male", births - 33, births - 28))
city <- data.frame(gss_code = "E09000001",
                   sex = c("female","male"),
                   births = c(28,33))

cornwall_scilly <- filter(births_2019, gss_code == "E06000052, E06000053")
cornwall <- cornwall_scilly %>%
  mutate(gss_code = "E06000052",
         births = ifelse(sex == "male", births - 6, births - 8))
scilly <- data.frame(gss_code = "E06000053",
                   sex = c("female","male"),
                   births = c(8,6))

nireland <- data.frame(gss_code = rep("N92000002",2),
                       sex = c("male","female"),
                       births = c(22647*(105/205),
                                  22647*(100/205)))

scotland <- data.frame(gss_code = rep("S92000003",2),
                       sex = c("male","female"),
                       births = c(50636*(105/205),
                                  50636*(100/205)))

births_2019 <- filter(births_2019, !gss_code %in% c("E09000012, E09000001",
                                                    "E06000052, E06000053")) %>%
  rbind(hackney, city, cornwall, scilly, nireland, scotland) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","sex"))

births_2018 <- readRDS("input_data/mye/2018/births_ons.rds")  %>%
  select(-gss_name, -country, -geography) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","year","sex","age"))

validate_same_geog(births_2019, births_2018)

mye_births <- births_2019 %>%
  mutate(year = 2019,
         age = 0) %>%
  complete_fertility(births_2018, col_rate = "births") %>%
  rbind(births_2018) %>%
  select(year, gss_code, sex, age, births)

saveRDS(mye_births, "input_data/mye/2019/temp_births.rds")
  
rm(list=ls())