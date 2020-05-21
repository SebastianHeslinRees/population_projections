###UPC
library(dplyr)
devtools::load_all('model_code/popmodules')

#Read in data
#Covid19 deaths for UK by age group and sex
covid_uk_age_sex <- data.table::fread("input_data/scenario_data/covid19_uk_age_sex.csv") %>%
  as.data.frame() %>%
  filter(sex %in% c("male","female"))

#Covid19 total deaths by LA and sex
#Recode gss codes so they're consitent with othre inputs
covid_la_total <- data.table::fread("input_data/scenario_data/covid19_totals_LA.csv") %>%
  as.data.frame() %>%
  filter(sex %in% c("male","female"),
         substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06","S92","N92")) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","sex"), year=2020)

#2018 Mid-year estimates for UK by sya and sex
uk_2018_deaths <- readRDS("input_data/mye/2018/deaths_ons.rds") %>%
  filter(year == 2018) %>%
  group_by(sex, age) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()


#-----------------------------------------

#Apply 2018 age structure to covid19 age group data
uk_covid_deaths_sya <- list()
for(i in unique(covid_uk_age_sex$age_group)){
  
  a <- filter(covid_uk_age_sex, age_group ==i)
  uk_covid_deaths_sya[[i]] <- distribute_within_age_band(a,
                                                uk_2018_deaths,
                                                "deaths", "deaths",
                                                unique(a$min), unique(a$max),
                                                col_aggregation=c("sex"))
}

uk_covid_deaths_sya <- data.table::rbindlist(uk_covid_deaths_sya) %>%
  as.data.frame() %>% 
  select(sex, age, deaths)

#deaths as a proportion of total by sex
uk_covid_mort_structure <- uk_covid_deaths_sya %>%
  group_by(sex) %>%
  mutate(structure = deaths/sum(deaths)) %>%
  as.data.frame() %>% 
  select(-deaths)

#Apply the age structure to the LA/sex data
covid_modelled_sya <- left_join(covid_la_total, uk_covid_mort_structure, by="sex") %>%
  mutate(upc = deaths*structure,
         year = 2020) %>%
  select(year, gss_code, sex, age, upc)%>%
  constrain_component(deaths_sya, col_aggregation = c("sex","age"),
                      col_popn = "upc", col_constraint = "deaths")


#save

saveRDS(covid_modelled_sya, "input_data/scenario_data/covid19_upc.rds")

