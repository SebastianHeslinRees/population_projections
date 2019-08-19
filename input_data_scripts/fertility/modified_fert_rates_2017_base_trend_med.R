library(dplyr)
library(tidyr)

fertility <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Outputs/2017 Base/medium_out_migration/Fertility Rates.rds") %>%
  mutate(sex = "female") %>% 
  select(year, gss_code, age, sex, rate = fertility_rate) %>%
  arrange(year, sex,gss_code, age) %>%
  as.data.frame()

# Update gss_code to 2018 to match MYE2018 and get rid of Scotland
fertility <- fertility %>%
  mutate(gss_code = case_when(
    gss_code == "E06000048" ~ "E06000057",
    gss_code == "E07000097" ~ "E07000242",
    gss_code == "E07000101" ~ "E07000243",
    gss_code == "E08000020" ~ "E08000037",
    TRUE ~ gss_code
  )) %>%
  filter(grepl("^E", gss_code))

# Make up some data for Wales to match MYE2018
wales <- filter(fertility, gss_code == "E06000001") %>%
  expand(gss_code = c("W06000001", "W06000002", "W06000003", "W06000004", "W06000005", "W06000006", "W06000008",
                      "W06000009", "W06000010", "W06000011", "W06000012", "W06000013", "W06000014", "W06000015",
                      "W06000016", "W06000018", "W06000019", "W06000020", "W06000021", "W06000022", "W06000023", "W06000024"),
         nesting(year, age, sex, rate)) %>%
  as.data.frame()


fertility  <- fertility %>% rbind(wales)

saveRDS(fertility, file = "input_data/fertility/modified_fert_rates_2017_base_trend_med.rds")
