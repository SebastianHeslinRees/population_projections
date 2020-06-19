library(dplyr)
library(tidyr)

int_out_rates <- readRDS("Q:/Teams/D&PA/Demography/Projections/Legacy Models/Trend Model/Outputs/2017 Base/medium_out_migration/International Out Probabilities.rds") %>%
  select(year, gss_code, age, sex, rate = int_out) %>%
  arrange(year, sex,gss_code, age) %>%
  as.data.frame()

# Update gss_code to 2018 to match MYE2018 and get rid of Scotland
int_out_rates <- int_out_rates %>%
 # filter(grepl("^E", gss_code)) %>%
  # mutate(gss_code = case_when(
  #   gss_code == "E06000048" ~ "E06000057",
  #   gss_code == "E07000097" ~ "E07000242",
  #   gss_code == "E07000101" ~ "E07000243",
  #   gss_code == "E08000020" ~ "E08000037",
  #   TRUE ~ gss_code
  # ),
  filter(gss_code != "W92000004") %>%
  mutate(sex = case_when(
    sex == "M" ~ "male",
    sex == "F" ~ "female"
  ))

# Make up some data for Wales to match MYE2018
wales <- filter(int_out_rates, gss_code == "E06000001") %>%
  expand(gss_code = c("W06000001", "W06000002", "W06000003", "W06000004", "W06000005", "W06000006", "W06000008",
                      "W06000009", "W06000010", "W06000011", "W06000012", "W06000013", "W06000014", "W06000015",
                      "W06000016", "W06000018", "W06000019", "W06000020", "W06000021", "W06000022", "W06000023", "W06000024"),
         nesting(year, age, sex, rate)) %>%
  as.data.frame()


int_out_rates  <- int_out_rates %>% rbind(wales) %>%
  popmodules::recode_gss_codes(data_cols = "int_out",
                               recode_gla_codes = TRUE)

saveRDS(int_out_rates, file = "input_data/migration/modified_int_out_rates_2017_base_trend_med.rds")
