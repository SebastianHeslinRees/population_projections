library(dplyr)
library(tidyr)
library(assertthat)

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/full_machine_readable.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la) %>%
  mutate(gss_out = case_when(
    gss_out == "E06000048" ~ "E06000057",
    gss_out == "E07000097" ~ "E07000242",
    gss_out == "E07000101" ~ "E07000243",
    gss_out == "E08000020" ~ "E08000037",
    TRUE ~ gss_out
  )) %>%
  mutate(gss_in = case_when(
    gss_in == "E06000048" ~ "E06000057",
    gss_in == "E07000097" ~ "E07000242",
    gss_in == "E07000101" ~ "E07000243",
    gss_in == "E08000020" ~ "E08000037",
    TRUE ~ gss_in
  ))


dir.create("input_data/domestic_migration/", showWarnings = T)
dir.create("input_data/domestic_migration/2018", showWarnings = T)

datestamp <- Sys.Date()

# Save absolute values
message("Saving domestic origin-destination flows. This may take a while")
saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))



# Calculate net flows
message("Calculating historic net flows. This may also take a while")
domestic <- domestic %>%
  mutate(year = as.integer(year))

dom_out <- domestic %>%
  group_by(year, gss_out, sex, age) %>%
  summarise(dom_out = sum(value)) %>%
  rename(gss_code = gss_out)
assert_that(all(complete.cases(dom_out)))

dom_in <- domestic %>%
  group_by(year, gss_in, sex, age) %>%
  summarise(dom_in = sum(value)) %>%
  rename(gss_code = gss_in)
assert_that(all(complete.cases(dom_in)))

dom_net <- full_join(dom_out, dom_in, by = c("year", "gss_code", "sex", "age")) %>%
  replace_na(list(dom_out = 0, dom_in = 0)) %>%
  complete(year=2002:2018, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
  mutate(dom_net = dom_in - dom_out)


saveRDS(dom_net, file = paste0("input_data/domestic_migration/2018/domestic_migration_net_", datestamp, ".rds"))

  


# Convert to rates
message("Calculating 5-year origin-destination migration rates. This may also take a whiiile")

mye <- readRDS("input_data/mye/2018/population_ons_2019-08-27.rds") %>%
  filter(year >= 2014)

domestic <- domestic %>%
  filter(year >= 2014) %>%
  left_join(mye, by = c("year", "gss_out"="gss_code", "age", "sex")) %>%
  mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
  select(year, gss_out, gss_in, sex, age, rate) %>%
  group_by(gss_out, gss_in, sex, age) %>%
  summarise(rate = mean(rate))

message("Saving 5-year origin-destination migration rates. This shouldn't take quite as long")
saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_rate_5year_", datestamp, ".rds"))
