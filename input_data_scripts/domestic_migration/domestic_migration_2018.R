library(dplyr)
library(tidyr)
library(assertthat)
library(data.table)
devtools::load_all("model_code/popmodules")

rm(list=ls()) # we're going to need memory, sorry
message("domestic migration")

region_lookup <- readRDS("input_data/lookup/district_to_region.rds")

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/2002-2018 but codes not changed.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la)

domestic <- domestic %>%
  recode_gss_codes(col_geog="gss_in", data_col="value", fun=list(sum), recode_gla_codes = TRUE) %>%
  recode_gss_codes(col_geog="gss_out", data_col="value", fun=list(sum), recode_gla_codes = TRUE)

domestic_region <- domestic %>%
  left_join(region_lookup, by=c("gss_in"="gss_code")) %>%
  select(-gss_in) %>%
  rename(gss_in = region_gss_code) %>%
  left_join(region_lookup, by=c("gss_out"="gss_code")) %>%
  select(-gss_out) %>% 
  rename(gss_out = region_gss_code) %>%
  dtplyr::lazy_dt() %>%
  filter(gss_in != gss_out) %>%
  group_by(gss_in, gss_out, age, sex, year) %>%
  summarise(value = sum(value)) %>%
  as.data.frame()

domestic <- rbind(domestic, domestic_region)

rm(domestic_region)

dir.create("input_data/domestic_migration/", showWarnings = F)
dir.create("input_data/domestic_migration/2018", showWarnings = F)

datestamp <- Sys.Date()

# The dataset's so big we'd better use data.table here (sorry)
data.table::setDT(domestic)
domestic <- domestic[, year := as.integer(year)
                     ][, age := ifelse(age < 90, age, 90)
                     ][, .(value = sum(value)), .(gss_out, gss_in, year, sex, age)]
data.table::setDF(domestic)

if(FALSE) { # tidyverse equivalent
  domestic <- domestic %>%
    mutate(year = as.integer(year)) %>%
    mutate(age = ifelse(age < 90, age, 90)) %>%   
    group_by(gss_out, gss_in, year, sex, age) %>%
    summarise(value = sum(value)) %>%
    ungroup()
}

# Here 'regional' means only England and Wales totals, without NI and S
# so we don't need to worry about double counting when we aggregate later in the script
domestic_region <- filter(domestic, substr(gss_in,1,3) %in% c("E12","W92")) %>%
  filter(substr(gss_out,1,3) %in% c("E12","W92"))
domestic_la <- filter(domestic, !substr(gss_in,1,3) %in% c("E12","W92")) %>%
  filter(!substr(gss_out,1,3) %in% c("E12","W92"))

message("Saving domestic origin-destination flows. This may take a while")
saveRDS(domestic_la, file = paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds"))
saveRDS(domestic_region, file = paste0("input_data/domestic_migration/2018/regional_domestic_migration_flows_ons.rds"))

rm(domestic_la, domestic_region)

# Calculate gross flows
message("Calculating historic gross flows")
backseries_years <- sort(unique(domestic$year))

# data.table again - it's repeated below in a tidyverse equivalent

data.table::setDT(domestic)

dom_out_dt <- domestic[, .(dom_out = sum(value)), .(year, gss_out, sex, age)]
data.table::setnames(dom_out_dt, "gss_out", "gss_code")

dom_in_dt <- domestic[, .(dom_in = sum(value)), .(year, gss_in, sex, age)]
data.table::setnames(dom_in_dt, "gss_in", "gss_code")

rm(domestic) # mercy for machines with less RAM
assert_that(all(complete.cases(dom_out_dt)))
assert_that(all(complete.cases(dom_in_dt)))

dom_net <- merge(dom_out_dt, dom_in_dt, by=c("year","gss_code","sex","age"), all=TRUE, sort=TRUE)
dom_net[ is.na(dom_out), dom_out := 0]
dom_net[ is.na(dom_in), dom_in := 0]

dom_net <- complete(dom_net, year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
  data.table::setDT()
dom_net[, dom_net := dom_in - dom_out] %>%
  data.table::setDF()

dom_in_dt  <- complete(dom_in_dt,  year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_in=0))
dom_out_dt <- complete(dom_out_dt, year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0))


# Tidyverse equivalent
if(FALSE) {
  dom_out <- domestic %>%
    group_by(year, gss_out, sex, age) %>%
    summarise(dom_out = sum(value)) %>%
    rename(gss_code = gss_out) %>%
    complete(year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0))
  assert_that(all(complete.cases(dom_out)))
  
  dom_in <- domestic %>%
    group_by(year, gss_in, sex, age) %>%
    summarise(dom_in = sum(value)) %>%
    rename(gss_code = gss_in) %>%
    complete(year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_in=0))
  assert_that(all(complete.cases(dom_in)))
  
  dom_net <- full_join(dom_out, dom_in, by = c("year", "gss_code", "sex", "age")) %>%
    replace_na(list(dom_out = 0, dom_in = 0)) %>%
    complete(year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
    mutate(dom_net = dom_in - dom_out)
}

saveRDS(dom_out_dt, file = paste0("input_data/domestic_migration/2018/domestic_migration_out.rds"))
saveRDS(dom_in_dt, file = paste0("input_data/domestic_migration/2018/domestic_migration_in.rds"))
saveRDS(dom_net, file = paste0("input_data/domestic_migration/2018/domestic_migration_net.rds"))

