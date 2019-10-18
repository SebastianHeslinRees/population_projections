library(dplyr)
library(tidyr)
library(assertthat)

message("domestic migration")

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/2002-2018 but codes not changed.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la)

domestic <- domestic %>%
  popmodules::recode_gss_to_2011(col_geog="gss_in", col_aggregation=c("gss_in","gss_out","age","sex","year"), fun=list(sum)) %>%
  popmodules::recode_gss_to_2011(col_geog="gss_out", col_aggregation=c("gss_in","gss_out","age","sex","year"), fun=list(sum))


dir.create("input_data/domestic_migration/", showWarnings = T)
dir.create("input_data/domestic_migration/2018", showWarnings = T)

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

message("Saving domestic origin-destination flows. This may take a while")
saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))



# Calculate net flows
message("Calculating historic net flows. This may also take a while")
backseries_years <- sort(unique(domestic$year))

# data.table again - it's repeated below in a tidyverse equivalent

data.table::setDT(domestic)

dom_out_dt <- domestic[, .(dom_out = sum(value)), .(year, gss_out, sex, age)]
data.table::setnames(dom_out_dt, "gss_out", "gss_code")

dom_in_dt <- domestic[, .(dom_in = sum(value)), .(year, gss_in, sex, age)]
data.table::setnames(dom_in_dt, "gss_in", "gss_code")

assert_that(all(complete.cases(dom_out_dt)))
assert_that(all(complete.cases(dom_in_dt)))

dom_net <- merge(dom_out_dt, dom_in_dt, by=c("year","gss_code","sex","age"), all=TRUE, sort=TRUE)
dom_net[ is.na(dom_out), dom_out := 0]
dom_net[ is.na(dom_in), dom_in := 0]
dom_net <- complete(dom_net, year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
  data.table::setDT()
dom_net[, dom_net := dom_in - dom_out] %>%
  data.table::setDF()

data.table::setDF(domestic)

# Tidyverse equivalent
if(FALSE) {
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
    complete(year=backseries_years, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
    mutate(dom_net = dom_in - dom_out)
}

saveRDS(dom_net, file = paste0("input_data/domestic_migration/2018/domestic_migration_net_", datestamp, ".rds"))