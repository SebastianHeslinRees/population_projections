library(dplyr)
library(tidyr)
library(assertthat)

message("domestic migration")

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/2002-2018 but codes not changed.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la)

domestic_a <- domestic %>%
  popmodules::recode_gss_to_2011(col_geog="gss_in", col_aggregation=c("gss_in","gss_out","age","sex","year"), fun=list(sum))

domestic_b <- domestic_a %>%
  popmodules::recode_gss_to_2011(col_geog="gss_out", col_aggregation=c("gss_in","gss_out","age","sex","year"), fun=list(sum))


dir.create("input_data/domestic_migration/", showWarnings = T)
dir.create("input_data/domestic_migration/2018", showWarnings = T)

datestamp <- Sys.Date()

# Save absolute values
message("Saving domestic origin-destination flows. This may take a while")
saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))



# Calculate net flows
message("Calculating historic net flows. This may also take a while")


# Everything below here is a mess - best to ignore it until the next big commit :O :O


# This code uses data.table - it's repeated below in the tidyverse equivalent

data.table::setDT(domestic)

domestic[, year := as.integer(year)]

dom_out <- data.table::copy(domestic)
dom_out <- dom_out[, dom_out := sum(value), by = c("year", "gss_out", "sex", "age")]
data.table::setnames(dom_out, "gss_out", "gss_code")

dom_in <- data.table::copy(domestic)
dom_in <- domestic[, dom_in := sum(value), by = c("year", "gss_in", "sex", "age")]
data.table::setnames(dom_in, "gss_in", "gss_code")

assert_that(all(complete.cases(dom_out)))
assert_that(all(complete.cases(dom_in)))

dom_net_dt <- data.table::merge(dom_out, dom_in, by=c("year","gss_code","sex","age"), all=TRUE, sort=TRUE)
dom_net_dt[ is.na(dom_out), dom_out = 0]
dom_net_dt[ is.na(dom_in), dom_in = 0]
dom_net_dt <- complete(year=2002:2018, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0))
dom_net_dt[, dom_net = dom_in - dom_out]

data.table::setDF(domestic)

# Tidyverse equivalent
if(FALSE) {
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
  
  dom_net_df <- full_join(dom_out, dom_in, by = c("year", "gss_code", "sex", "age")) %>%
    replace_na(list(dom_out = 0, dom_in = 0)) %>%
    complete(year=2002:2018, gss_code, sex, age=0:90, fill=list(dom_out=0, dom_in=0)) %>%
    mutate(dom_net = dom_in - dom_out)
}

saveRDS(dom_net, file = paste0("input_data/domestic_migration/2018/domestic_migration_net_", datestamp, ".rds"))

  


# Convert to rates
message("Calculating 5-year origin-destination migration rates. This may also take a whiiile")

mye <- readRDS("input_data/mye/2018/population_ons_2019-08-27.rds")

# data.table code: tidyverse equivalent below
data.table::setDT(mye)
mye[year >=2014,]

domestic[year >= 2014, ]

if(FALSE) {
  
  system.time({

  mye = filter(mye, year >= 2014)

domestic <- domestic %>%
  filter(year >= 2014) %>%
  left_join(mye, by = c("year", "gss_out"="gss_code", "age", "sex")) %>%
  mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
  select(year, gss_out, gss_in, sex, age, rate) %>%
  group_by(gss_out, gss_in, sex, age) %>%
  summarise(rate = mean(rate))

})
  
  
  system.time({
    mye_1 <- data.table(mye, key=c("year", "gss_code", "age", "sex"))
    dom_1 <- domestic %>%
      filter(year >= 2014) %>%
      mutate(year = as.integer(year)) %>%
      setDT(key=c("year", "gss_out", "age", "sex"))
    
  historic.domestic  <- mye_1[dom_1] %>%
    setnames("gss_code","gss_out") %>%
    select(gss_out, gss_in, year, age, sex, value, popn)%>%
    mutate(flow_rate = ifelse(popn == 0, 0, value/popn))
  
  # domestic.average <- setDT(historic.domestic)[, sum(flow_rate), by=list(gss_in, gss_out, sex, age)] %>%
  #   setnames("V1", "flow_rate") %>%
  #   as.data.frame() %>%
  #   mutate(flow_rate = flow_rate/5)  
  
  })

message("Saving 5-year origin-destination migration rates. This shouldn't take quite as long")
saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_rate_5year_", datestamp, ".rds"))
=======
saveRDS(domestic_b, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))

rm(domestic_file, domestic, domestic_a, domestic_b, datestamp)
