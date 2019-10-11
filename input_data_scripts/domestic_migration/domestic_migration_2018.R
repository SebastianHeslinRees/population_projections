library(dplyr)
library(popmodules)

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

saveRDS(domestic_b, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))

rm(domestic_file, domestic, domestic_a, domestic_b, datestamp)
