library(dplyr)

domestic_file <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/full_machine_readable.rds"

domestic <- readRDS(domestic_file) %>%
  mutate(sex = as.character(sex)) %>%
  rename(gss_in = in_la,
         gss_out = out_la)


dir.create("input_data/domestic_migration/", showWarnings = T)
dir.create("input_data/domestic_migration/2018", showWarnings = T)

datestamp <- Sys.Date()

saveRDS(domestic, file = paste0("input_data/domestic_migration/2018/domestic_migration_ons_", datestamp, ".rds"))
