library(assertthat)
message("domestic migration 2020")

data_loc <- "input_data/domestic_migration/2020/"
q_drive_loc <- "Q:/Teams/D&PA/Data/domestic_migration/current_series_from_2002/processed/2020/"

dir.create(data_loc, showWarnings = FALSE, recursive = TRUE)

assert_that(all(
  file.copy(paste0(q_drive_loc, "origin_destination_2002_to_2020_(2021_geog).rds"),
            paste0(data_loc, "domestic_migration_flows_ons_(2021_geog).rds"),
            overwrite = TRUE),
  
  file.copy(paste0(q_drive_loc, "domestic_migration_in_(2021_geog).rds"),
            paste0(data_loc, "domestic_migration_in_(2021_geog).rds"),
            overwrite = TRUE),
  
  file.copy(paste0(q_drive_loc, "domestic_migration_out_(2021_geog).rds"),
            paste0(data_loc, "domestic_migration_out_(2021_geog).rds"),
            overwrite = TRUE),
  
  file.copy(paste0(q_drive_loc, "domestic_migration_net_(2021_geog).rds"),
            paste0(data_loc, "domestic_migration_net_(2021_geog).rds"),
            overwrite = TRUE)),
  msg="failed to copy domestic migration files from Q:")

rm(list=ls())
