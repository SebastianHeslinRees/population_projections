births_2022 <- readRDS("Q:/Teams/D&PA/Demography/Projections/flexible_area_model_data/births_mid_22.rds")
dir.create("input_data/scenario_data", showWarnings = FALSE)
saveRDS(births_2022, "input_data/scenario_data/births_mid_22.rds")
rm(list=ls())
