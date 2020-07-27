library(popmodules)
library(dplyr)

fert_curves <- readRDS("input_data/fertility/ons_asfr_curves_2018.rds") %>%
  recode_gss_codes(data_cols = "rate",
                   fun=list(mean),
                   recode_to_year = 2020)

saveRDS(fert_curves, "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds")