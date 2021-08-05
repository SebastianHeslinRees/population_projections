library(popmodules)
library(dplyr)

message("ASFRs 2021 geography")

fert_curves <- readRDS("input_data/fertility/ons_asfr_curves_2018.rds") %>%
  recode_gss_codes(data_cols = "rate",
                   fun=list(mean),
                   recode_to_year = 2021)

saveRDS(fert_curves, "input_data/fertility/ons_asfr_curves_2018_(2021_geog).rds")
