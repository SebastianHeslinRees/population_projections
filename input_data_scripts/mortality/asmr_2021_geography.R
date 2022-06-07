library(popmodules)
library(dplyr)

message("ASMRs 2021 geography")

mort_curves <- readRDS("input_data/mortality/ons_asmr_curves_2018.rds") %>%
  recode_gss_codes(data_cols = "rate",
                   fun="mean",
                   recode_to_year = 2021)

saveRDS(mort_curves, "input_data/mortality/ons_asmr_curves_2018_(2021_geog).rds")
