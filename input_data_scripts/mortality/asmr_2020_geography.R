devtools::load_all("model_code/popmodules")
library(dplyr)

mort_curves <- readRDS("input_data/mortality/ons_asmr_curves_2018.rds") %>%
  recode_gss_codes(col_aggregation=c("gss_code","sex","age","year"),
                   fun=list(mean),
                   recode_to_year = 2020)

saveRDS(mort_curves, "input_data/mortality/ons_asmr_curves_2018_(2020_geog).rds")