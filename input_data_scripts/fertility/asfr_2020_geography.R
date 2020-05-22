devtools::load_all("model_code/popmodules")
library(dplyr)

fert_curves <- readRDS("input_data/fertility/ons_asfr_curves_2018.rds") %>%
  recode_gss_codes(col_aggregation=c("gss_code","sex","age","year"),
                   fun=list(mean),
                   recode_to_year = 2020)

saveRDS(fert_curves, "input_data/fertility/ons_asfr_curves_2018_(2020_geog).rds")