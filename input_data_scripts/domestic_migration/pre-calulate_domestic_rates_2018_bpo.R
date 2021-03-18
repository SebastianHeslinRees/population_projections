library(dplyr)
library(popmodules)

message("domestic migration rates (2018 bpo)")

#create domestic inputs rates
popn_mye_path <- "input_data/mye/2018/population_gla.rds"
births_mye_path <-  "input_data/mye/2018/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2018/domestic_migration_flows_ons.rds"


#All rates
rates_backseries <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                    popn_mye_path = popn_mye_path,
                                                    births_mye_path = births_mye_path,
                                                    years_backseries = 2009:2018,
                                                    col_partial_match = c("gss_out","gss_in"),
                                                    col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                    col_component = "value",
                                                    rate_cap = NULL)
#-------------------------------------

#Averages for 2018 based BPO projections

#high migration
dom_rates_3yr_avg_2018 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2018,
                                            n_years_to_avg = 3,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

#low migration
dom_rates_4yr_avg_2012 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2012,
                                            n_years_to_avg = 4,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


loc <- "input_data/domestic_migration/processed_rates/"
dir.create(loc, showWarnings = FALSE)

saveRDS(dom_rates_3yr_avg_2018, paste0(loc, "dom_rates_3yr_avg_2018.rds"))
saveRDS(dom_rates_4yr_avg_2012, paste0(loc, "dom_rates_4yr_avg_2012.rds"))
