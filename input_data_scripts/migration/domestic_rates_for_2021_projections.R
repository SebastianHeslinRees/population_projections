library(dplyr)
library(popmodules)

message("scenario domestic migration rates (2021 projections)")

popn_mye_path <- "input_data/mye/2021/population_gla.rds"
births_mye_path <-  "input_data/mye/2021/births_gla.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2021/domestic_migration_flows_ons_(2021_geog).rds"
dir.create("input_data/scenario_data", showWarnings = FALSE)
 
popn <- readRDS(popn_mye_path) %>%
  filter(!substr(gss_code,1,3) %in% c("E12","E92","W92","E13"))

births <- readRDS(births_mye_path) %>%
  filter(!substr(gss_code,1,3) %in% c("E12","E92","W92","E13"))

flows <- readRDS(dom_origin_destination_path) %>%
  filter(!substr(gss_in, 1, 3) %in% c("E12","E92","W92","E13")) %>%
  filter(!substr(gss_out, 1, 3) %in% c("E12","E92","W92","E13"))

#-------------------------------------------------------------------------------

#domestic
rates_backseries <- get_rate_backseries(component_mye_path = flows,
                                        popn_mye_path = popn,
                                        births_mye_path = births,
                                        years_backseries = 2006:2021,
                                        col_partial_match = c("gss_out","gss_in"),
                                        col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                        col_component = "value",
                                        rate_cap = NULL)

dom_rates_avg_2017_2021 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2021,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_avg_2012_2021 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2021,
                                n_years_to_avg = 10,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_avg_2007_2021 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2021,
                                n_years_to_avg = 15,
                                col_rate = "rate",
                                rate_cap = 0.8)

#-------------------------------------------------------------------------------

saveRDS(dom_rates_avg_2017_2021, "input_data/scenario_data/2021_dom_5yr_avg.rds")
saveRDS(dom_rates_avg_2012_2021, "input_data/scenario_data/2021_dom_10yr_avg.rds")
saveRDS(dom_rates_avg_2007_2021, "input_data/scenario_data/2021_dom_15yr_avg.rds")



