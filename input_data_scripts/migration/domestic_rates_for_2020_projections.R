library(dplyr)
library(popmodules)

message("scenario domestic migration rates (2020 projections)")

popn_mye_path <- "input_data/mye/2020/population_gla.rds"
births_mye_path <-  "input_data/mye/2020/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2020/domestic_migration_flows_ons_(2021_geog).rds"
dir.create("input_data/scenario_data", showWarnings = FALSE)
 
#-------------------------------------------------------------------------------

#domestic
rates_backseries <- get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                        popn_mye_path = popn_mye_path,
                                        births_mye_path = births_mye_path,
                                        years_backseries = 2002:2020,
                                        col_partial_match = c("gss_out","gss_in"),
                                        col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                        col_component = "value",
                                        rate_cap = NULL)

dom_rates_avg_2016_2020 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2020,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_avg_2011_2020 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2020,
                                n_years_to_avg = 10,
                                col_rate = "rate",
                                rate_cap = 0.8)

dom_rates_avg_2008_2012 <- rates_backseries %>%
  calculate_mean_domestic_rates(last_data_year = 2012,
                                n_years_to_avg = 5,
                                col_rate = "rate",
                                rate_cap = 0.8)

into_london <- dom_rates_avg_2016_2020 %>% 
  filter(substr(gss_in,1,3)=="E09") %>% 
  filter(substr(gss_out,1,3)!="E09")

outfrom_london <- dom_rates_avg_2016_2020 %>% 
  filter(substr(gss_out,1,3)=="E09") %>% 
  filter(substr(gss_in,1,3)!="E09")

everywhere_else <- dom_rates_avg_2016_2020 %>% 
  filter(substr(gss_in,1,3) != "E09") %>% 
  filter(substr(gss_out,1,3) != "E09") %>% 
  rbind(filter(dom_rates_avg_2016_2020,
               substr(gss_out,1,3) == "E09" & substr(gss_in,1,3)=="E09"))

#-------------------------------------------------------------------------------

dom_in_20 <- into_london %>% mutate(rate = rate * 0.2)
dom_in_50 <- into_london %>% mutate(rate = rate * 0.5)

dom_out_60 <- outfrom_london %>% mutate(rate = rate * 0.6)
dom_out_50 <- outfrom_london %>% mutate(rate = rate * 0.5)

everywhere_else <- everywhere_else %>% mutate(rate = rate * 0.7)

#-------------------------------------------------------------------------------

scenario_1_yr_2021 <- rbind(dom_in_20, dom_out_60, everywhere_else)
scenario_1_yr_2022 <- rbind(dom_in_50, dom_out_50, everywhere_else)

#-------------------------------------------------------------------------------

saveRDS(scenario_1_yr_2021, "input_data/scenario_data/2020_dom_scenario_1_yr_2021.rds")
saveRDS(scenario_1_yr_2022, "input_data/scenario_data/2020_dom_scenario_1_yr_2022.rds")
saveRDS(dom_rates_avg_2016_2020, "input_data/scenario_data/2020_dom_5yr_avg.rds")
saveRDS(dom_rates_avg_2011_2020, "input_data/scenario_data/2020_dom_10yr_avg.rds")
saveRDS(dom_rates_avg_2008_2012, "input_data/scenario_data/2012_dom_5yr_avg.rds")



