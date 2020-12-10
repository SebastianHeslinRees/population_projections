library(dplyr)
library(popmodules)

output_loc <- "input_data/domestic_migration/processed_rates/"
dir.create(output_loc)



#Averages for 2019 based projections
#Using adjusted GLA mye population denominators

#create domestic inputs rates
gla_popn_mye_path <- "input_data/mye/2019/population_gla.rds"
births_mye_path <-  "input_data/mye/2019/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2019/domestic_migration_flows_ons_(2020_geog).rds"


#All rates
rates_backseries <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                    popn_mye_path = gla_popn_mye_path,
                                                    births_mye_path = births_mye_path,
                                                    years_backseries = 2002:2019,
                                                    col_partial_match = c("gss_out","gss_in"),
                                                    col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                    col_component = "value",
                                                    rate_cap = NULL)

dom_rates_10yr_avg_2019 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2019,
                                            n_years_to_avg = 10,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

dom_rates_5yr_avg_2019 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2019,
                                            n_years_to_avg = 5,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


dom_rates_15yr_avg_2019 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2019,
                                            n_years_to_avg = 15,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

#-------------------------------------

#save files

output_list <- c("dom_rates_10yr_avg_2019",
                 "dom_rates_5yr_avg_2019",
                 "dom_rates_15yr_avg_2019")

for(i in 1:length(output_list)){
  saveRDS(get(output_list[i]),
          paste0(output_loc,output_list[i],'_gla_mye.rds'))
}

rm(list=ls())
