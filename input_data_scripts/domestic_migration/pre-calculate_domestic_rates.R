library(dplyr)
library(popmodules)

message("domestic migration rates (standard)")

output_loc <- "input_data/domestic_migration/processed_rates/"
dir.create(output_loc)

#create domestic inputs rates
#for 2018 and 2016 use the mye series with gla geography
popn_mye_path <- paste0("input_data/mye/2018/population_gla.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
dom_origin_destination_path <- "input_data/domestic_migration/2018/domestic_migration_flows_ons.rds"

#All rates
rates_backseries <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                    popn_mye_path = popn_mye_path,
                                                    births_mye_path = births_mye_path,
                                                    years_backseries = 2002:2018,
                                                    col_partial_match = c("gss_out","gss_in"),
                                                    col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                    col_component = "value",
                                                    rate_cap = NULL)
#-------------------------------------

#Averages for 2016 based projections
dom_rates_10yr_avg_2016 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2016,
                                            n_years_to_avg = 10,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


dom_rates_5yr_avg_2016 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2016,
                                            n_years_to_avg = 5,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


#Averages for 2018 based projections
dom_rates_10yr_avg_2018 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2018,
                                            n_years_to_avg = 10,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


dom_rates_5yr_avg_2018 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2018,
                                            n_years_to_avg = 5,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


dom_rates_15yr_avg_2018 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2018,
                                            n_years_to_avg = 15,
                                            col_rate = "rate",
                                            rate_cap = 0.8)


dom_rates_2yr_avg_2018 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2018,
                                            n_years_to_avg = 2,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

#-------------------------------------
rm(rates_backseries, popn_mye_path, births_mye_path, dom_origin_destination_path)


#Averages for 2019 based projections
#Use 2020 geography MYE files

#create domestic inputs rates
popn_mye_path <- "input_data/mye/2019/population_ons.rds"
births_mye_path <-  "input_data/mye/2019/births_ons.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2019/domestic_migration_flows_ons_(2020_geog).rds"


#All rates
rates_backseries <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                    popn_mye_path = popn_mye_path,
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

dom_rates_18yr_avg_2019 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2019,
                                            n_years_to_avg = 18,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

dom_rates_2yr_avg_2019 <- rates_backseries %>%
  popmodules::calculate_mean_domestic_rates(last_data_year = 2019,
                                            n_years_to_avg = 2,
                                            col_rate = "rate",
                                            rate_cap = 0.8)

#-------------------------------------

#Fractional flows for 2019 scenario projections
dom_rates_10yr_avg_2019_70pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.7)

dom_rates_10yr_avg_2019_50pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.5)

dom_rates_10yr_avg_2019_30pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.3)

dom_rates_zero <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = 0)

dom_rates_2011_levels <- calculate_mean_domestic_rates(rates_backseries,
                                                       last_data_year = 2011,
                                                       n_years_to_avg = 1,
                                                       col_rate = "rate",
                                                       rate_cap = 0.8)

#-------------------------------------

#In some scenarios we want to affect the amount of migration coming into
#or going out of London without amending the rest of the rates

reduce_rates_to <- 0.8 #80% of average
increase_rates_to <- 1.2 #120% of average

#remote working scenario
#reduced inflow, maintain outflow
reduced_in_rate <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*reduce_rates_to, rate))

#agglomeration scenario  
#2 possibles

#Inflow rates increased, outfow decreased
ldn <- dom_rates_10yr_avg_2019 %>%
  filter(substr(gss_in,1,3)=="E09" & substr(gss_out,1,3)=="E09")

in_up_out_down <- dom_rates_10yr_avg_2019 %>%
  filter(!(substr(gss_in,1,3)=="E09" & substr(gss_out,1,3)=="E09")) %>%
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*increase_rates_to, rate),
         rate = ifelse(substr(gss_out,1,3)=="E09", rate*reduce_rates_to, rate)) %>%
  rbind(ldn)

#increase inflow, maintain outflow
increased_in_rate <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*increase_rates_to, rate))

#----------------------------------------------------------

#save files

output_list <- c("dom_rates_10yr_avg_2016",
                 "dom_rates_5yr_avg_2016",
                 "dom_rates_10yr_avg_2018",
                 "dom_rates_5yr_avg_2018",
                 "dom_rates_15yr_avg_2018",
                 "dom_rates_2yr_avg_2018",
                 "dom_rates_10yr_avg_2019",
                 "dom_rates_5yr_avg_2019",
                 "dom_rates_15yr_avg_2019",
                 "dom_rates_18yr_avg_2019",
                 "dom_rates_2yr_avg_2019",
                 "dom_rates_10yr_avg_2019_70pc",
                 "dom_rates_10yr_avg_2019_50pc",
                 "dom_rates_10yr_avg_2019_30pc",
                 "dom_rates_zero",
                 "dom_rates_2011_levels")

for(i in 1:length(output_list)){
  saveRDS(get(output_list[i]),
          paste0(output_loc,output_list[i],'.rds'))
}

saveRDS(reduced_in_rate, paste0(output_loc,"dom_rates_10yr_avg_2019_reduced_ldn_in.rds"))
saveRDS(in_up_out_down, paste0(output_loc,"dom_rates_10yr_avg_2019_inc_ldn_in_reduced_ldn_out.rds"))
saveRDS(increased_in_rate, paste0(output_loc,"dom_rates_10yr_avg_2019_increased_ldn_in.rds"))

rm(list=ls())
