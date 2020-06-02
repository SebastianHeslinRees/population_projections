library(dplyr)
devtools::load_all('model_code/popmodules')

#create domestic inputs rates
popn_mye_path <- "input_data/mye/2019/temp_gla_population.rds"
births_mye_path <-  "input_data/mye/2019/temp_births.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2019/temp_domestic_flows.rds"


#All rates
rates_backseries <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                    popn_mye_path = popn_mye_path,
                                                    births_mye_path = births_mye_path,
                                                    years_backseries = 2002:2019,
                                                    col_partial_match = c("gss_out","gss_in"),
                                                    col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                    col_component = "value",
                                                    rate_cap = NULL)
#-------------------------------------

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

#-------------------------------------

#Averages for 2019 based projections
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

#Fractional flows for 2019 scenario projections
dom_rates_10yr_avg_2019_70pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.7)

dom_rates_10yr_avg_2019_50pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.5)

dom_rates_10yr_avg_2019_30pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.3)

dom_rates_zero <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = 0)

dom_rates_2011 <- calculate_mean_domestic_rates(rates_backseries,
                                                last_data_year = 2011,
                                                n_years_to_avg = 1,
                                                col_rate = "rate",
                                                rate_cap = 0.8)

#-------------------------------------

#In some scenarios we want to affect the amount of migration coming into
#or going out of London without ammending the rest of the rates

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
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*increase_rate_to, rate))

#----------------------------------------------------------

#save files
loc <- "input_data/domestic_migration/processed_rates/"
dir.create(loc)

saveRDS(dom_rates_10yr_avg_2018, paste0(loc, "dom_rates_10yr_avg_2018.rds"))
saveRDS(dom_rates_5yr_avg_2018, paste0(loc, "dom_rates_5yr_avg_2018.rds"))
saveRDS(dom_rates_15yr_avg_2018, paste0(loc, "dom_rates_15yr_avg_2018.rds"))

saveRDS(dom_rates_10yr_avg_2019, paste0(loc, "dom_rates_10yr_avg_2019.rds"))
saveRDS(dom_rates_5yr_avg_2019, paste0(loc, "dom_rates_5yr_avg_2019.rds"))
saveRDS(dom_rates_15yr_avg_2019, paste0(loc, "dom_rates_15yr_avg_2019.rds"))

saveRDS(dom_rates_10yr_avg_2019_70pc, paste0(loc, "dom_rates_10yr_avg_2019_70pc.rds"))
saveRDS(dom_rates_10yr_avg_2019_50pc, paste0(loc, "dom_rates_10yr_avg_2019_50pc.rds"))
saveRDS(dom_rates_10yr_avg_2019_30pc, paste0(loc, "dom_rates_10yr_avg_2019_30pc.rds"))

saveRDS(dom_rates_zero, paste0(loc, "dom_rates_zero.rds"))

saveRDS(dom_rates_2011, paste0(loc, "dom_rates_2011_levels.rds"))

saveRDS(reduced_in_rate, "input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019_reduced_ldn_in.rds")
saveRDS(in_up_out_down, "input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019_inc_ldn_in_reduced_ldn_out.rds")
