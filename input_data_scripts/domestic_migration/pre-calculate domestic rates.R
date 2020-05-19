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

#Averages
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

dom_rates_10yr_avg_2019_50pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.5)

dom_rates_10yr_avg_2019_30pc <- dom_rates_10yr_avg_2019 %>%
  mutate(rate = rate *0.3)



#save files
loc <- "input_data/domestic_migration/processed_rates/"
dir.create(loc)

saveRDS(dom_rates_10yr_avg_2018, paste0(loc, "dom_rates_10yr_avg_2018.rds"))
saveRDS(dom_rates_5yr_avg_2018, paste0(loc, "dom_rates_5yr_avg_2018.rds"))
saveRDS(dom_rates_15yr_avg_2018, paste0(loc, "dom_rates_15yr_avg_2018.rds"))


saveRDS(dom_rates_10yr_avg_2019, paste0(loc, "dom_rates_10yr_avg_2019.rds"))
saveRDS(dom_rates_5yr_avg_2019, paste0(loc, "dom_rates_5yr_avg_2019.rds"))
saveRDS(dom_rates_15yr_avg_2019, paste0(loc, "dom_rates_15yr_avg_2019.rds"))

saveRDS(dom_rates_10yr_avg_2019_50pc, paste0(loc, "dom_rates_10yr_avg_2019_50pc.rds"))
saveRDS(dom_rates_10yr_avg_2019_30pc, paste0(loc, "dom_rates_10yr_avg_2019_30pc.rds"))

