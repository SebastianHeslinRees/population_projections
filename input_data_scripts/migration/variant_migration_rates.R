popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-13.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")

origin_destination_rate <- popmodules::get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                           popn_mye_path = popn_mye_path,
                                                           births_mye_path = births_mye_path,
                                                           years_backseries = 2002:2018,
                                                           col_partial_match = c("gss_out","gss_in"),
                                                           col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                           col_component = "value",
                                                           rate_cap = NULL)


low_migration <- popmodules::calculate_mean_domestic_rates(origin_destination_rate,
                                                           last_data_year = 2012,
                                                           n_years_to_avg = 4,
                                                           col_rate = "rate",
                                                           rate_cap = 0.8)

high_migration <- popmodules::calculate_mean_domestic_rates(origin_destination_rate,
                                                            last_data_year = 2018,
                                                            n_years_to_avg = 3,
                                                            col_rate = "rate",
                                                            rate_cap = 0.8)

saveRDS(low_migration, "input_data/migration/low_domestic_migration_rates_(2009_2012).rds")
saveRDS(high_migration, "input_data/migration/high_domestic_migration_rates_(2016_2018).rds")
