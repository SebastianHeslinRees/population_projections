devtools::load_all('model_code/housingledmodel')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "city_of_london_decoupled",
                                 shlaa_first_yr = 2027,
                                 last_proj_yr = 2050,
                                 first_proj_yr = 2012,
                                 migration_scenario = scenario,
                                 csv_name = "city_of_london")
}
