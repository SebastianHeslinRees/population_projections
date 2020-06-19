devtools::load_all('model_code/housingledmodel')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "croydon",
                                 shlaa_first_yr = 2040,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}
