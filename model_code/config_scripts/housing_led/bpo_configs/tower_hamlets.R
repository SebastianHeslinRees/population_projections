devtools::load_all('model_code/housingledmodel')

for(scenario in c("high","medium","low")){

  bpo_name <- run_bpo_projection(bpo_name = "tower_hamlets",
                                 shlaa_first_yr = 2035,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}

bpo_name <- run_bpo_projection(bpo_name = "tower_hamlets",
                               shlaa_first_yr = 2035,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
