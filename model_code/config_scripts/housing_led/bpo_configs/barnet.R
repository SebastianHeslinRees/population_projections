devtools::load_all('model_code/housingledmodel')

for(scenario in c("high","medium","low")){

  bpo_name <- run_bpo_projection(bpo_name = "barnet",
                                 dev_first_yr = 2020,
                                 shlaa_first_yr = 2037,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}

bpo_name <- run_bpo_projection(bpo_name = "barnet",
                               dev_first_yr = 2020,
                               shlaa_first_yr = 2037,
                               final_proj_yr = 2025,
                               migration_scenario = "medium")
