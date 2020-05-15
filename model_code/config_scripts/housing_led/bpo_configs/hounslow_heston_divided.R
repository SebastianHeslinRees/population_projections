source('model_code/model_scripts/housing_led/run_bpo_projection.R')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "hounslow_heston_divided",
                                 dev_first_yr = 2020,
                                 shlaa_first_yr = 2035,
                                 final_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 fertility_scenario = "average")
}

bpo_name <- run_bpo_projection(bpo_name = "hounslow_heston_divided",
                               dev_first_yr = 2020,
                               shlaa_first_yr = 2035,
                               final_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
