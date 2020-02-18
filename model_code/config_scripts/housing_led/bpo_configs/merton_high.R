source('model_code/model_scripts/housing_led/run_bpo_projection.R')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "merton_high",
                                 shlaa_first_year = 2042,
                                 final_proj_yr = 2050,
                                 migration_scenario = scenario)
}
