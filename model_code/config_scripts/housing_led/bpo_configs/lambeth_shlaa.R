source('model_code/model_scripts/housing_led/run_bpo_projection.R')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "lambeth_shlaa",
                                 shlaa_first_yr = 2019,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}
