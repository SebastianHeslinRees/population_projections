source('model_code/model_scripts/housing_led/run_bpo_projection.R')

#for(scenario in c("high","medium","low")){
  
  scenario <- "medium"
  bpo_name <- run_bpo_projection(bpo_name = "camden",
                                 shlaa_first_yr = 2033,
                                 final_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 dev_first_yr = 2012,
                                 ldd_final_yr = 2011,
                                 fertility_scenario = "trend")
#}
