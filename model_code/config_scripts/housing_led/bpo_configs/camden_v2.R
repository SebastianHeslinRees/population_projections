#Revised camden trajectory. Sent on 22/06/20
source('model_code/model_scripts/housing_led/run_bpo_projection.R')

# for(scenario in c("high","medium","low")){
#   
#   bpo_name <- run_bpo_projection(bpo_name = "camden_v2",
#                                  shlaa_first_yr = 2033,
#                                  last_proj_yr = 2050,
#                                  migration_scenario = scenario,
#                                  dev_first_yr = 2012,
#                                  ldd_final_yr = 2011,
#                                  fertility_scenario = "average")
# }

bpo_name <- run_bpo_projection(bpo_name = "camden_v2",
                               shlaa_first_yr = 2033,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               dev_first_yr = 2012,
                               ldd_final_yr = 2011,
                               fertility_scenario = "trend")
