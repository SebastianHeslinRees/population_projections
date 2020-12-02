#Hounslow updated trajectory - 02/12/2020
library(popmodules)
load_gla_models()

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "hounslow_updated",
                                 dev_first_yr = 2020,
                                 shlaa_first_yr = 2034,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 fertility_scenario = "average")
}

bpo_name <- run_bpo_projection(bpo_name = "hounslow_updated",
                               dev_first_yr = 2020,
                               shlaa_first_yr = 2034,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
