#trajectory received 11/08/20
library(housingledmodel)

for(scenario in c("high","medium","low")){

  bpo_name <- run_bpo_projection(bpo_name = "barking_and_dagenham_revised",
                                 shlaa_first_yr = 2038,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}

bpo_name <- run_bpo_projection(bpo_name = "barking_and_dagenham_revised",
                               shlaa_first_yr = 2038,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")