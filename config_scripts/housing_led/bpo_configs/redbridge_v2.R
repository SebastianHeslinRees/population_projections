#Trajectory supplied 30/11/20

library(popmodules)
library(trendmodel)
library(housingledmodel)

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "redbridge_version_2",
                                 shlaa_first_yr = 2032,
                                 last_proj_yr = 2050,
                                 dev_first_yr = 2016,
                                 migration_scenario = scenario)
}

bpo_name <- run_bpo_projection(bpo_name = "redbridge_version_2",
                               shlaa_first_yr = 2032,
                               last_proj_yr = 2050,
                               dev_first_yr = 2016,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
