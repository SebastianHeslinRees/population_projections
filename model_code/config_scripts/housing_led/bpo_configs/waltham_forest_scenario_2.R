#Data supplied on 30/06/2020
library(popmodules)
library(trendmodel)
library(housingledmodel)

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "waltham_forest_scenario_2",
                                 shlaa_first_yr = 2036,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}


bpo_name <- run_bpo_projection(bpo_name = "waltham_forest_scenario_2",
                               shlaa_first_yr = 2036,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
