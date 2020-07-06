library(popmodules)
library(trendmodel)
library(housingledmodel)

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "greenwich_scenario_1",
                                 shlaa_first_yr = 2035,
                                 last_proj_yr = 2050,
                                 dev_first_yr = 2020,
                                 migration_scenario = scenario)
}
