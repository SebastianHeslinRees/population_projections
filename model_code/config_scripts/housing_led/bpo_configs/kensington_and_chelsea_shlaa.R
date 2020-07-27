library(popmodules)
library(trendmodel)
library(housingledmodel)

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "kensington_and_chelsea_shlaa",
                                 shlaa_first_yr = 2019,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}


bpo_name <- run_bpo_projection(bpo_name = "kensington_and_chelsea_shlaa",
                               shlaa_first_yr = 2019,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")
