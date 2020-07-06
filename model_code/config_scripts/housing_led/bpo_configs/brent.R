library(popmodules)
library(trendmodel)
library(housingledmodel)

for(scenario in c("high","medium","low")){

  bpo_name <- run_bpo_projection(bpo_name = "brent",
                                 shlaa_first_yr = 2042,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}
