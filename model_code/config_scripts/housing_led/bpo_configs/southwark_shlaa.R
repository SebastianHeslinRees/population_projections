devtools::load_all("model_code/popmodules")
devtools::load_all("model_code/trendmodel")
devtools::load_all("model_code/housingledmodel")

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "southwark_shlaa",
                                 shlaa_first_yr = 2019,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 fertility_scenario = "average")
}

bpo_name <- run_bpo_projection(bpo_name = "southwark_shlaa",
                               shlaa_first_yr = 2019,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")