devtools::load_all("model_code/popmodules")
devtools::load_all("model_code/trendmodel")
devtools::load_all("model_code/housingledmodel")

for(scenario in c("high","medium","low")){
  
  bpo_name <- run_bpo_projection(bpo_name = "camden",
                                 shlaa_first_yr = 2033,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 dev_first_yr = 2012,
                                 ldd_final_yr = 2011,
                                 fertility_scenario = "average")
}

bpo_name <- run_bpo_projection(bpo_name = "camden",
                               shlaa_first_yr = 2033,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               dev_first_yr = 2012,
                               ldd_final_yr = 2011,
                               fertility_scenario = "trend")