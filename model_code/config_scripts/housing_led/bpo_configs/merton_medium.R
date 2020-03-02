devtools::load_all("model_code/housingledmodel")

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "merton_medium",
                                 shlaa_first_yr = 2042,
                                 final_proj_yr = 2050,
                                 migration_scenario = scenario)
}
