devtools::load_all('model_code/housingledmodel')

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "redbridge",
                                 shlaa_first_yr = 2031,
                                 last_proj_yr = 2050,
                                 dev_first_yr = 2020,
                                 migration_scenario = scenario)
}
