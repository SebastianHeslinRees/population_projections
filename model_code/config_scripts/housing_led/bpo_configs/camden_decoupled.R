devtools::load_all("model_code/popmodules")
devtools::load_all("model_code/trendmodel")
devtools::load_all("model_code/housingledmodel")

for(scenario in c("high","medium","low")){

  bpo_name <- run_bpo_projection(bpo_name = "camden_decoupled",
                                 shlaa_first_yr = 2032,
                                 last_proj_yr = 2050,
                                 first_proj_yr = 2012,
                                 migration_scenario = scenario,
                                 csv_name = "camden")
}
