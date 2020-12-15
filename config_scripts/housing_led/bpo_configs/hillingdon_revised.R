popmodules::load_gla_models()

#trajectory received 11/11/20

for(scenario in c("high","medium","low")){
  bpo_name <- run_bpo_projection(bpo_name = "hillingdon_revised",
                                 dev_first_yr = 2012,
                                 shlaa_first_yr = 2026,
                                 last_proj_yr = 2050,
                                 migration_scenario = scenario)
}

bpo_name <- run_bpo_projection(bpo_name = "hillingdon_revised",
                               dev_first_yr = 2012,
                               shlaa_first_yr = 2026,
                               last_proj_yr = 2050,
                               migration_scenario = "medium",
                               fertility_scenario = "trend")