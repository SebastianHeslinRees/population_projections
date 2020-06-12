source('model_code/model_scripts/housing_led/run_bpo_projection.R')

bpo_name <- run_bpo_projection(bpo_name = "greenwich_scenario_1",
                               shlaa_first_yr = 2035,
                               last_proj_yr = 2050,
                               dev_first_yr = 2020,
                               migration_scenario = "high")

