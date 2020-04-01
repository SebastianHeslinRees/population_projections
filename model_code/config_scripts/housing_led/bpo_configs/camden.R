source('model_code/model_scripts/housing_led/run_bpo_projection.R')

for(scenario in c("high","medium","low")){
  
  bpo_name <- run_bpo_projection(bpo_name = "camden_trended_births",
                                 shlaa_first_yr = 2032,
                                 final_proj_yr = 2050,
                                 migration_scenario = scenario,
                                 csv_name = "camden",
                                 housing_led_params = list(fertility_rates_path = "input_data/fertility/fertility_rates_inc_2019_in_london_5yr_trend.rds",
                                                           additional_births_path = "input_data/fertility/births_2019.rds"))
}
