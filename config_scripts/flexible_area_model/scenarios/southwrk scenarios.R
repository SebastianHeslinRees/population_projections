#devtools::document('model_code/flexibleareamodel')
devtools::install('model_code/flexibleareamodel', upgrade = FALSE)
library(flexibleareamodel)
library(job)

job({
  run_bpo_projection(bpo_name = "Southwark_SHLAA_50pc_WD13",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_SHLAA_50pc_WD13")},
  
  title = "50%"
)

job({
  run_bpo_projection(bpo_name = "Southwark_SHLAA_150pc_WD13",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_SHLAA_150pc_WD13")},
  
  title = "150%"
)

job({
  run_bpo_projection(bpo_name = "Southwark_SHLAA_200pc_WD13",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_SHLAA_200pc_WD13")},
  
  title = "200%"
)

job({
  run_bpo_projection(bpo_name = "Southwark_zero_dev_WD13",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_zero_dev_WD13")},
  
  title = "0 dev"
)


job({
  run_bpo_projection(bpo_name = "Southwark_SHLAA_low_fert_WD13",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_SHLAA_low_fert_WD13",
                     fert_scenario = "low")},
  
  title = "low fert"
)

job({
  run_bpo_projection(bpo_name = "Southwark_SHLAA_high_fert_WD13_TEST",
                     wards = "WD13",
                     variant = "lower",
                     trajectory_range = 2012:2041,
                     projection_range = 2021:2030,
                     bpo_dir = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/",
                     csv_name = "Southwark_SHLAA_high_fert_WD13",
                     fert_scenario = "high")},
  
  title = "high fert"
)
