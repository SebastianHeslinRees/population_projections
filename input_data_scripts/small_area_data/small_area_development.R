load("Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2016/October 2017 (v2)/Output/2016 SHLAA.Rdata")
# file.copy("Q:/Teams/D&PA/Data/housing_development/shlaa/shlaa_2016/October 2017 (v2)/Output/borough_shlaa_trajectory.rds",
#          "input_data/housing_led_model/borough_shlaa_trajectory.rds")

borough_dev <- select(borough_dev, year, gss_code, units = new_homes)
saveRDS(borough_dev, "input_data/housing_led_model/borough_shlaa_trajectory.rds")

ward_dev <- select(ward_dev, year, gss_code_ward, units = new_homes)
saveRDS(ward_dev, "input_data/small_area_model/ward_shlaa_trajectory.rds")

msoa_dev <- select(msoa_dev, year, gss_code_msoa, units = new_homes)
saveRDS(msoa_dev, "input_data/small_area_model/msoa_shlaa_trajectory.rds")
