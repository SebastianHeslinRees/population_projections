rm(list=ls())
devtools::load_all('model_code/popmodules')
source('model_code/config_scripts/housing_led/config_function.R')
source('model_code/model_scripts/bpo_template_to_rds.R')

bpo_name <- "merton_low"


#--------
borough_gss <- create_bpo_trajectory(bpo_name = bpo_name)
first_proj_yr <- 2019
final_proj_yr <- 2050
bpo_dir <- "Q:/Teams/D&PA/Demography/Projections/bpo_2018_based/"
dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_borough_trajectory_",bpo_name,".rds")
small_area_dev_trajectory_path <- paste0(bpo_dir,"rds/bpo_ward_trajectory_",bpo_name,".rds")

bpo <- run_bpo_projection(projection_name = bpo_name,
                          dev_trajectory_path,
                          small_area_dev_trajectory_path,
                          first_proj_yr,
                          final_proj_yr,
                          bpo = borough_gss)


