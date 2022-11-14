devtools::load_all('model_code/popmodules/')
devtools::load_all('model_code/flexibleareamodel')
data_dir <- "input_data/flexible_area_model/"
ward_2022_name_lookup <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds")
th_wards <- filter(ward_2022_name_lookup, gss_code == "E09000030")$gss_code_ward
base_trajectory <- readRDS("Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/rds/bpo_ward_trajectory_tower_hamlets_WD22.rds")

#-----------

# BASE SCENARIO

dev_trajectory <- "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/rds/bpo_ward_trajectory_tower_hamlets_WD22.rds"
projection_name <- "TH sensitivity Base Scenario"

#Config
base_config <- list(projection_name = projection_name,
                    first_proj_yr = 2021,
                    n_proj_yr = 21,
                    output_dir = paste0("outputs/flexible_area_model/", projection_name),
                    
                    #backseries
                    population_path = paste0(data_dir, "backseries/ward_population_WD22CD.rds"),
                    deaths_path = paste0(data_dir, "backseries/ward_deaths_WD22CD.rds"),
                    births_path = paste0(data_dir, "backseries/ward_births_WD22CD.rds"),
                    out_migration_path = paste0(data_dir, "backseries/ward_outflow_WD22CD.rds"),
                    in_migration_path = paste0(data_dir, "backseries/ward_inflow_WD22CD.rds"),
                    
                    #rates
                    mortality_rates = paste0(data_dir, "processed/mortality_rates_WD22CD.rds"),
                    fertility_rates = paste0(data_dir, "processed/fertility_rates_WD22CD.rds"),
                    in_migration = list(
                      '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2021.rds"),
                                    transition = F),
                      '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2022.rds"),
                                    transition = T),
                      '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                                    transition = F)),
                    out_migration = list(
                      '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2021.rds"),
                                    transition = F),
                      '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2022.rds"),
                                    transition = T),
                      '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                                    transition = F)),
                    
                    #constraints
                    constraint_list = list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
                                           make_constraint_lookup_path = "input_data/flexible_area_model/lookups/NUTS2_hma.rds",
                                           apply_constraint_lookup_path = "input_data/flexible_area_model/lookups/NUTS2_WD22.rds",
                                           mapping = c("constraint_area","year","sex","age"),
                                           components = list(births = F,
                                                             deaths = F,
                                                             in_migration = F,
                                                             out_migration = F,
                                                             population = F)),
                    
                    #HOUSING-LED STUFF
                    dev_trajectory_path = "Q:/Teams/D&PA/Demography/Projections/bpo_2020_based/rds/bpo_ward_trajectory_tower_hamlets_WD22.rds", 
                    ldd_backseries_path = paste0(data_dir, "development_data/ldd_backseries_dwellings_ward_WD22CD.rds"),
                    communal_est_path = paste0(data_dir, "processed/communal_establishment_popn_WD22CD.rds"),
                    dwellings_to_households_path = paste0(data_dir, "processed/ward_dwelling_2_hh_ratio_WD22CD.rds"),
                    
                    #settings
                    hhr_path = paste0(data_dir, "processed/ward_hh_rep_rate_WD22CD.rds"),
                    ahs_mix = 0.8,
                    hhr_static_or_projected = "static",
                    lookup_path = "input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds",
                    excess_deaths_path = NULL,
                    geog_code_col = "gss_code_ward",
                    geog_name_col = "ward_name",
                    parallel = FALSE,
                    borough_outputs = TRUE
                    
)

model_output <- flexmodel_hl_projection(base_config)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#No COVID

no_covid_config <- base_config
projection_name <- "TH_sensitivity_No_COVID"
no_covid_config$projection_name <- projection_name
no_covid_config$output_dir <- paste0("outputs/flexible_area_model/", projection_name)

no_covid_config$in_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_5yr_avg.rds"),
                transition = F))

no_covid_config$out_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_5yr_avg.rds"),
                transition = F))


model_output <- flexmodel_hl_projection(no_covid_config)

#-------------------------------------------------------------------------------

#AHS Mix = 1
AHS_1_config <- base_config
projection_name <- "TH_sensitivity_AHS_1"
AHS_1_config$projection_name <- projection_name
AHS_1_config$output_dir <- paste0("outputs/flexible_area_model/", projection_name)
AHS_1_config$ahs_mix <- 1

model_output <- flexmodel_hl_projection(AHS_1_config)

#-------------------------------------------------------------------------------

#AHS Mix = 0
AHS_0_config <- base_config
projection_name <- "TH_sensitivity_AHS_0"
AHS_0_config$projection_name <- projection_name
AHS_0_config$output_dir <- paste0("outputs/flexible_area_model/", projection_name)
AHS_0_config$ahs_mix <- 0

model_output <- flexmodel_hl_projection(AHS_0_config)


#-------------------------------------------------------------------------------

# 9-year migration

long_mig_config <-base_config
projection_name <- "9_yr_migration"
long_mig_config$projection_name <- projection_name
long_mig_config$output_dir <- paste0("outputs/flexible_area_model/", projection_name)

long_mig_config$in_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/in_migration_flows_WD22CD_9yr_avg.rds"),
                transition = F))

long_mig_config$out_migration <- list(
  '2021' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2021.rds"),
                transition = F),
  '2022' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_Covid_2022.rds"),
                transition = T),
  '2025' = list(path = paste0(data_dir, "processed/out_migration_rates_WD22CD_9yr_avg.rds"),
                transition = F))

model_output <- flexmodel_hl_projection(long_mig_config)

#-------------------------------------------------------------------------------

#1000 unit uplift
uplift <- base_config
projection_name <- "TH_sensitivity_1000_uplift"
uplift$projection_name <- projection_name
uplift$output_dir <- paste0("outputs/flexible_area_model/", projection_name)
uplift$dev_trajectory_path <-  "input_data/flexible_area_model/TH_dev_scenarios/TH_sensitivity_1000_unit_uplift_DEV.rds"

model_output <- flexmodel_hl_projection(uplift)


#-------------------------------------------------------------------------------