#rates and upc save location
save_location <- "input_data/scenario_data/"

#pre-calculate rates for use in trend model
devtools::load_all("model_code/popmodules")

first_proj_yr <- 2019
n_proj_yr <- 5
projection_name <- "2018_central_transition"

popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-13.rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla_2019-11-13.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla_2019-11-13.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out.rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in.rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")
upc_path <- NULL

mortality_years_to_avg <- 5
mortality_avg_or_trend <- "trend"
mortality_last_data_year <- 2018
mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves_2018.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"
mortality_npp_variant <- "2018_principal"

fertility_years_to_avg <- 5
fertility_avg_or_trend <- "average"
fertility_last_data_year <- 2018
fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves_2018.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"
fertility_npp_variant <- "2018_principal"

int_out_last_data_year <- 2018
int_out_years_to_avg <- 10
int_out_flow_or_rate <- "rate"
int_out_rate_cap <- 0.8

int_in_last_data_year <- 2018
int_in_years_to_avg <- 10
int_in_flow_or_rate <- "flow"

domestic_transition_yr <- 2021 #NULL if not used
dom_mig_last_data_year_initial <- 2018
dom_mig_years_to_avg_initial <- 5
dom_mig_last_data_year_longterm <- 2018
dom_mig_years_to_avg_longterm <- 10

popn_constraint_path <- "input_data/constraints/npp_2018_population_constraint.rds"
births_constraint_path <- "input_data/constraints/npp_2018_fertility_constraint.rds"
deaths_constraint_path <- "input_data/constraints/npp_2018_mortality_constraint.rds"
int_in_constraint_path <- "input_data/constraints/npp_2018_international_in_constraint.rds"
int_out_constraint_path <- "input_data/constraints/npp_2018_international_out_constraint.rds"
cross_in_constraint_path <- "input_data/constraints/npp_2018_cross_border_in_constraint.rds"
cross_out_constraint_path <- "input_data/constraints/npp_2018_cross_border_out_constraint.rds"



#-------------------------------------------------


mortality <- popmodules::scaled_mortality_curve(popn_mye_path = popn_mye_path,
                                                births_mye_path = births_mye_path,
                                                deaths_mye_path = deaths_mye_path,
                                                target_curves_filepath = mortality_curve_filepath,
                                                last_data_year = mortality_last_data_year,
                                                n_years_to_avg = mortality_years_to_avg,
                                                avg_or_trend = mortality_avg_or_trend,
                                                data_col = "deaths",
                                                output_col = "rate") %>% 
  
  popmodules::project_rates_npp(rate_col = "rate",
                                rate_trajectory_filepath = mortality_trajectory_filepath,
                                first_proj_yr = first_proj_yr,
                                n_proj_yr = n_proj_yr,
                                npp_var = mortality_npp_variant)


#------------------------------------------

fertility <- popmodules::scaled_fertility_curve(popn_mye_path = popn_mye_path,
                                                births_mye_path = births_mye_path,
                                                target_curves_filepath = fertility_curve_filepath,
                                                last_data_year = fertility_last_data_year,
                                                n_years_to_avg = fertility_years_to_avg,
                                                avg_or_trend = fertility_avg_or_trend,
                                                data_col = "births",
                                                output_col = "rate") %>% 
  
  popmodules::project_rates_npp(rate_col = "rate",
                                rate_trajectory_filepath = fertility_trajectory_filepath,
                                first_proj_yr = first_proj_yr,
                                n_proj_yr = n_proj_yr,
                                npp_var = fertility_npp_variant)



#-----------------------------------------------------

int_out_rate <- popmodules::calculate_mean_international_rates_or_flows(popn_mye_path = popn_mye_path,
                                                                        births_mye_path = births_mye_path,
                                                                        flow_or_rate = int_out_flow_or_rate,
                                                                        component_path = int_out_mye_path,
                                                                        last_data_year = int_out_last_data_year,
                                                                        n_years_to_avg = int_out_years_to_avg,
                                                                        data_col = "int_out",
                                                                        first_proj_yr = first_proj_yr,
                                                                        n_proj_yr = n_proj_yr,
                                                                        rate_cap = int_out_rate_cap)



int_in <- popmodules::calculate_mean_international_rates_or_flows(popn_mye_path = popn_mye_path,
                                                                  births_mye_path = births_mye_path,
                                                                  flow_or_rate = int_in_flow_or_rate,
                                                                  component_path = int_in_mye_path,
                                                                  last_data_year = int_in_last_data_year,
                                                                  n_years_to_avg = int_in_years_to_avg,
                                                                  data_col = "int_in",
                                                                  first_proj_yr = first_proj_yr,
                                                                  n_proj_yr = n_proj_yr)

#-----------------------------------------------------


dom_rates <- popmodules::calculate_domestic_rates_transition(dom_origin_destination_path,
                                                             popn_mye_path,
                                                             births_mye_path,
                                                             last_data_year_initial = dom_mig_last_data_year_initial,
                                                             years_to_avg_initial = dom_mig_years_to_avg_initial,
                                                             last_data_year_longterm = dom_mig_last_data_year_longterm,
                                                             years_to_avg_longterm = dom_mig_years_to_avg_longterm,
                                                             domestic_transition_yr = domestic_transition_yr)
#---------------------------------------------

dir.create(save_location)
saveRDS(dom_rates, paste0(save_location, "dom_rates.rds"))
saveRDS(int_in, paste0(save_location, "int_in.rds"))
saveRDS(int_out_rate, paste0(save_location, "int_out_rate.rds"))
saveRDS(fertility, paste0(save_location, "fertility.rds"))
saveRDS(mortality, paste0(save_location, "mortality.rds"))

#-----------------------------------------------------

###UPC
age_sex <- data.table::fread("c:/temp/scenario_projections/total_covid_deaths_by_age_sex.csv") %>%
  as.data.frame() %>%
  filter(sex %in% c("male","female"))

uk_2018_deaths <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2018/deaths_ons.rds") %>%
  filter(year == 2018) %>%
  group_by(sex, age) %>%
  summarise(deaths = sum(deaths)) %>%
  as.data.frame()

deaths_sya <- list()
for(i in unique(age_sex$age_group)){
  
  a <- filter(age_sex, age_group ==i)
  deaths_sya[[i]] <- distribute_within_age_band(a,
                                                uk_2018_deaths,
                                                "deaths", "deaths",
                                                unique(a$min), unique(a$max),
                                                col_aggregation=c("sex"))
}

deaths_sya <- data.table::rbindlist(deaths_sya) %>%
  as.data.frame() %>% 
  select(sex, age, deaths)

la <- data.table::fread("c:/temp/scenario_projections/total_deaths_by_LA.csv") %>%
  as.data.frame() %>%
  filter(sex %in% c("male","female"),
         substr(gss_code,1,3) %in% c("E06","E07","E08","E09","W06","S92","N92")) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","sex"))

death_structure <- deaths_sya %>%
  group_by(sex) %>%
  mutate(structure = deaths/sum(deaths)) %>%
  as.data.frame() %>% 
  select(-deaths)

la_sya <- left_join(la, death_structure, by="sex") %>%
  mutate(upc = deaths*structure,
         year = 2020) %>%
  select(year, gss_code, sex, age, upc)%>%
  constrain_component(deaths_sya, col_aggregation = c("sex","age"),
                      col_popn = "upc", col_constraint = "deaths")

saveRDS(la_sya, paste0(save_location,"covid19_upc.rds"))

#-----------------------------------------------------