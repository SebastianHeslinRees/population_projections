library(dplyr)

interested_in_gss <- c("E09000001","E09000002","E09000003","W06000001","S92000003","N92000002","E12000007","E92000001")
interested_in_yrs <- 2016:2018

####

popn_mye_path <- paste0("input_data/mye/2018/population_gla_2019-11-13.rds")
deaths_mye_path <-  paste0("input_data/mye/2018/deaths_ons.rds")
births_mye_path <-  paste0("input_data/mye/2018/births_ons.rds")
int_out_mye_path <-  paste0("input_data/mye/2018/international_out_gla_2019-11-13.rds")
int_in_mye_path <-  paste0("input_data/mye/2018/international_in_gla_2019-11-13.rds")
dom_out_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_out.rds")
dom_in_mye_path <- paste0("input_data/domestic_migration/2018/domestic_migration_in.rds")
dom_origin_destination_path <- paste0("input_data/domestic_migration/2018/domestic_migration_flows_ons.rds")

mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"

fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"

popn_constraint_path <- "input_data/constraints/npp_2018_population_constraint.rds"
births_constraint_path <- "input_data/constraints/npp_2018_fertility_constraint.rds"
deaths_constraint_path <- "input_data/constraints/npp_2018_mortality_constraint.rds"
int_in_constraint_path <- "input_data/constraints/npp_2018_international_in_constraint.rds"
int_out_constraint_path <- "input_data/constraints/npp_2018_international_out_constraint.rds"
cross_in_constraint_path <- "input_data/constraints/npp_2018_cross_border_in_constraint.rds"
cross_out_constraint_path <- "input_data/constraints/npp_2018_cross_border_out_constraint.rds"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates.rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016.rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population.rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014.rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014.rds"


a <- function(x, interested_in_yrs=interested_in_yrs, interested_in_gss=interested_in_gss){
  
  x <- readRDS(x) %>%
    filter(year %in% interested_in_yrs,
           gss_code %in% interested_in_gss)
  
  return(x)
}

pop <- a(popn_mye_path)
deaths <- a(deaths_mye_path)
births <- a(births_mye_path)
int_in <- a(int_in_mye_path)
int_out <- a(int_out_mye_path)

dom <- readRDS(dom_origin_destination_path) %>%
  filter(year %in% interested_in_yrs,
         gss_in %in% interested_in_gss,
         gss_out %in% interested_in_gss)

dom_in <- group_by(dom, year, gss_code = gss_in, sex, age) %>%
  summarise(dom_in = sum(value)) %>%
  as.data.frame() %>%
  tidyr::complete(year = interested_in_yrs,
                  gss_code = unique(int_out$gss_code),
                  sex = c("male","female"),
                  age = 0:90, fill = list(dom_in = 0))

dom_out <- group_by(dom, year, gss_code = gss_out, sex, age) %>%
  summarise(dom_out = sum(value)) %>%
  as.data.frame()%>%
  tidyr::complete(year = interested_in_yrs,
                  gss_code = unique(int_out$gss_code),
                  sex = c("male","female"),
                  age = 0:90, fill = list(dom_out = 0))

mort_curve <- a(mortality_curve_filepath)
fert_curve <- a(fertility_curve_filepath)

ons1 <- a(ons_stage1_file_path)
ons2 <- a(ons_stage2_file_path)
ce <- a(communal_est_pop_path)
dclg1 <- a(dclg_stage1_file_path)
dclg2 <- a(dclg_stage2_file_path)

b <- function(x){
  
  nm <- last(names(x))
  
  x <- x %>%
    mutate(country = substr(gss_code,1,1)) %>%
    group_by(country, year, sex, age) %>%
    summarise(value = sum(!!sym(nm))) %>%
    as.data.frame() %>%
    mutate(value = value*1.3) %>%
    filter(year == 2018) %>%
    project_forward_flat(2020) %>%
    rename(!!nm := value)
  
  return(x)
}

pop_constraint <- b(pop)
births_constraint <- b(births)
deaths_constraint <- b(deaths)
int_in_constraint <- b(int_in)
int_out_constraint <- b(int_out)
cross_in_constraint <- b(dom_in) %>% rename(cross_in = dom_in)
cross_out_constraint <- b(dom_out) %>% rename(cross_out = dom_out)

saveRDS(pop, "input_data/test_data/pop_test.rds")
saveRDS(deaths, "input_data/test_data/deaths_test.rds")
saveRDS(births, "input_data/test_data/births_test.rds")
saveRDS(int_in, "input_data/test_data/int_in_test.rds")
saveRDS(int_out, "input_data/test_data/int_out_test.rds")
saveRDS(dom_in, "input_data/test_data/dom_in_test.rds")
saveRDS(dom_out, "input_data/test_data/dom_out_test.rds")
saveRDS(dom, "input_data/test_data/dom_flows_test.rds")
saveRDS(mort_curve, "input_data/test_data/mort_curve_test.rds")
saveRDS(fert_curve, "input_data/test_data/fert_curve_test.rds")
saveRDS(ons1, "input_data/test_data/ons_stage1_test.rds")
saveRDS(ons2, "input_data/test_data/ons_stage2_test.rds")
saveRDS(ce, "input_data/test_data/communal_est_pop_test.rds")
saveRDS(dclg1, "input_data/test_data/dclg_stage1_test.rds")
saveRDS(dclg2, "input_data/test_data/dclg_stage2_test.rds")
saveRDS(pop_constraint, "input_data/test_data/test_pop_constraint.rds")
saveRDS(births_constraint, "input_data/test_data/test_birth_constraint.rds")
saveRDS(deaths_constraint, "input_data/test_data/test_death_constraint.rds")
saveRDS(int_in_constraint, "input_data/test_data/test_int_in_constraint.rds")
saveRDS(int_out_constraint, "input_data/test_data/test_int_out_constraint.rds")
saveRDS(cross_in_constraint, "input_data/test_data/test_cross_in_constraint.rds")
saveRDS(cross_out_constraint, "input_data/test_data/test_cross_out_constraint.rds")
