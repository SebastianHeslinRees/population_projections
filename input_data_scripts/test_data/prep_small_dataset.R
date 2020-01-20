library(dplyr)

interested_in_gss <- c("E09000001","E09000002","E09000003","W06000001","S92000003","N92000002","E12000007","E92000001")
interested_in_yrs <- 2016:2018


####

popn_mye_path <- "input_data/mye/2018/population_gla_2019-11-13.rds"
deaths_mye_path <-  "input_data/mye/2018/deaths_ons.rds"
births_mye_path <-  "input_data/mye/2018/births_ons.rds"
int_out_mye_path <-  "input_data/mye/2018/international_out_gla_2019-11-13.rds"
int_in_mye_path <-  "input_data/mye/2018/international_in_gla_2019-11-13.rds"
dom_out_mye_path <- "input_data/domestic_migration/2018/domestic_migration_out.rds"
dom_in_mye_path <- "input_data/domestic_migration/2018/domestic_migration_in.rds"
dom_origin_destination_path <- "input_data/domestic_migration/2018/domestic_migration_flows_ons.rds"

mortality_curve_filepath <- "input_data/mortality/ons_asmr_curves.rds"
mortality_trajectory_filepath <- "input_data/mortality/npp_mortality_trend.rds"

fertility_curve_filepath <- "input_data/fertility/ons_asfr_curves.rds"
fertility_trajectory_filepath <- "input_data/fertility/npp_fertility_trend.rds"

# popn_constraint_path <- "input_data/constraints/npp_2018_population_constraint.rds"
births_constraint_path <- "input_data/constraints/npp_2018_fertility_constraint.rds"
# deaths_constraint_path <- "input_data/constraints/npp_2018_mortality_constraint.rds"
# int_in_constraint_path <- "input_data/constraints/npp_2018_international_in_constraint.rds"
# int_out_constraint_path <- "input_data/constraints/npp_2018_international_out_constraint.rds"
# cross_in_constraint_path <- "input_data/constraints/npp_2018_cross_border_in_constraint.rds"
# cross_out_constraint_path <- "input_data/constraints/npp_2018_cross_border_out_constraint.rds"

ons_stage1_file_path <- "input_data/household_model/ons_household_representative_rates.rds"
ons_stage2_file_path <- "input_data/household_model/ons_headship_rates_2016.rds"
communal_est_pop_path <- "input_data/household_model/ons_communal_establishment_population.rds"
dclg_stage1_file_path <- "input_data/household_model/dclg_stage1_data_2014.rds"
dclg_stage2_file_path <- "input_data/household_model/dclg_headship_rates_2014.rds"


dir.create("input_data/test_data", showWarnings = FALSE)

read_and_reduce <- function(path, interested_yrs=interested_in_yrs, interested_gss=interested_in_gss){
  readRDS(path) %>%
    filter(year %in% interested_yrs,
           gss_code %in% interested_gss)
}

pop <- read_and_reduce(popn_mye_path)
deaths <- read_and_reduce(deaths_mye_path)
births <- read_and_reduce(births_mye_path)
int_in <- read_and_reduce(int_in_mye_path)
int_out <- read_and_reduce(int_out_mye_path)

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

mort_curve <- read_and_reduce(mortality_curve_filepath)
fert_curve <- read_and_reduce(fertility_curve_filepath)

ons1 <- read_and_reduce(ons_stage1_file_path)
ons2 <- read_and_reduce(ons_stage2_file_path)
ce <- read_and_reduce(communal_est_pop_path)
dclg1 <- read_and_reduce(dclg_stage1_file_path)
dclg2 <- read_and_reduce(dclg_stage2_file_path)

summarise_by_country_and_extend_to_2020 <- function(df){
  
  data_col <- last(names(df))
  
  df %>%
    mutate(country = substr(gss_code,1,1)) %>%
    group_by(country, year, sex, age) %>%
    summarise(value = sum(!!sym(data_col))) %>%
    as.data.frame() %>%
    mutate(value = value*1.3) %>%
    filter(year == 2018) %>%
    popmodules::project_forward_flat(2020) %>%
    rename(!!data_col := value)
}

pop_constraint <- summarise_by_country_and_extend_to_2020(pop)
deaths_constraint <- summarise_by_country_and_extend_to_2020(deaths)
int_in_constraint <- summarise_by_country_and_extend_to_2020(int_in)
int_out_constraint <- summarise_by_country_and_extend_to_2020(int_out)
cross_in_constraint <- summarise_by_country_and_extend_to_2020(dom_in) %>% rename(cross_in = dom_in)
cross_out_constraint <- summarise_by_country_and_extend_to_2020(dom_out) %>% rename(cross_out = dom_out)

# we do this differently because we constrain births by mother's age, and the MYEs don't contain that
births_constraint <- readRDS(births_constraint_path) %>%
  filter(year == 2019) %>%
  popmodules::complete_fertility(population = pop_constraint, col_rate = "births") %>%
  group_by(country, year, sex, age) %>%
  summarise(births = sum(births)) %>%
  ungroup() %>%
  # Correct the constraints for countries we're only modelling part of
  mutate(births = case_when(country == "E" ~ 3 * births / 326,
                            country == "W" ~ births/22,
                            TRUE ~ births)) %>%
  popmodules::project_forward_flat(2020)

  
  
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
