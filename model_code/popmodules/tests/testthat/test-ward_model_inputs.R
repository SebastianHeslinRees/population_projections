ward_district_lookup <- readRDS("input_data/lookup/2011_ward_to_district.rds")
london_wards <- filter(ward_district_lookup, substr(gss_code,1,3) == "E09") %>%
  filter(gss_code != "E09000001")
london_wards <- sort(c(unique(london_wards$gss_code_ward), "E09000001"))

adults_per_dwelling <- "input_data/small_area_model/ward_adults_per_dwelling.rds"
pop_est <- "input_data/small_area_model/ward_population_estimates_2010_2017.rds"
ce_est <- "input_data/small_area_model/ward_communal_establishment_population.rds"
out_migration_rates <- "input_data/small_area_model/ward_out_migration_rates.rds"
in_migration_characteristics <- "input_data/small_area_model/ward_in_migration_characteristics.rds"
births <- "input_data/small_area_model/ward_births_2001_2018.rds"
deaths <- "input_data/small_area_model/ward_deaths_2001_2018.rds"

test_ward_inputs <- function(data_path, col_aggregation=c("gss_code_ward", "sex", "age")){
  
  data <- readRDS(data_path)
  
  #all wards are present
  testthat::test_that("missing wards",{
    expect_equal(sort(unique(data$gss_code_ward)), london_wards)})
  
  #no duplicates
  testthat::test_that("duplicates found",{
    expect_equal(nrow(group_by_at(data, col_aggregation) %>% 
                        filter(n()>1)), 0)})
  
  #All values exist
  a <- list()
  for(i in 1:length(col_aggregation)){
    var <- col_aggregation[[i]]
    a[[var]] <- unique(data[[var]])
  }
  a <- expand.grid(a)
  
  testthat::test_that("missing aggreagtion levels",{
    
    expect_equal(nrow(a),nrow(data))})
}

test_ward_inputs(data_path = adults_per_dwelling, col_aggregation = c("gss_code_ward", "year")) 
test_ward_inputs(pop_est, c("year", "gss_code_ward", "sex", "age"))
test_ward_inputs(ce_est)
test_ward_inputs(out_migration_rates)
test_ward_inputs(in_migration_characteristics)
test_ward_inputs(births, c("year", "gss_code_ward", "age_group"))
test_ward_inputs(deaths, c("year", "gss_code_ward", "sex", "age_group"))


