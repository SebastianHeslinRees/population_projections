int_in <- readRDS("~/Projects/population_projections/input_data/mye/2018/international_in_ons_2019-10-11.rds")
int_out <- readRDS("~/Projects/population_projections/input_data/mye/2018/international_out_ons_2019-10-11.rds")
pop <- readRDS("~/Projects/population_projections/input_data/mye/2018/population_ons_2019-10-11.rds")

calc_mean_rate <- function(rate_backseries, years_to_avg, last_data_year, rate_col){
  
  assert_that(is.data.frame(rate_backseries),
              msg="calc_trend_rate expects that rate_backseries is a dataframe")
  assert_that(is.numeric(years_to_avg),
              msg="calc_trend_rate expects that years_to_avg is an numeric")
  assert_that(is.numeric(last_data_year),
              msg="calc_trend_rate expects that last_data_year is an numeric")
  assert_that(is.character(rate_col),
              msg="calc_trend_rate expects that rate_col is a column")
  assert_that(rate_col %in% names(rate_backseries),
              msg = "in calc_trend_rate the rtae_col variable must be the name of a column in the rate_backseries dataframe")
  
  back_years <- c((last_data_year - years_to_avg + 1):last_data_year)
  
  averaged <- rate_backseries %>%
    rename(rate = rate_col) %>%
    filter(year %in% back_years) %>%
    group_by(gss_code, sex, age) %>%
    summarise(rate = sum(rate)/years_to_avg) %>%
    ungroup() %>%
    mutate(year = last_data_year+1) %>%
    select(gss_code, year, sex, age, rate) %>%
    rename(!!rate_col := rate)
  
  return(averaged)
  
}

#----------------------------------------------------------

international_rates <- function(population, component, years_to_avg, last_data_year, data_col, n_proj_years) {
  
  population <- popn_age_on(population) %>%
    filter(population, year %in% unique(component$year))
  
  rate_backseries <- left_join(population, component, by=c("gss_code","year","sex","age")) %>%
    rename(value = data_col) %>%
    mutate(rate = ifelse(popn == 0, 0, value/popn)) %>%
    select(gss_code, year, sex, age, rate)
  
  rates <- calc_mean_rate(rate_backseries, years_to_avg, last_data_year, "rate")
  
  rates <- international_trajectory(rates, last_data_year, n_proj_years) %>%
    rbind(rate_backseries)
  
  return(rates)
  
}
  
international_flows <- function(component, years_to_avg, last_data_year, flow_col, n_proj_years){
  
  flow_backseries <- select(component, gss_code, year, sex, age, flow_col)
  
  flows <- calc_mean_rate(component, years_to_avg, last_data_year, flow_col)
  
  flows <- international_trajectory(flows, last_data_year, n_proj_years) %>%
    rbind(flow_backseries)
  
  return(flows)
  
}


international_trajectory <- function(rates, last_data_year, n_proj_years){
  
  trajectory <- myls <- vector("list", length = last_data_year+n_proj_years)
  
  for(yr in c(last_data_year:(last_data_year+n_proj_years))){
    trajectory[[yr]] <- mutate(rates, year = yr)
  }
  
  trajectory <- data.table::rbindlist(trajectory)
  
  return(trajectory)
  
}
  
  population <- pop
  component <- int_out
  years_to_avg <- 5
  last_data_year <- max(component$year)
  data_col <- "int_out"
  n_proj_years <- 25
  flow_col <- "int_in"
  
  rates <- international_rates(population, component, years_to_avg, last_data_year, rate_col, n_proj_years)
  flows <- international_flows(int_in, years_to_avg, last_data_year, flow_col, n_proj_years)
  

                               