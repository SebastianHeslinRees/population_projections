return_domestic_rates <- function(domestic_rates, domestic_rates_info,
                                  projection_year, first_proj_yr){
  
  curr_yr_info <- filter(domestic_rates_info, year == projection_year)
  assertthat::assert_that(nrow(curr_yr_info)==1,msg="too many rows in domestic_rates_info")
  
  if(!is.na(curr_yr_info$path)){
    
    if(projection_year == first_proj_yr){
      
      #create the df in the first year
      domestic_rates <- readRDS(curr_yr_info$path) %>%
        rename(rate_2 = rate)
    }
    
    domestic_rates <- select(domestic_rates, gss_in, gss_out, sex, age, rate_2) %>%
      rename(rate_1 = rate_2)
    
    if(!is.na(curr_yr_info$next_path)){
      
      #add next ste of rates
      domestic_rates <- readRDS(curr_yr_info$next_path) %>%
        rename(rate_2 = rate) %>%
        full_join(domestic_rates, by = c("gss_in", "gss_out", "sex", "age"))
      
    }
    
  }
  
  if(curr_yr_info$transition){
    
    domestic_rates <- domestic_rates %>% 
      mutate(increment = (rate_2-rate_1)/curr_yr_info$transition_period,
             rate = rate_1+(increment*curr_yr_info$period_step))
    
  } else {
    
    domestic_rates <- mutate(domestic_rates, rate = rate_1)
  }
  
  return(domestic_rates)
  
}

.get_domestic_rates_info <- function(domestic, first_proj_yr, last_proj_yr){
  
  domestic_rates_info <- data.table::rbindlist(domestic, idcol="year") %>%
    as.data.frame() %>% 
    mutate(year= as.numeric(year)) %>%
    right_join(data.frame(year = first_proj_yr:last_proj_yr), by = "year") %>% 
    tidyr::fill(transition) %>%
    
    mutate(next_path = path) %>%
    tidyr::fill(next_path, .direction="up") %>%
    mutate(next_path = lead(next_path),
           next_path = ifelse(is.na(path),NA,next_path)) %>% 
    
    mutate(period_start = ifelse(!is.na(path),year, NA)) %>%
    tidyr::fill(period_start) %>%
    
    mutate(period_end = lead(period_start),
           period_end = ifelse(period_start==period_end,NA,period_end),
           period_end = ifelse(year == last_proj_yr, last_proj_yr+1, period_end)) %>%
    tidyr::fill(period_end, .direction = "up") %>%
    
    mutate(transition_period = period_end-period_start,
           period_step = year - period_start) %>% 
    select(-period_start, -period_end)
}