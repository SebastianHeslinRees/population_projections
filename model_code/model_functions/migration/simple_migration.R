calculate_migration <- function(in_df){
  
  inflow <- calculate_inflows(in_df)
  outflow <- calculate_outflows(in_df)
  
  net_migration <- bind_rows(inflow, outflow)%>%
    group_by_at(vars(-value))%>%
    summarise(value = sum(value))%>%
    ungroup()
  
  return(net_migration)
}

calculate_infant_migration <- function(births){
  
  m0 = 0.1
  
  net_migration <- births%>%
    mutate(value = -(value * m0))
  
  return(net_migration)
}

calculate_inflows <- function(in_df){
 
  inflow <- in_df%>%
    mutate(value = case_when(between(age_interval, 20, 35) ~ 15,
                             TRUE ~ 0))
  return(inflow)
}

calculate_outflows <- function(in_df){
  
  outflow_rates <- create_outflow_rates()
  
  outflow <- in_df%>%
    left_join(outflow_rates, by = "age_interval")%>%
    mutate(value = -(value * mx))%>%
    select(-mx)
  
  return(outflow)
}

create_outflow_rates <- function(){
  
  age_interval <- seq(0, 90, by = 1)
  
  outflow_rates <- tibble(age_interval)%>%
    mutate(mx = case_when(between(age_interval, 0, 10) ~ 0.05,
                             between(age_interval, 19, 19) ~ 0.2,
                             between(age_interval, 30, 45) ~ 0.03,
                             TRUE ~ 0))
  return(outflow_rates)
}

