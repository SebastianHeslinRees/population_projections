

calculate_deaths <- function(in_df){
  
  asmr <- create_mortality_rates()
  
  deaths <- in_df%>%
    left_join(asmr, by = "age_interval")%>%
    mutate(value = -(value * mx))%>%
    select(-mx)
  
  return(deaths)
}

calculate_infant_deaths <- function(births){
# fixed mortality rate
  m0 = 0.005
  
  deaths <- births%>%
    mutate(value = -(value * m0))
  
  return(deaths)
}

create_mortality_rates <- function(m0 = 0.005, b = 0.02){
# simple Gompertz Law 

  age_interval <- seq(0, 100, by = 1)
  asmr <- tibble(age_interval)%>%
    mutate(mx = m0 * exp(b * age_interval))
  
  return(asmr)
}