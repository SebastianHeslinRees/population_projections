

calculate_components <- function(in_df){
  
  births <- calculate_births(in_df)
  deaths <- calculate_deaths(in_df)
  net_migration <- calculate_migration(in_df)
  
  infant_deaths <- calculate_infant_deaths(births)
  infant_net_migration <- calculate_infant_migration(births)
  
  components <- list("births" = births,
                     "deaths" = deaths, 
                     "net_migration" = net_migration, 
                     "infant_deaths" = infant_deaths, 
                     "infant_net_migration" = infant_net_migration)
  return(components)
}

age_on <- function(in_df, e_year, components){
  
  #accounts for deaths and migration of population who were age 0 to 90 at start of year
  out_df1 <- age_on_population(in_df, e_year, components)
  
  #adds births, accounting for deaths and migration of infants
  out_df2 <- age_on_births(out_df1, e_year, components)
  
  return(out_df2)
}

age_on_population <- function(in_df, e_year, components){
  
  max_interval <- pop_parameters$max_age_interval
  
  deaths <- components$deaths
  net_migration <- components$net_migration
  
  out_df <- in_df%>%
    
    bind_rows(deaths, net_migration)%>%
    
    mutate(year = e_year)%>%
    
    mutate(age_interval = case_when(age_interval == max_interval ~ age_interval,
                                    TRUE ~ age_interval + 1))%>%
    
    group_by_at(vars(-value))%>%
    summarise(value = sum(value))%>%
    ungroup()
  
  return(out_df)
}


age_on_births <- function(in_df, e_year, components){
  
  births <- components$births
  infant_deaths <- components$infant_deaths
  infant_net_migration <- components$infant_net_migration
  
  out_df <- in_df%>%
    
    bind_rows(births, infant_deaths, infant_net_migration)%>%

    mutate(year = e_year)%>%
    
    group_by_at(vars(-value))%>%
    summarise(value = sum(value))%>%
    ungroup()
  
  return(out_df)
}

get_year_pop <- function(in_df, selected_year){
  
  out_df <- in_df%>%
    filter(year == selected_year)
  
  return(out_df)
}

add_births <- function(pop, births){
  
  out_df <- bind_rows(pop, births)%>%
    select(-component)%>%
    group_by_at(vars(-value))%>%
    summarise(value = sum(value))%>%
    ungroup()
  
  return(out_df)
}
