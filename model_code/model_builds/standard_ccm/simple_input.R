#read in the population estimates
get_pop_estimates <- function(file_path, format = "rds"){
  
  supported <- c("rds", "csv")
  
  if(!format %in% supported) stop("file format not supported")
  
  #read data
  if(format == "rds"){
   pop <- readRDS(file_path)
  } 
  
  if(format == "csv"){
    pop <- read_csv(file_path)
  }
  
  return(pop)
}

# format estimates data ready for use in model
ft_pop_estimates <- function(in_pop){
  
  dim_pop <- dimension_data(in_pop)
  out_pop <- enforce_max_age(dim_pop)
  
  return(out_pop)
}

#reduce population dataframe to specified dimensions
dimension_data <- function(in_df){
  
  pop_dimensions <- pop_parameters$pop_dimensions
  
  out_df <- in_df%>%
    group_by_at(vars(pop_dimensions))%>%
    summarise(value = sum(value))%>%
    ungroup()
  
  return(out_df)
}

# ensure specified maximum age is applied 
enforce_max_age <- function(in_df){
  
  remove_interval <- FALSE
  
  if(!"age_interval" %in% colnames(in_df)){
    
    remove_interval <- TRUE
    in_df <- add_age_interval(in_df)
  } 
  
  if("age" %in% colnames(in_df)){
    
    add_age_back <- TRUE
    in_df <- in_df%>%
      select(-age)
  } 
  
  max_interval <- pop_parameters$max_age_interval
  
  out_df <- in_df%>%
    mutate(age_interval = case_when(age_interval > max_interval ~ max_interval,
                                    TRUE ~ age_interval))%>%
             group_by_at(vars(-value))%>%
             summarise(value = sum(value))%>%
             ungroup()
  
  #these two steps try and ensure the dataframe is returned with the same columns as it was passed in with
  if(add_age_back) out_df <- add_age(out_df)
  if(remove_interval) out_df <- out_df%>%select(-age_interval)  
  
  return(out_df)
}

# populations ccm_pop with data for the base year
# age is replaced by age_interval 
create_pop_ccm <- function(in_pop = pop_estimates, base_year = base_year){
  
  base_pop <- in_pop%>%
    filter(year == base_year)
  
  out_df <- add_age_interval(base_pop)%>%
    select(-age)
  
  return(out_df)
}

#return the parameters that define the population data
get_population_parameters <- function(){
  
  # this is the size of the age and year intervals (they should match)
  interval_size <- 1
  
  #maximum age of the population data - this is assumed to be a 90+ style category
  max_age  <- 85
  
  max_age_interval <- get_max_age_interval(max_age)
  
  # dimensions of the population data, i.e. age, ethnicity, sex, area residence (this also provides the preferred sort order)
  # if the input data has more than specified, it is collapsed to these only
  pop_dimensions <- c("area", "year", "age")
  
  pop_parameters <- list("interval_size" = interval_size, 
                         "max_age" = max_age, 
                         "max_age_interval" = max_age_interval,
                         "pop_dimensions" = pop_dimensions)
  
  return(pop_parameters)
}

#find age interval that corresponds to specified max_age 
get_max_age_interval <- function(max_age){
  
  lookup <- create_age_interval_lookup()
  j <- match(max_age, lookup$age)
  max_age_interval <- lookup$age_interval[j]
  
  return(max_age_interval)
}

#add age interval to a dataframe that has an age variable
add_age_interval <- function(in_df){
  
  lookup <- create_age_interval_lookup()
  
  out_df <- in_df%>%
    left_join(lookup, by = "age")
  
  return(out_df)
}

# add age to a dataframe that has an age_interval variable
add_age <- function(in_df){
  
  lookup <- create_age_interval_lookup()
  
  out_df <- in_df%>%
    left_join(lookup, by = "age_interval")
  
  return(out_df)
}

# create lookup used to convert between age and age interval
create_age_interval_lookup <- function(){

  # for single year of age, this is simply a 1:1 mapping  
  age <- seq(0, 90, by = 1)
  age_interval <- seq(0, 90, by = 1)
  lookup <- tibble(age, age_interval)
  
  return(lookup)
}

#create population output dataframe from ccm_pop - in this case it's just a conversion back to age
ft_output_pop <- function(ccm_pop){

  in_df <- bind_rows(ccm_pop)
  out_df <- add_age(in_df)%>%
    select(-age_interval)
  
  return(out_df)
}

#TODO this is all a bit clumsy
#create components of change output dataframe from ccm_coc
ft_output_coc <- function(ccm_coc){
  
  ccm_coc <- ccm_coc[-1]
  ft_coc <- lapply(ccm_coc, ft_ccm_coc)
  coc_years <- as.list(names(ft_coc))
  
  out_df <- bind_rows(mapply(add_coc_year, ft_coc, coc_years, SIMPLIFY = FALSE))
  
  out_df <- add_age(out_df)%>%
    select(-age_interval)
  
  return(out_df)
}

ft_ccm_coc <- function(coc){
  coc_names <- as.list(names(coc))
  out_df <- bind_rows(mapply(add_coc_names, coc, coc_names, SIMPLIFY = FALSE))
  
  return(out_df)
}

add_coc_year <- function(in_coc, coc_year){
  
  out_df <- as_tibble(bind_rows(in_coc))%>%
    mutate(year = as.numeric(coc_year))
  
  return(out_df)
}

add_coc_names <- function(in_coc, component_name){
  
  out_df <- as_tibble(bind_rows(in_coc))%>%
    mutate(component = component_name)
  
  return(out_df)
}
