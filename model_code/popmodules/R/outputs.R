output_component <- function(component_data, component, output_location,
                             col_aggregation=c("gss_code","year","sex","age"),
                             col_data){
  
  df <- component_data %>%
    region_data(col_aggregation, col_data) %>%
    group_sex(col_aggregation, col_data) %>%
    add_totals(col_aggregation, col_data) 
    
  saveRDS(df, paste0(output_location,"/",component,".rds")
  
}

region_data <- function(la_data, col_aggregation) {
  
  region <- left_join(region_lookup, la_data) %>%
    group_by(col_aggregation) %>%
    summarise(value = sum(col_data)) %>%
    ungroup() %>%
    rename(!!col_data := value)
  
  df <- rbind(la_data, region_data)
  
  return(df)
  
}

add_names <- function(df){
  
  nm <- c("gss_code", "gss_name", names(df)[names(df)!="gss_code"])
  
  df <- left_join(df, name_lookup) %>%
    select(nm)
  
  return(df)
  
}

add_totals <- function(df, col_aggregation=c("gss_code","year","sex","age", col_data){
  
  totals <- mutate(df, age = 99) %>%
    group_by(col_aggregation) %>%
    summarise(value = sum(col_data)) %>%
    ungroup() %>%
    rename(!!col_data := value)
  
  df <- rbind(df, totals)
  
  return(df)
  
} 

arrange_outputs <- function(df, data_col) {
  
  df <- left_join(df, arrange_order) %>%
    spread(year, data_col)
    arrange(sex, age, order) %>%
    mutate(age = ifelse(age == 99, "total", age))
  
}

group_sex <- function(df, col_aggregation, col_data){
  
  df <- df %>%
    mutate(sex = "persons") %>%
    group_by(col_aggregation) %>%
    summarise(value = sum(col_data)) %>%
    ungroup() %>%
    rename(!!col_data := calue)
  
  return(df)
  
}
