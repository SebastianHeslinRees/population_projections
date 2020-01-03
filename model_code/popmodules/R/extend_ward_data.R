#TODO:
#checks and balances
#documentation
#rename
#need to add !! and sym()

extend_ward_data <- function(ward_data, borough_data, ward_data_col, borough_data_col,
                             col_aggregation=c("gss_code","sex")){
  
  max_ward_age <- max(ward_data$age)

  just_max_age <- filter(ward_data, age == max_ward_age)
  
  borough_distribution <- filter(borough_data, age >= max_ward_age) %>%
    group_by(col_aggregation) %>%
    mutate(borough_total = sum(borough_data_col)) %>%
    as.data.frame() %>%
    mutate(age_distribution = borough_data_col / borough_total) %>%
    select(gss_code, sex, age, age_distribution)
  
  if(!"gss_code" %in% names(just_max_age)){
    ward_lookup <- readRDS()
    just_max_age <- left_join(just_max_age, ward_lookup, by="gss_code_ward")
  }
  
  extended <- just_max_age %>%
    left_join(borough_distribution, by=c(col_aggregation,"age")) %>%
    mutate(ward_data_col = ward_data_col * age_distribution) %>%
    select(names(ward_data))
  
  output_df <- filter(ward_data, age != max_ward_age) %>%
    rbind(extended) %>%
    as.data.frame()
  
  return(output_df)

}