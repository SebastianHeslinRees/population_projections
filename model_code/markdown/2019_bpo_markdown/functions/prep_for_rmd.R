prep_data_for_rmd <- function(bpo_data, gss, area_name){
  
  output_list <- list()
  
  filtered_bpo <- lapply(bpo_data, function(x) if(is.data.frame(x)){filter(x, gss_code == gss)})
  
  total_pop <- filtered_bpo[['population']] %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  short_term <- total_pop %>% 
    filter(year %in% 2019:2030)
  
  primary_pop <- filtered_bpo[['population']] %>%
    filter(age %in% 4:10) %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame()
  
  secondary_pop <- filtered_bpo[['population']] %>%
    filter(age %in% 11:15) %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame() 
  
  births <- filtered_bpo[['births']] %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(births), .groups = 'drop_last') %>% 
    data.frame() 
  
  deaths <- filtered_bpo[['deaths']] %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(deaths), .groups = 'drop_last') %>% 
    data.frame()
  
  migration <- filtered_bpo[['migration']] %>% 
    group_by(year, variant) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame() 
  
  popn_2011 <- filter(total_pop, year == 2011)$value
  pop_change <- total_pop %>% 
    mutate(value = value - popn_2011) 
  
  assumed_dev <- bpo_data[['assumed_dev']]
  
  if(unique(assumed_dev$gss_code) != gss){
    
    assumed_dev <- assumed_dev %>% 
      filter(gss_code_ward == gss) %>% 
      mutate(gss_code = gss_code_ward)
  }
  
  assumed_dev <- assumed_dev %>% 
    group_by(gss_code, year, variant) %>% 
    summarise(value = sum(units), .groups = 'drop_last') %>% 
    data.frame()
  
  output_list[['total_pop_chart']] <- line_chart_plotly(total_pop, title = "Total population", y_axis="persons", area_name)
  output_list[['primary_chart']] <- line_chart_plotly(primary_pop, title = "Primary age population", y_axis="persons", area_name)
  output_list[['secondary_chart']] <- line_chart_plotly(secondary_pop, title = "Secondary age population", y_axis="persons", area_name)
  output_list[['births_chart']] <- line_chart_plotly(births, title = "Total births", y_axis="births", area_name, TRUE)
  output_list[['deaths_chart']] <- line_chart_plotly(deaths, title = "Total deaths", y_axis="deaths", area_name, TRUE)
  output_list[['migration_chart']] <- line_chart_plotly(migration, title = "Total net migration", y_axis="net migration", area_name)
  output_list[['pop_change_chart']] <- line_chart_plotly(pop_change, title = "Population change", y_axis="Change since 2011 (persons)", area_name)
  output_list[['assumed_dev_chart']] <- bar_chart_plotly(assumed_dev, title = "Assumed development", y_axis="net additional units", area_name)
  output_list[['short_term_chart']] <- line_chart_plotly(short_term, title = "Total population (2019-2030)", y_axis="persons", area_name)
  
  age_structure <- filtered_bpo[['population']] %>% 
    filter(year %in% c(2011,2019,2034)) %>% 
    group_by(year, variant, age) %>% 
    summarise(value = sum(popn), .groups = 'drop_last') %>% 
    data.frame() 
  
  age_chart_plotlys <- list()
  i <- 0
  for(v in unique(age_structure$variant)){
    i <- i+1
    age_chart_plotlys[[i]] <- age_chart_plotly(age_structure, area_name, var = v)
  }
  
  output_list[['age_structure_charts']] <- age_chart_plotlys
  
  output_list[['age_table']] <- filtered_bpo[['population']] %>% 
    data.frame() %>% 
    mutate(age_group = cut(age, breaks = c(-1,3,10,15,64,79,Inf),
                           labels = c("under 4",
                                      "primary (4-10)",
                                      "secondary (11-15)",
                                      "working age (16-64)",
                                      "older (65-79)",
                                      "elderly (80 and over)"))) %>% 
    select(-gss_code, -sex, -age) %>% 
    rbind(total_pop %>% 
            mutate(age_group = "total",
                   ward_name = area_name) %>% 
            select(year, ward_name, variant, popn = value, age_group)) %>% 
    filter(year %in% c(2011, 2019, 2034)) %>% 
    group_by(age_group, year, variant) %>% 
    summarise(value = format(round(sum(popn),0), big.mark = ","), .groups = 'drop_last') %>% 
    data.frame() %>% 
    mutate(year = ifelse(year == 2034,
                         paste(variant, year),
                         year)) %>% 
    select(-variant) %>% 
    unique() %>% 
    tidyr::pivot_wider(names_from = "year", values_from = "value")
  
  output_list[['area_name']] <- area_name
  
  return(output_list)
  
}