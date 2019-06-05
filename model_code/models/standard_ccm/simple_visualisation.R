create_population_plots <- function(in_df){
  
  age_plot <- plot_age_structure(in_df, years = c(2011, 2036, 2071, 2096), e_age = 84)
  total_plot <- plot_total_pop_year(in_df, 2011, 2100)
  
  plots <- list("age_structure" = age_plot, "total_pop" = total_plot)
  
  return(plots)
}

plot_age_structure <- function(in_df, years = c(base_year, end_year), s_age = 0, e_age = 90){
  
  age_plot <- in_df%>%
    filter(year %in% years)%>%
    filter(between(age, s_age, e_age))%>%
    mutate(year = as.factor(year))%>%
    ggplot(aes(x = age, y = value, group = year, col = year)) + geom_line(size = 1.05) +
    ylim(0, NA) + ylab("Population") + xlab("Age") +
    ggtitle("Population by age") +
    facet_wrap("area")
  
  return(age_plot)
}

plot_total_pop_year <- function(in_df, s_year, e_year){
  
  total_pop <- in_df%>%
    filter(between(year, s_year, e_year))%>%
    group_by(area, year)%>%
    summarise(value = sum(value))%>%
    ungroup()%>%
    ggplot(aes(x = year, y = value, group = area, col = area)) + geom_line(size = 1.05) +
    ylim(0, NA) + ylab("Population") + xlab("Year") +
    ggtitle("Population by age") +
    facet_wrap("area")
  
  return(total_pop)
}


create_component_plots <- function(in_df){
  
  birth_plot <- plot_births(in_df, s_year = 2011, e_year = 2100)
  
  plots <- list("births" = birth_plot)
  
  return(plots)
}

plot_births <- function(in_df, s_year, e_year){
  
  birth_plot <- in_df%>%
    filter(component == "births")%>%
    filter(between(year, s_year, e_year))%>%
    ggplot(aes(x = year, y = value, group = area, col = area)) + geom_line(size = 1.05) +
    ylim(0, NA) + ylab("Births") + xlab("Year") +
    ggtitle("Births") +
    facet_wrap("area")
  
  return(birth_plot)
}

