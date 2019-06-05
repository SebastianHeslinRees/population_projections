

calculate_births <- function(in_df){
  
  #if(length(unique(in_df$year)) != 1) stop("non-unique year for input to birth calculations")
  
  births <- in_df%>%
    filter(between(age_interval, 20, 40))%>%
    group_by_at(vars(-value, -age_interval))%>%
    summarise(value = mean(value))%>%
    ungroup()%>%
    mutate(age_interval = 0)
  
  return(births)
}
