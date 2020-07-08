library(dplyr)

data_location <- "Q:/Teams/D&PA/Data/population_estimates/ons_mid_year_estimates/current_series/mye_2019/"
output_location <- "input_data/mye/2019/"

all_components <- readRDS(paste0(data_location, "mye_2019_detailed_components_of_change_(2020_geog).rds"))

#----------------------

unique(all_components$component)

split_components <- function(data, comp, data_col=comp, check_neg=TRUE){
  
  if(check_neg){
    x <- filter(data, component == comp) %>%
      select(gss_code, year, sex, age, value)  %>%
      check_negative_values("value") %>%
      rename(!!sym(data_col) := value)
  } else {
    x <- filter(data, component == comp) %>%
      select(gss_code, year, sex, age, value)  %>%
      rename(!!sym(data_col) := value)
  }
  
}

population <- split_components(all_components, "population", "popn")
births <- split_components(all_components, "births")
deaths <-  split_components(all_components, "deaths")
dom_in <- split_components(all_components, "internal_in", "dom_in")
dom_out <- split_components(all_components, "internal_out", "dom_out")
int_in <- split_components(all_components, "international_in", "int_in")
int_out <- split_components(all_components, "international_out", "int_out")

dom_net <- split_components(all_components, "internal_net", "dom_net", check_neg=FALSE)
int_net <- split_components(all_components, "international_net", "int_net", check_neg=FALSE)

other <- rbind(split_components(all_components, "unattrib", "upc", check_neg=FALSE),
               split_components(all_components, "other_adjust", "upc", check_neg=FALSE),
               split_components(all_components, "special_change", "upc", check_neg=FALSE)) %>%
  group_by(gss_code, year, sex, age) %>% 
  summarise(upc = sum(upc)) %>% 
  data.frame()

#------------

saveRDS(population, paste0(output_location, "population_ons.rds"))
saveRDS(births, paste0(output_location, "births_ons.rds"))
saveRDS(deaths, paste0(output_location, "deaths_ons.rds"))
saveRDS(dom_in, paste0(output_location, "dom_in_ons.rds"))
saveRDS(dom_out, paste0(output_location, "dom_out_ons.rds"))
saveRDS(dom_net, paste0(output_location, "dom_net_ons.rds"))
saveRDS(int_in, paste0(output_location, "int_in_ons.rds"))
saveRDS(int_out, paste0(output_location, "int_out_ons.rds"))
saveRDS(int_net, paste0(output_location, "int_net_ons.rds"))
saveRDS(other, paste0(output_location, "upc_ons.rds"))

rm(list=ls())
