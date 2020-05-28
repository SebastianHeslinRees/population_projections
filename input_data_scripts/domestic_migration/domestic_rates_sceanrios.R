#In some scenarios we want to affect the amount of migration coming into
#or going out of London without ammending the rest of the rates

library(dplyr)

amend_rates_by <- 0.5 #50%

#read in pre-calculated 10-year average rates
avg_flows <- readRDS("input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019.rds")

#scenario 8
#reduced inflow, maintain outflow
reduced_in_rate <- avg_flows %>%
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*amend_rates_by, rate))

saveRDS(reduced_in_rate, "input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019_reduced_ldn_in.rds")

#scenario 10         
#Inflow rates increased, outfow decreased
ldn <- avg_flows %>%
  filter(substr(gss_in,1,3)=="E09" & substr(gss_out,1,3)=="E09")

in_up_out_down <- avg_flows %>%
  filter(!(substr(gss_in,1,3)=="E09" & substr(gss_out,1,3)=="E09")) %>%
  mutate(rate = ifelse(substr(gss_in,1,3)=="E09", rate*(1+amend_rates_by), rate),
         rate = ifelse(substr(gss_out,1,3)=="E09", rate*amend_rates_by, rate)) %>%
  rbind(ldn)

saveRDS(in_up_out_down, "input_data/domestic_migration/processed_rates/dom_rates_10yr_avg_2019_inc_ldn_in_reduced_ldn_out.rds")

