#In some scenarios we want to change the proportion of international in migration
#that comes to London. The UK total stays the same but the share among regions
#changes.

library(dplyr)
library(popmodules)

int_flows_loc <- "input_data/mye/2019/"

district_to_region <- readRDS("input_data/lookup/district_to_region.rds")

proportion_to_redistribute <- 0.1 #redistribute 10% of UK inflow away from/to London

#-------------------------------------------------------------------------------

#10 year average
int_out_mye_path <- "input_data/mye/2019/int_out_ons.rds"
int_in_mye_path <-  "input_data/mye/2019/int_in_ons.rds"

avg_10_yr <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL,
  births_mye_path = NULL,
  flow_or_rate = "flow",
  component_path = int_in_mye_path,
  last_data_year = 2019,
  n_years_to_avg = 10,
  data_col = "int_in",
  first_proj_yr = 2020,
  n_proj_yr = 1,
  rate_cap = 0.8) %>%
  select(-year)

avg_10_yr_80 <- mutate(avg_10_yr, int_in = int_in*0.8)
avg_10_yr_70 <- mutate(avg_10_yr, int_in = int_in*0.7)
avg_10_yr_50 <- mutate(avg_10_yr, int_in = int_in*0.5)
avg_10_yr_30 <- mutate(avg_10_yr, int_in = int_in*0.3)

saveRDS(avg_10_yr_80, paste0(int_flows_loc,"int_in_10yr_avg_2019_80pc.rds"))
saveRDS(avg_10_yr_70, paste0(int_flows_loc,"int_in_10yr_avg_2019_70pc.rds"))
saveRDS(avg_10_yr_50, paste0(int_flows_loc,"int_in_10yr_avg_2019_50pc.rds"))
saveRDS(avg_10_yr_30, paste0(int_flows_loc,"int_in_10yr_avg_2019_30pc.rds"))

rm(avg_10_yr_80, avg_10_yr_70, avg_10_yr_50, avg_10_yr_30)

#------------------------------------------------------------------------------

#proportion of uk int in going to different regions

int_in_proportions <- avg_10_yr %>%
  left_join(district_to_region, by="gss_code") %>%
  mutate(london = region_gss_code == "E12000007") %>%
  group_by(london) %>%
  summarise(int_in = sum(int_in), .groups = 'drop_last') %>%
  ungroup() %>%
  mutate(proportion = 100*(int_in/sum(int_in))) %>%
  data.frame()

#total uk in migration to redistribute
amount_to_change <- sum(avg_10_yr$int_in)*proportion_to_redistribute

#reduction in London, increase elsewhere
increase_to_regions <- int_in_proportions %>% 
  mutate(new_total = ifelse(london,
                            int_in - amount_to_change,
                            int_in + amount_to_change),
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_regions, london)$scaling
other_scaling <- filter(increase_to_regions, !london)$scaling

increase_to_regions <- avg_10_yr %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling)) %>%
  select(-int_in) %>%
  rename(int_in = new_int_in)

saveRDS(increase_to_regions, paste0(int_flows_loc,"int_in_reduced_ldn_share.rds"))

#------------------------------------------------------------------------------

#Opposite thing: increase London, reduction everywhere else

increase_to_london <- int_in_proportions %>% 
  mutate(new_total = ifelse(london,
                            int_in + amount_to_change,
                            int_in - amount_to_change),
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_london, london)$scaling
other_scaling <- filter(increase_to_london, !london)$scaling

increase_to_london <- avg_10_yr %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling)) %>%
  select(-int_in) %>%
  rename(int_in = new_int_in)

saveRDS(increase_to_london, paste0(int_flows_loc,"int_in_increased_ldn_share.rds"))

#------------------------------------------------------------------------------

#Pre-accession int in & London reduction

pre_accession <- increase_to_regions %>%
  mutate(proportion = int_in/sum(int_in)) %>%
  data.frame() %>%
  mutate(new_int_in = 483419*proportion) %>% #This is the value of int in in 2003
  select(-int_in, -proportion) %>%
  rename(int_in = new_int_in)

saveRDS(pre_accession, paste0(int_flows_loc,"int_in_pre_accession_reduced_ldn_share.rds"))

#------------------------------------------------------------------------------

#proportion of uk int in switched from London to WSE

#total uk in migration to redistribute
amount_to_change <- sum(avg_10_yr$int_in)*proportion_to_redistribute

#reduction in London
ldn <- avg_10_yr %>%
  left_join(district_to_region, by="gss_code") %>%
  filter(region_gss_code == "E12000007") %>%
  mutate(region_total = sum(int_in),
         new_total = region_total - amount_to_change,
         scaling = new_total / region_total,
         int_in = scaling * int_in) %>%
  data.frame() %>% 
  select(names(avg_10_yr))

wse <- avg_10_yr %>%
  left_join(district_to_region, by="gss_code") %>% 
  filter(region_gss_code %in% c("E12000006", "E12000008")) %>%
  mutate(region_total = sum(int_in),
         new_total = region_total + amount_to_change,
         scaling = new_total / region_total,
         int_in = scaling * int_in) %>%
  data.frame() %>% 
  select(names(avg_10_yr))

other <- avg_10_yr %>% 
  filter(!gss_code %in% c(unique(ldn$gss_code),unique(wse$gss_code)))

redist_to_wse <- rbind(other, wse, ldn)   

sum(redist_to_wse$int_in)
sum(avg_10_yr$int_in)

saveRDS(redist_to_wse, paste0(int_flows_loc,"int_in_reduced_ldn_increased_wse.rds"))

rm(list = ls())
