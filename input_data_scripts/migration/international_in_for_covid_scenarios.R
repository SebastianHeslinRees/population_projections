#In some sceanrio we want to change the proportion of international in migration
#that comes to london. The UK total stays the same but the share among regions
#changes.

library(dplyr)
library(popmodules)

int_flows_loc <- "input_data/mye/2019/"

district_to_region <- readRDS("input_data/lookup/district_to_region.rds")

#####
proportion_to_redistribute <- 0.1 #redistribute 10% of UK inflow away from/to London


#10 year average
int_out_mye_path <- "input_data/mye/2019/temp_gla_international_out.rds"
int_in_mye_path <-  "input_data/mye/2019/temp_gla_international_in.rds"

avg_10_yr <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = NULL, births_mye_path = NULL,
  flow_or_rate = "flow", component_path = int_in_mye_path,
  last_data_year = 2019, n_years_to_avg = 10, data_col = "int_in",
  first_proj_yr = 2020, n_proj_yr = 1, rate_cap = 0.8) %>%
  select(-year)

avg_20_yr_70 <- mutate(avg_10_yr, int_in = int_in*0.7)
avg_20_yr_30 <- mutate(avg_10_yr, int_in = int_in*0.3)
avg_20_yr_50 <- mutate(avg_10_yr, int_in = int_in*0.5)
avg_20_yr_80 <- mutate(avg_10_yr, int_in = int_in*0.8)

saveRDS(avg_10_yr, paste0(int_flows_loc,"int_in_10yr_avg_2019.rds"))
saveRDS(avg_20_yr_70, paste0(int_flows_loc,"int_in_10yr_avg_2019_70pc.rds"))
saveRDS(avg_20_yr_30, paste0(int_flows_loc,"int_in_10yr_avg_2019_30pc.rds"))
saveRDS(avg_20_yr_50, paste0(int_flows_loc,"int_in_10yr_avg_2019_50pc.rds"))
saveRDS(avg_20_yr_80, paste0(int_flows_loc,"int_in_10yr_avg_2019_80pc.rds"))

#proportion of uk int in going to different regions
int_in_proportions <- avg_10_yr %>%
  left_join(district_to_region, by="gss_code") %>%
  mutate(london = region_gss_code == "E12000007") %>%
  group_by(london) %>%
  summarise(int_in = sum(int_in)) %>%
  ungroup() %>%
  mutate(proportion = 100*(int_in/sum(int_in))) %>%
  data.frame()

#total uk in migration to redistribute
amount_to_change <- sum(avg_10_yr$int_in)*proportion_to_redistribute

#reduction in London, increase elsewhere
increase_to_regions <- int_in_proportions %>% 
  mutate(new_total = ifelse(london,
                            int_in + amount_to_change,
                            int_in - amount_to_change),
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_regions, london)$scaling
other_scaling <- filter(increase_to_regions, !london)$scaling

int_in_flows_2 <- avg_10_yr %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling))

#check
assertthat::assert_that(sum(int_in_flows_2$int_in)==sum(int_in_flows_2$new_int_in))

#project forward
int_in_flows_3 <- int_in_flows_2 %>%
  select(-int_in) %>%
  rename(int_in = new_int_in) 

saveRDS(int_in_flows_3, paste0(int_flows_loc,"int_in_reduced_ldn_share.rds"))


#####
#Opposite thing: increase london, resuction everywhere else
increase_to_regions <- int_in_proportions %>% 
  mutate(new_total = ifelse(london,
                            int_in - amount_to_change,
                            int_in + amount_to_change),
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_regions, london)$scaling
other_scaling <- filter(increase_to_regions, !london)$scaling

int_in_flows_4 <- avg_10_yr %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling))

#check
assertthat::assert_that(sum(int_in_flows_4$int_in)==sum(int_in_flows_4$new_int_in))

#project forward
int_in_flows_5 <- int_in_flows_4 %>%
  select(-int_in) %>%
  rename(int_in = new_int_in)

saveRDS(int_in_flows_5, paste0(int_flows_loc,"int_in_increased_ldn_share.rds"))


#Pre-accession int in & london reduction
int_in_flows_6 <- int_in_flows_3 %>%
  mutate(proportion = int_in/sum(int_in)) %>%
  data.frame() %>%
  mutate(new_int_in = 483419*proportion) %>% #This is the value of int in in 2003
  select(-int_in, -proportion) %>%
  rename(int_in = new_int_in)
  
saveRDS(int_in_flows_6, paste0(int_flows_loc,"int_in_pre_accession_reduced_ldn_share.rds"))

