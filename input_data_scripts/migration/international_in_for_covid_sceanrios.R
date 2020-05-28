#In some sceanrio we want to change the proportion of international in migration
#that comes to london. The UK total stays the same but the share among regions
#changes.

library(dplyr)
devtools::load_all('model_code/popmodules')

int_flows_loc <- "input_data/mye/2019/"

district_to_region <- readRDS("input_data/lookup/district_to_region.rds")

#####
proportion_to_redistribute <- 0.1 #redistribute 10% of UK inflow away from/to London


#10 year average
popn_mye_path <- "input_data/mye/2019/temp_gla_population.rds"
births_mye_path <- "input_data/mye/2019/temp_births.rds"
int_out_mye_path <- "input_data/mye/2019/temp_gla_international_out.rds"
int_in_mye_path <-  "input_data/mye/2019/temp_gla_international_in.rds"

avg_10_yr <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path, births_mye_path = births_mye_path,
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
#for reference
int_in_proprtions <- avg_10_yr %>% 
  left_join(district_to_region, by="gss_code") %>%
  group_by(region_gss_code) %>%
  summarise(int_in = sum(int_in)) %>%
  ungroup() %>%
  mutate(proportion = 100*(int_in/sum(int_in))) %>%
  data.frame() %>%
  mutate(dist_without_ldn = ifelse(region_gss_code == "E12000007",0,int_in),
         dist_without_ldn = ifelse(region_gss_code == "E12000007",0,
                                   int_in / sum(dist_without_ldn)))

#total uk in migration to redistribute
amount_to_change <- sum(avg_10_yr$int_in)*proportion_to_redistribute

#reduction in London, increase elsewhere
increase_to_regions <- int_in_proprtions %>% 
  mutate(additional = ifelse(region_gss_code == "E12000007",
                             amount_to_change*-1,
                             amount_to_change * dist_without_ldn),
         new_total = additional+int_in,
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_regions, region_gss_code=="E12000007")$scaling
other_scaling <- unique(filter(increase_to_regions, region_gss_code!="E12000007")$scaling)

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

increase_to_london <- int_in_proprtions %>% 
  mutate(additional = ifelse(region_gss_code == "E12000007",
                             amount_to_change,
                             amount_to_change * dist_without_ldn * -1),
         new_total = additional+int_in,
         scaling = new_total/int_in) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_london, region_gss_code=="E12000007")$scaling
other_scaling <- unique(filter(increase_to_london, region_gss_code!="E12000007")$scaling)

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
  
saveRDS(int_in_flows_5, paste0(int_flows_loc,"int_in_pre_accession_reduced_ldn_share.rds"))
