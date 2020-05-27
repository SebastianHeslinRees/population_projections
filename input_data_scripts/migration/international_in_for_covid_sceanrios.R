#In some sceanrio we want to change the proportion of international in migration
#that comes to london. The UK total stays the same but the share among regions
#changes.

library(dplyr)
devtools::load_all('model_code/popmodules')

district_to_region <- readRDS("input_data/lookup/district_to_region.rds")

#####
proportion_to_redistribute <- 0.1 #redistribute 10% of UK inflow away from/to London


#10 year average
popn_mye_path <- "input_data/mye/2019/temp_gla_population.rds"
births_mye_path <- "input_data/mye/2019/temp_births.rds"
int_out_mye_path <- "input_data/mye/2019/temp_gla_international_out.rds"
int_in_mye_path <-  "input_data/mye/2019/temp_gla_international_in.rds"

int_in_flows <- popmodules::calculate_mean_international_rates_or_flows(
  popn_mye_path = popn_mye_path, births_mye_path = births_mye_path,
  flow_or_rate = "flow", component_path = int_in_mye_path,
  last_data_year = 2019, n_years_to_avg = 10, data_col = "int_in",
  first_proj_yr = 2020, n_proj_yr = 1, rate_cap = 0.8) 

#proportion of uk int in going to different regions
#for reference
int_in_proprtions <- int_in_flows %>% 
  left_join(district_to_region, by="gss_code") %>%
  group_by(region_gss_code, year) %>%
  summarise(int_in = sum(int_in)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(proportion = 100*(int_in/sum(int_in))) %>%
  data.frame() %>%
  mutate(dist_without_ldn = ifelse(region_gss_code == "E12000007",0,int_in),
         dist_without_ldn = ifelse(region_gss_code == "E12000007",0,
                                   int_in / sum(dist_without_ldn)))

#total uk in migration to redistribute
amount_to_change <- sum(int_in_flows$int_in)*proportion_to_redistribute

#reduction in London, increase elsewhere
increase_to_regions <- int_in_proprtions %>% 
  mutate(additional = ifelse(region_gss_code == "E12000007",
                             amount_to_change*-1,
                             amount_to_change * dist_without_ldn),
         new_total = additional+int_in,
         scaling = new_total/int_in) %>% 
  group_by(year) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_regions, region_gss_code=="E12000007")$scaling
other_scaling <- unique(filter(increase_to_regions, region_gss_code!="E12000007")$scaling)

int_in_flows_2 <- int_in_flows %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling))

#check
assertthat::assert_that(sum(int_in_flows_2$int_in)==sum(int_in_flows_2$new_int_in))

#project forward
int_in_flows_3 <- int_in_flows_2 %>%
  select(-int_in) %>%
  rename(int_in = new_int_in) %>%
  project_forward_flat(last_proj_yr = 2050)

saveRDS(int_in_flows_3, "input_data/mye/2019/temp_ons_international_in_reduced_ldn_share.rds")


#####
#Opposite thing: increase london, resuction everywhere else

increase_to_london <- int_in_proprtions %>% 
  mutate(additional = ifelse(region_gss_code == "E12000007",
                             amount_to_change,
                             amount_to_change * dist_without_ldn * -1),
         new_total = additional+int_in,
         scaling = new_total/int_in) %>% 
  group_by(year) %>% 
  mutate(check = 100*(new_total / sum(new_total))) %>%
  data.frame()

london_scaling <- filter(increase_to_london, region_gss_code=="E12000007")$scaling
other_scaling <- unique(filter(increase_to_london, region_gss_code!="E12000007")$scaling)

int_in_flows_4 <- int_in_flows %>%
  mutate(new_int_in = ifelse(substr(gss_code,1,3)=="E09",
                             int_in*london_scaling,
                             int_in*other_scaling))

#check
assertthat::assert_that(sum(int_in_flows_4$int_in)==sum(int_in_flows_4$new_int_in))

#project forward
int_in_flows_5 <- int_in_flows_4 %>%
  select(-int_in) %>%
  rename(int_in = new_int_in) %>%
  project_forward_flat(last_proj_yr = 2050)

saveRDS(int_in_flows_5, "input_data/mye/2019/temp_ons_international_in_increased_ldn_share.rds")


#Pre-accession int in & london reduction
int_in_flows_6 <- int_in_flows_3 %>%
  group_by(year) %>%
  mutate(proportion = int_in/sum(int_in)) %>%
  data.frame() %>%
  mutate(new_int_in = 483419*proportion) %>% #This is the value of int in in 2003
  select(-int_in, -proportion) %>%
  rename(int_in = new_int_in)
  
saveRDS(int_in_flows_5, "input_data/mye/2019/temp_ons_international_in_pre_accession_decreased_ldn_share.rds")
