#revised GLA estimates of population & international flows
#data created in a separate process
#this script calls on the processed data
#data created by ML November 2020

message("2020 GLA adjusted MYE")

gla_estimates_dir <- "Q:/Teams/D&PA/Data/population_estimates/gla_adjusted_mid_year_estimates/"

# #read in data
gla_popn <- readRDS(paste0(gla_estimates_dir, "gla_popn.rds"))
gla_int_in <- readRDS(paste0(gla_estimates_dir, "gla_int_in.rds"))
gla_int_out <- readRDS(paste0(gla_estimates_dir, "gla_int_out.rds"))

ons_popn <- readRDS("input_data/mye/2020/population_ons.rds")
ons_int_in <- readRDS("input_data/mye/2020/int_in_ons.rds")
ons_int_out <- readRDS("input_data/mye/2020/int_out_ons.rds")

#deal with negative populations
neg_pop <- filter(gla_popn, popn < 0)

message(paste("changing", nrow(neg_pop),"negative rows to 0 in population, adding to int_in"))

gla_popn <- gla_popn %>% 
  mutate(popn = ifelse(popn < 0, 0, popn))

gla_int_in <- neg_pop %>% 
  mutate(int_in = popn*-1) %>% 
  select(-popn) %>% 
  rbind(gla_int_in) %>% 
  group_by(gss_code, year, sex, age) %>% 
  summarise(int_in = sum(int_in), .groups = 'drop_last') %>% 
  data.frame()

#combine gla estimates for London with ONS for everywhere else
popn <- filter(ons_popn, substr(gss_code,1,3)!="E09") %>% 
  rbind(gla_popn)%>% 
  arrange(gss_code, year, sex, age)

int_in <-filter(ons_int_in, substr(gss_code,1,3)!="E09") %>% 
  rbind(gla_int_in)%>% 
  arrange(gss_code, year, sex, age)

int_out <-filter(ons_int_out, substr(gss_code,1,3)!="E09") %>% 
  rbind(gla_int_out)%>% 
  arrange(gss_code, year, sex, age)

#save
saveRDS(popn, "input_data/mye/2020/population_gla.rds")
saveRDS(int_in, "input_data/mye/2020/int_in_gla.rds")
saveRDS(int_out, "input_data/mye/2020/int_out_gla.rds")

rm(list=ls())
