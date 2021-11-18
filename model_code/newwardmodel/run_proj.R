library(data.table)
library(dplyr)

rm(list=ls())
source("model_code/newwardmodel/projection_loop.R")
data_dir <- "model_code/newwardmodel/data/"

mort_rates <- readRDS(paste0(data_dir, "mortality_rates_WD20CD.rds"))
fert_rates <- readRDS(paste0(data_dir, "fertility_rates_WD20CD.rds"))
in_mig_flows <- readRDS(paste0(data_dir, "in_migration_flows_WD20CD.rds"))
out_mig_rates <- readRDS(paste0(data_dir, "out_migration_rates_WD20CD.rds"))

#-------------------------------------------------------------------------------

proj <- list()
population <- list()
components <- list()
population[[2019]] <- readRDS(paste0(data_dir, "ward_population_WD20CD.rds")) %>%
  filter(year == 2019) %>% 
  select(year, gss_code, gss_code_ward, sex, age, popn)

system.time({
for(yr in 2020:2030){
  proj[[yr]] <- projection_loop(start_pop = population[[yr-1]],
                                fert_rates,
                                mort_rates,
                                out_mig_rates,
                                in_mig_flows,
                                proj_yr=yr)
  
  population[[yr]] <- proj[[yr]]$next_yr_popn
  components[[yr]] <- proj[[yr]]$components
}
})

population <- rbindlist(population) %>% data.frame()
components <- rbindlist(components) %>% data.frame()

#-------------------------------------------------------------------------------

x_00F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 0)
x_06F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 6)
x_23F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 23)
x_90F <- filter(components, gss_code_ward == "E05000026", sex == 'female', age == 90)

neg_pop <- filter(components, popn < 0)
neg_pop_check <- filter(components, sqrt(change^2) > start_popn & change < 0)
neg_pop_other <- setdiff(neg_pop, neg_pop_check)
