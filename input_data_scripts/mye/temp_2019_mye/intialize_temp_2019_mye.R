library(dplyr)
library(popmodules)

#ONS population
source('input_data_scripts/mye/temp_2019_myeons_popn_2019.R')

#Births
source('input_data_scripts/mye/temp_2019_myebirths_2019.R')

#Deaths
source('input_data_scripts/mye/temp_2019_myedeaths_2019.R')

#International (ONS & GLA)
source('input_data_scripts/mye/temp_2019_myeinternational_2019.R')

#Domestic (takes a minute)
source('input_data_scripts/mye/temp_2019_myedomestic_2019.R')

#GLA population
source('input_data_scripts/mye/temp_2019_myegla_popn_2019.R')

#household model inputs
source('input_data_scripts/households/household_model_inputs_(2020 geog).R')

#mortality & fertility
source('input_data_scripts/mortality/asmr_2020_geography.R')
source('input_data_scripts/fertility/asfr_2020_geography.R')

#validate
validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/mye/2019/temp_births.rds"))

validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/mye/2019/temp_deaths.rds"))

validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/mye/2019/temp_gla_international_in.rds"))

validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/mye/2019/temp_gla_international_out.rds"))

validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/domestic_migration/2019/temp_domestic_migration_in.rds") %>%
                     filter_to_LAs())

validate_same_geog(readRDS("input_data/mye/2019/temp_ons_popn.rds"),
                   readRDS("input_data/domestic_migration/2019/temp_domestic_migration_out.rds") %>%
                     filter_to_LAs())
