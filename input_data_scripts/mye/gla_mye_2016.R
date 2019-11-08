library(dplyr); library(tidyr)
load("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/2016 base/CCM Data Inputs - UPC.RData")

process_rdata_file <- function(component, col_name){
  
  df <- mye_components %>%
    filter(var == component) %>%
    mutate(sex = case_when(sex=="F" ~ "female",
                           sex=="M" ~ "male")) %>%
    select(year,gss_code,sex,age,estimate) %>%
    rename(!!col_name := estimate) %>%
    recode_gss_to_2011(col_aggregation = c("year","gss_code","sex","age"))
  
}

popn <- process_rdata_file("population", "popn")
births <- process_rdata_file("births", "births")
deaths <- process_rdata_file("deaths", "deaths")
int_in <- process_rdata_file("international_in", "int_in")
int_out <- process_rdata_file("international_out", "int_out")
int_net <- mye_components %>% filter(var %in% c("international_in","international_out")) %>%
  spread(var, estimate) %>%
  mutate(int_net = international_in - international_out)%>%
  mutate(sex = case_when(sex=="F" ~ "female",
                         sex=="M" ~ "male")) %>%
  select(year,gss_code,sex,age,int_net)
special <- process_rdata_file("special_change", "special_change")
upc <- mutate(upc, sex = case_when(sex=="F" ~ "female",
                                   sex=="M" ~ "male"))


popn_mye_path <- "input_data/mye/2016/population_gla_.rds"
deaths_mye_path <-  "input_data/mye/2016/deaths_gla_.rds"
births_mye_path <-  "input_data/mye/2016/births_gla_.rds"
int_in_mye_path <-  "input_data/mye/2016/international_in_gla_.rds"
int_out_mye_path <- "input_data/mye/2016/international_out_gla_.rds"
int_net_mye_path <-  "input_data/mye/2016/international_net_gla_.rds"
special_path <- "input_data/mye/2016/special_change_gla_.rds"
upc <- "input_data/mye/2016/upc_gla_.rds"

df <- list(popn,births, deaths,int_in,int_out,int_net,special,upc)
path <- c(popn_mye_path, deaths_mye_path, births_mye_path, int_in_mye_path, int_out_mye_path, int_net_mye_path,
          special_path, upc)


dir.create("input_data/mye/2016/", recursive = T)
for(i in 1:7){
  saveRDS(df[[i]], path[[i]])
}

