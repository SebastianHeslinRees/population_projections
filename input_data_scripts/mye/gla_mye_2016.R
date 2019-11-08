library(dplyr); library(tidyr)
load("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model - original/Inputs/2016 base/CCM Data Inputs - UPC.RData")

process_rdata_file <- function(component, col_name){
  
  x <- nrow(filter(mye_components, var==component, estimate <0))
  if(x != 0){
    message(paste("changing",x,"negative rows to 0 in",component))
  }
  
  df <- mye_components %>%
    filter(var == component) %>%
    mutate(sex = case_when(sex=="F" ~ "female",
                           sex=="M" ~ "male")) %>%
    select(year,gss_code,sex,age,estimate) %>%
    mutate(estimate = ifelse(estimate <0, 0, estimate)) %>%
    rename(!!col_name := estimate) %>%
    recode_gss_to_2011(col_aggregation = c("year","gss_code","sex","age")) %>%
    filter(year > 2001)
  
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
upc <- mutate(upc, sex = case_when(sex=="F" ~ "female",
                                   sex=="M" ~ "male")) %>%
  rename(upc = change)

#WALES
datestamp <- "2019-10-11"
popn_wales <- readRDS(paste0("input_data/mye/2018/population_ons_",datestamp,".rds")) %>% filter(year <= 2016, year > 2001, substr(gss_code,1,1)=="W") %>% select(names(popn))
deaths_wales <-  readRDS(paste0("input_data/mye/2018/deaths_ons_",datestamp,".rds")) %>% filter(year <= 2016, year > 2001, substr(gss_code,1,1)=="W") %>% select(names(deaths))
births_wales <-  readRDS(paste0("input_data/mye/2018/births_ons_",datestamp,".rds")) %>% filter(year <= 2016, year > 2001, substr(gss_code,1,1)=="W") %>% select(names(births))
int_out_wales <-  readRDS(paste0("input_data/mye/2018/international_out_ons_",datestamp,".rds")) %>% filter(year <= 2016, year > 2001, substr(gss_code,1,1)=="W") %>% select(names(int_out))
int_in_wales <-  readRDS(paste0("input_data/mye/2018/international_in_ons_",datestamp,".rds")) %>% filter(year <= 2016, year > 2001, substr(gss_code,1,1)=="W") %>% select(names(int_in))

#Other
ni_births <- filter(popn, gss_code=="N92000002", age > 0) %>%
  mutate(births = 0) %>%
  select(-popn)
sc_births <- filter(popn, gss_code=="S92000003", age > 0) %>%
  mutate(births = 0) %>%
  select(-popn)

popn <- filter(popn, substr(gss_code,1,1)!="W") %>% rbind(popn_wales)
births <- filter(births, substr(gss_code,1,1)!="W") %>% rbind(births_wales, ni_births, sc_births) %>% unique()
deaths <- filter(deaths, substr(gss_code,1,1)!="W") %>% rbind(deaths_wales)
int_in <- filter(int_in, substr(gss_code,1,1)!="W") %>% rbind(int_in_wales)
int_out <- filter(int_out, substr(gss_code,1,1)!="W") %>% rbind(int_out_wales)


popn_mye_path <- "input_data/mye/2016/population_gla_.rds"
births_mye_path <-  "input_data/mye/2016/births_gla_.rds"
deaths_mye_path <-  "input_data/mye/2016/deaths_gla_.rds"
int_in_mye_path <-  "input_data/mye/2016/international_in_gla_.rds"
int_out_mye_path <- "input_data/mye/2016/international_out_gla_.rds"
int_net_mye_path <-  "input_data/mye/2016/international_net_gla_.rds"
upc_path <- "input_data/mye/2016/upc_gla_.rds"

df <- list(popn, births, deaths,int_in,int_out,int_net,upc)
path <- c(popn_mye_path, births_mye_path, deaths_mye_path, int_in_mye_path, int_out_mye_path, int_net_mye_path,
          upc_path)


dir.create("input_data/mye/2016/", recursive = T)
for(i in seq(length(df))){
  saveRDS(df[[i]], path[[i]])
}

