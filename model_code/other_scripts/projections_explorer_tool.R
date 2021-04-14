devtools::load_all("C:/Projects_c/demogtools/")
library(dplyr)
library(data.table)
library(assertthat)  


trend_list <- list(CC = "outputs/trend/2019/published_27_nov_2020/2019_variant_CC_20-11-27_1153/",
                   CH = "outputs/trend/2019/published_27_nov_2020/2019_variant_CH_20-11-27_1154/",
                   LC = "outputs/trend/2019/published_27_nov_2020/2019_variant_LC_20-11-27_1208/",
                   LH = "outputs/trend/2019/published_27_nov_2020/2019_variant_LH_20-11-27_1313/")

housing_list <- list(s1 = "outputs/housing_led/2019/bpo/BPO_scenario_1_21-03-24_1820/",
                     s2 = "outputs/housing_led/2019/bpo/BPO_scenario_2_21-03-24_1820/",
                     s3 = "outputs/housing_led/2019/bpo/BPO_scenario_3_21-03-24_1820/")

#-------------------------------------------------------------------------------


a <- function(proj_list, file_nm, col_nm = file_nm, comp_nm=file_nm){
  x <- read_multiple_projections(proj_list,
                                 component = file_nm,
                                 col_aggregation = c("gss_code","year","sex","age")) %>% 
    mutate(component = comp_nm) %>% 
    rename(projection_name = variant,
           value = col_nm) %>%
    select(projection_name, gss_code, year, sex, age, component, value) %>% 
    filter(substr(gss_code,1,3)=="E09" | gss_code == "E12000007") %>% 
    filter(year %in% 2011:2050)
  
  if(!"E12000007" %in% unique(x$gss_code)){
    y <- filter_london(x, col_aggregation = c("projection_name", "gss_code", "year", "sex", "age", "component"),
                       data_col = "value")
    x <- rbind(x, y)
  }
  
  return(x)
}

#-------------------------------------------------------------------------------

validate_output <- function(trend, housing_led, ward){

  assert_that(length(unique(trend$gss_code))==34, msg = "problem in trend$gss_code")
  assert_that(length(unique(housing_led$gss_code))==34, msg = "problem in housing_led$gss_code")
  assert_that(length(unique(ward$gss_code))==624, msg = "problem in ward$gss_code")
  
  assert_that(length(unique(trend$year))==40, msg = "problem in trend$year")
  assert_that(length(unique(housing_led$year))==40, msg = "problem in housing_led$year")
  assert_that(length(unique(ward$year))==40, msg = "problem in ward$year")
  
  assert_that(length(unique(trend$sex))==2, msg = "problem in trend$sex")
  assert_that(length(unique(housing_led$sex))==2, msg = "problem in housing_led$sex")
  assert_that(length(unique(ward$sex))==2, msg = "problem in ward$sex")
  
  assert_that(length(unique(trend$age))==91, msg = "problem in trend$age")
  assert_that(length(unique(housing_led$age))==91, msg = "problem in housing_led$age")
  assert_that(length(unique(ward$age))==91, msg = "problem in ward$age")
  
  assert_that(length(unique(trend$component))==8, msg = "problem in trend$component")
  assert_that(length(unique(housing_led$component))==8, msg = "problem in housing_led$component")
  assert_that(length(unique(ward$component))==4, msg = "problem in ward$component")
  
  assert_that(nrow(trend)/34/40/2/((91*7)+1)==4, msg = "problem in trend dataframe")
  assert_that(nrow(housing_led)/34/40/2/((91*7)+1)==3, msg = "problem in housing-led dataframe")
  assert_that(nrow(ward)/624/40/2/((91*3)+1)==3, msg = "problem in ward dataframe")
  
}

#-------------------------------------------------------------------------------

t <- list()
h <- list()
w <- list()

#-------------------------------------------------------------------------------
tm <- Sys.time()

#TREND

t$popn <- a(trend_list, "population", "popn")
t$births <- a(trend_list, "births")
t$deaths <- a(trend_list, "deaths")
t$dom_in <- a(trend_list, "dom_in", comp_nm = "domestic in")
t$dom_out <- a(trend_list, "dom_out", comp_nm = "domestic out")
t$int_in <- a(trend_list, "int_in", comp_nm = "international in")
t$int_out <- a(trend_list, "int_out", comp_nm = "international out")
t$total_net <- rbind(t$dom_in, t$dom_out, t$int_in, t$int_out) %>% 
  mutate(value = ifelse(component %in% c("domestic_out", "international_out"),
                        value * -1, value)) %>% 
  mutate(component = "total net") %>% 
  group_by(projection_name, gss_code, year, sex, age, component) %>% 
  summarise(value = sum(value)) %>% 
  data.frame()

trend <- rbindlist(t)

#-------------------------------------------------------------------------------

#HOUSING-LED

h$popn <- a(housing_list, "population", "popn")
h$births <- a(housing_list, "births")
h$deaths <- a(housing_list, "deaths")
h$dom_in <- a(housing_list, "dom_in", comp_nm = "domestic in")
h$dom_out <- a(housing_list, "dom_out", comp_nm = "domestic out")
h$int_in <- a(housing_list, "int_in", comp_nm = "international in")
h$int_out <- a(housing_list, "int_out", comp_nm = "international out")
h$total_net <- rbind(h$dom_in, h$dom_out, h$int_in, h$int_out) %>% 
  mutate(value = ifelse(component %in% c("domestic_out", "international_out"),
                        value * -1, value)) %>% 
  mutate(component = "total net") %>% 
  group_by(projection_name, gss_code, year, sex, age, component) %>% 
  summarise(value = sum(value)) %>% 
  data.frame()

housing_led <- rbindlist(h)

#-------------------------------------------------------------------------------

#WARD

ward_list <- lapply(housing_list, function(x) paste0(x,"ward/"))

w$popn <- read_multiple_projections(ward_list,
                                    component = "population_ward",
                                    col_aggregation = c("gss_code_ward","year","sex","age")) %>% 
  rename(value = popn) %>% 
  mutate(component = "population")

w$births <- read_multiple_projections(ward_list,
                                      component = "births_ward",
                                      col_aggregation = c("gss_code_ward","year","sex","age")) %>% 
  rename(value = births) %>% 
  mutate(component = "births")

w$deaths <- read_multiple_projections(ward_list,
                                      component = "deaths_ward",
                                      col_aggregation = c("gss_code_ward","year","sex","age")) %>% 
  rename(value = deaths) %>% 
  mutate(component = "deaths")

w$total_net <- read_multiple_projections(ward_list,
                                         component = "migration_ward",
                                         col_aggregation = c("gss_code_ward","year","sex","age")) %>% 
  rename(value = migration) %>% 
  mutate(component = "total net")

w <- lapply(w, function(x) x %>% 
              filter(gss_code_ward != "E09000001",
                     year >= 2011)%>% 
              rename(gss_code = gss_code_ward,
                     projection_name = variant) %>% 
              select(projection_name, gss_code, year, sex, age, component, value))

ward <- rbindlist(w)

#-------------------------------------------------------------------------------
rm(t,w,h)
gc()

all_3 <- rbind(trend, housing_led, ward)
all_3 <- data.frame(all_3) %>% 
  mutate(value = round(value,0))

#-------------------------------------------------------------------------------

#checks
years <- length(2011:2050)
sexes <- 2
ages <- length(0:90)
boroughs <- 34
wards <- 624
components_b <- 7
components_w <- 3
births_component <- 1
ward_rows <- years*sexes*wards*((components_w*ages)+births_component)
borough_rows <-  years*sexes*boroughs*((components_b*ages)+births_component)

rows_expected <- (ward_rows*3)+(borough_rows*7)
rows_actual <- nrow(all_3)
assert_that(rows_actual==rows_expected)
validate_output(trend, housing_led, ward)

#-------------------------------------------------------------------------------

#SAVE

#saveRDS(all_3, "c:/temp/mike.rds")
print(Sys.time()-tm)
out_dir <- "Q:/Teams/D&PA/Demography/Projections/population_models/outputs/projections_explorer/"
system.time(fwrite(all_3, paste0(out_dir, "2019_based_projections.csv")))


