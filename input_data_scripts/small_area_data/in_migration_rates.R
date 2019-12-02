# IN Migration MSOA

# Should be 540,074 records in each of the 4 read in files
# Last age is 75+

#setwd("/Volumes/LONDON DATA/Migration CSVs")
setwd("Q:/Teams/D&PA/Demography/Projections/R Models/Housing Led Model/Inputs/msoa inputs")


clean <- function(x, sex){
        x <- mutate(x, gss_code_msoa = substr(GSS.Code.MSOA,1,9)) %>%
                select(-Total) %>%
                gather(age, inMig, 2:76) %>%
                mutate(age = substr(age,5,6)) %>%
                mutate(age = as.numeric(age)) %>%
                mutate(inMig = as.numeric(inMig)) %>%
                mutate(sex = sex)
        return(x)
}

F.Int <- read.csv("Female Int In.csv", stringsAsFactors = F) %>%
        clean("F")
M.Int <- read.csv("Male Int In.csv", stringsAsFactors = F) %>%
        clean("M")
F.Dom <- read.csv("Female Dom In.csv", stringsAsFactors = F) %>%
        clean("F")
M.Dom <- read.csv("Male Dom In.csv", stringsAsFactors = F) %>%
        clean("M")

#Distribute the over 75s
load("Base Inputs.Rdata")
#msoa.la.lookup <- select(msoa.la.lookup, -District)
rm(births_msoa, deaths_msoa, census_dwellings ,base_fertility_rates,
   census_household_pop, base_institutional_popn, base_survival_rates)

over.75 <- filter(base_population, age >= 75) 

over.75 <- group_by(over.75, gss_code_msoa, sex) %>%
        summarise(Total = sum(total_population)) %>%
        as.data.frame() %>%
        left_join(over.75, by=c("gss_code_msoa","sex")) %>%
        mutate(distrib = total_population / Total) %>%
        select(gss_code_msoa, sex, age, distrib)


#####

In.Mig <- rbind(F.Int, M.Int, F.Dom, M.Dom) %>%
        filter(substr(gss_code_msoa,1,1)=="E") %>%
        group_by(gss_code_msoa, age, sex) %>%
        summarise(inMig = sum(inMig)) %>%
        as.data.frame()

In.Mig75 <- filter(In.Mig, age == 75) %>%
        select(-age) %>%
        left_join(over.75, by=c("gss_code_msoa","sex")) %>%
        mutate(inMig = inMig * distrib) %>%
        select(gss_code_msoa, sex, age, inMig)

In.Mig <- filter(In.Mig, age != 75) %>%
        rbind(In.Mig75) %>%
        arrange(gss_code_msoa, sex, age, inMig)

inflow <- In.Mig

Tots <- In.Mig %>%
        group_by(gss_code_msoa) %>%
        summarise(Total = sum(inMig)) %>%
        as.data.frame()

In.Mig <- left_join(In.Mig, Tots, by=c("gss_code_msoa")) %>%
        mutate(Rate = inMig/Total)

inRates <- filter(In.Mig, age >= 18) %>%
        group_by(gss_code_msoa) %>%
        summarise(Tot.Rate = sum(Rate)) %>%
        as.data.frame() %>%
        mutate(Scaling = 1/Tot.Rate) %>%
        left_join(filter(In.Mig, age >= 18), by=c("gss_code_msoa")) %>%
        mutate(Rate = Rate * Scaling) %>%
        select(gss_code_msoa, sex, age, Rate) %>%
        rbind(select(filter(In.Mig, age < 18), gss_code_msoa, sex, age, Rate)) %>%
        filter(substr(gss_code_msoa, 1, 1) == "E") %>%
        arrange(gss_code_msoa, sex, age)

bebe <- filter(inRates, age==1) %>%
        mutate(Rate = Rate/2,
               age = 0)

inRates <- rbind(inRates, bebe) %>%
        select(gss_code_msoa, sex, age, Rate) %>%
        arrange(gss_code_msoa, sex, age)


        
inRates <- left_join(inRates, readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Lookups/msoa to borough.rds"),
                     by="gss_code_msoa") %>%
  filter(substr(gss_code_borough,1,3)=="E09") %>%
  select(gss_code_borough, gss_code_msoa, sex, age, Rate) %>%
  setnames("Rate", "in_migration_rate")


source("Q:/Teams/D&PA/Demography/Projections/R Models/Functions/remove_functions.R")
rm.all.except("inRates")

load("Base Inputs.Rdata")
sum(base_inmigration_rates$in_migration_rate)
base_inmigration_rates <- inRates
sum(base_inmigration_rates$in_migration_rate)
rm(inRates)
save.image("Base Inputs.Rdata")


