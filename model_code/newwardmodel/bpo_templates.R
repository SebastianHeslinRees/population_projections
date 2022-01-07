library(tidyverse)
library(popmodules)
library(data.table)

new_wards <- read_csv("C:/Projects_c/regrosser/data/lookups/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2020)_to_LAD_(2020)_Lookup_in_England_and_Wales_V2.csv")

new_wards <- new_wards %>% 
  select(LAD20CD, LAD20NM, WD20CD, WD20NM) %>% 
  filter(substr(LAD20CD,1,3)=="E09") %>% 
  distinct()

for(borough in unique(new_wards$LAD20NM)){

  a <- filter(new_wards, LAD20NM == borough) %>% 
    arrange(WD20CD) %>% 
    select(gss_code = WD20CD, ward_name = WD20NM) %>% 
    mutate(year = 2020,
           value = NA) %>% 
    project_forward_flat(2041) %>% 
    pivot_wider(names_from = year, values_from = value)
  
  fwrite(a, paste0('input_data/new_ward_model/templates/',borough,'.csv'))
  
  
  a <- fread( paste0('input_data/new_ward_model/templates/',borough,'.csv'), header = FALSE) %>% 
    data.frame()
  
  b <- a[1,] %>% mutate_all(~replace(., !is.na(.), NA)) 
  b[1,1] <- "Self-contained"
  
  c <- b
  c[1,1] <- "Non Self-contained"
  
  d <- b
  e <- b
  f <- b
  g <- b
  h <- b
  i <- b
  j <- b
  
  for(z in 4:10){assign(letters[z], value = b)}
  
  d[1,1] <- "Notes"
  e[1,1] <- "Please do not edit or change the template. Please do not add columns or rows."
  f[1,1] <- "Years indicate the year to mid-year (June). If mid-year aren't available use data to financial year endnig March in the reference year."
  g[1,1] <- "Blank cells will be filled by data from a SHLAA-based trajectory. To indicate no development use a zero."
  h[1,1] <- "Development up to and including 2019 is taken from the London Development Database."
  i[1,1] <- "Non Self-contained units should be converted to equivalent self-contained units. London Plan conversion rates are 2.5:1 for student housing, 1:1 for housing for older people (C2), and 1.8:1 for all other non self-contained housing." 
  j[1,1] <- "Return to wil.tonkiss@london.gov.uk"

  x <- rbind(b,a) %>% 
    add_row() %>% 
    rbind(c,a) %>% 
    add_row() %>% 
    rbind(d, e, f, g, h, i, j)

  fwrite(x, paste0('input_data/new_ward_model/templates/', borough, '.csv'), col.names = FALSE)
  
  rm(a, x)
  
}

