devtools::load_all("model_code/popmodules")
library(dplyr)

#2019
international_in 	<- data.frame(country = c("E","W","S","N"),
                                int_in_2019 = c(537699,17518,39900,14191),
                                stringsAsFactors = FALSE)

international_out <- data.frame(country = c("E","W","S","N"),
                                int_out_2019 = c(338890,9706,19700,10478),
                                stringsAsFactors = FALSE)

####ONS INTERNATIONAL BACKSERIES

#2018
ons_int_in <- readRDS("input_data/mye/2018/international_in_ons.rds") %>%
  select(gss_code, year, sex, age, int_in) %>% 
  recode_gss_codes(data_cols = "int_in", recode_to_year = 2020)

ons_int_out <- readRDS("input_data/mye/2018/international_out_ons.rds")  %>%
  select(gss_code, year, sex, age, int_out) %>% 
  recode_gss_codes(data_cols = "int_out", recode_to_year = 2020)
  
int_in_2018 <- ons_int_in %>% 
  filter(year == 2018) %>%
  select(-year)

int_out_2018 <- ons_int_out %>% 
  filter(year == 2018) %>%
  select(-year)

#Scale up 2018
int_in_scaled <- int_in_2018 %>% 
  mutate(country = substr(gss_code,1,1)) %>%
  group_by(country) %>%
  mutate(proportion = int_in / sum(int_in)) %>%
  data.frame() %>% 
  left_join(international_in, by="country") %>%
  mutate(int_in = proportion * int_in_2019) %>%
  mutate(year = 2019) %>%
  select(gss_code, year, sex, age, int_in)

int_out_scaled <- int_out_2018 %>% 
  mutate(country = substr(gss_code,1,1)) %>%
  group_by(country) %>%
  mutate(proportion = int_out / sum(int_out)) %>%
  data.frame() %>% 
  left_join(international_out, by="country") %>%
  mutate(int_out = proportion * int_out_2019) %>%
  mutate(year = 2019) %>%
  select(gss_code, year, sex, age, int_out)

#bind and save
int_in <- ons_int_in %>%
  rbind(int_in_scaled) %>%
  select(year, gss_code, sex, age, int_in)

int_out <- ons_int_out %>%
  rbind(int_out_scaled) %>%
  select(year, gss_code, sex, age, int_out)

saveRDS(int_in, "input_data/mye/2019/temp_ons_international_in.rds")
saveRDS(int_out, "input_data/mye/2019/temp_ons_international_out.rds")



#GLA INTERNATIONAL BACKSERIES

#2018
gla_int_in <- readRDS("input_data/mye/2018/international_in_gla_2019-11-13.rds") %>%
  select(gss_code, year, sex, age, int_in) %>% 
  recode_gss_codes(data_cols = "int_in", recode_to_year = 2020)

gla_int_out <- readRDS("input_data/mye/2018/international_out_gla_2019-11-13.rds")  %>%
  select(gss_code, year, sex, age, int_out) %>% 
  recode_gss_codes(data_cols = "int_out", recode_to_year = 2020)

gla_in_2018 <- ons_int_in %>% 
  filter(year == 2018) %>%
  select(-year)

gla_out_2018 <- ons_int_out %>% 
  filter(year == 2018) %>%
  select(-year)

rm(int_in_scaled, int_out_scaled, int_in, int_out)

#Scale up 2018
int_in_scaled <- gla_in_2018 %>% 
  mutate(country = substr(gss_code,1,1)) %>%
  group_by(country) %>%
  mutate(proportion = int_in / sum(int_in)) %>%
  data.frame() %>% 
  left_join(international_in, by="country") %>%
  mutate(int_in = proportion * int_in_2019) %>%
  mutate(year = 2019) %>%
  select(gss_code, year, sex, age, int_in)

int_out_scaled <- gla_out_2018 %>% 
  mutate(country = substr(gss_code,1,1)) %>%
  group_by(country) %>%
  mutate(proportion = int_out / sum(int_out)) %>%
  data.frame() %>% 
  left_join(international_out, by="country") %>%
  mutate(int_out = proportion * int_out_2019) %>%
  mutate(year = 2019) %>%
  select(gss_code, year, sex, age, int_out)

#bind and save
int_in <- gla_int_in %>%
  rbind(int_in_scaled) %>%
  select(year, gss_code, sex, age, int_in)

int_out <- gla_int_out %>%
  rbind(int_out_scaled) %>%
  select(year, gss_code, sex, age, int_out)

saveRDS(int_in, "input_data/mye/2019/temp_gla_international_in.rds")
saveRDS(int_out, "input_data/mye/2019/temp_gla_international_out.rds")

rm(list=ls())