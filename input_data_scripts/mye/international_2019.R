devtools::load_all("model_code/popmodules")

#2019
international_in 	<- data.frame(country = c("E","W","S","N"),
                                int_in_2019 = c(537699,17518,39900,14191),
                                stringsAsFactors = FALSE)

international_out <- data.frame(country = c("E","W","S","N"),
                                int_out_2019 = c(338890,9706,19700,10478),
                                stringsAsFactors = FALSE)
#2018
ons_int_in <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2018/international_in_ons.rds") %>%
  select(gss_code, year, sex, age, int_in) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","age","sex"))

ons_int_out <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2018/international_out_ons.rds")  %>%
  select(gss_code, year, sex, age, int_out) %>% 
  recode_gss_to_2011(col_aggregation = c("gss_code","age","sex"))
  
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
  rbind(int_in_scaled)

int_out <- ons_int_out %>%
  rbind(int_out_scaled)

saveRDS(int_in, "input_data/mye/2019/ons_international_in.rds")
saveRDS(int_out, "input_data/mye/2019/ons_international_out.rds")

