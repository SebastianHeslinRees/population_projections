ons_popn <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2019/ons_popn.rds")

births <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2019/births.rds") %>%
  filter(year == 2019) %>%
  rename(popn = births)

ons_international_in <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2019/ons_international_in.rds") %>%
  filter(year == 2019) %>%
  rename(popn = int_in)

ons_international_out <- readRDS("C:/Projects_c/population_projections_c/input_data/mye/2019/ons_international_out.rds") %>%
  filter(year == 2019) %>%
  rename(popn = int_out) %>%
  mutate(popn = popn*-1)

popn_2018 <- ons_popn %>%
  filter(year == 2018) %>%
  mutate(popn = popn*-1)

popn_2019 <- ons_popn %>%
  filter(year == 2019)

domestic_net <- rbind(births, ons_international_in, ons_international_out,
                      popn_2018, popn_2019) %>%
  group_by(gss_code, sex, year, age) %>% 
  summarise(dom_net = sum(popn)) %>%
  as.data.frame()
