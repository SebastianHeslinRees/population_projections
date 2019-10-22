library(dplyr)
library(testthat)
devtools::load_all()

popn <- expand.grid(year = c(2000:2004),
                    gss_code = c("E0901", "E0902"),
                    age = c(1,2),
                    sex = c("male", "female"),
                    stringsAsFactors = F) %>%
  data.frame()
popn$popn <- sample(c(1000:3000), size=40, replace=T)

components <- expand.grid(year = c(2001:2005),
                          gss_code = c("E0901", "E0902"),
                          age = c(2,3),
                          sex = c("male", "female"),
                          stringsAsFactors = F)%>%
  data.frame()

#----------------------------------

int_out <- components
int_out$int_out <- sample(c(100:200), size=40, replace=T)

out_rates <- int_out %>%
  left_join(popn) %>%
  mutate(int_out = int_out/popn) %>%
  select(year, gss_code, sex, age, int_out)

out_flow <- out_rates %>%
  filter(year %in% c(2003:2005)) %>%
  group_by(gss_code, sex, age) %>%
  summarise(int_out = sum(int_out)/3) %>%
  ungroup()

traj <- list()
for(i in 2006:2010){
  traj[[i]] <- mutate(out_flow, year = i)
}
traj <- data.table::rbindlist(traj) %>% select(names(out_rates))
out_rates <- rbind(out_rates, traj)

#-----------------------------------

test_that("international_rates produces the expected output", {
  expect_equivalent(international_rates(population = popn, component=int_in,
                                        last_data_year=2005, years_to_avg=3, data_col="int_out", n_proj_years=5),
                    in_flow)
})

#-----------------------------------
int_in <- components
int_in$int_in <- sample(c(100:200), size=40, replace=T)

in_flow <- filter(int_in, year %in% c(2003:2005)) %>%
  group_by(gss_code, sex, age) %>%
  summarise(int_in = sum(int_in)/3) %>%
  ungroup()

traj <- list()
for(i in 2006:2010){
  traj[[i]] <- mutate(in_flow, year = i)
}
traj <- data.table::rbindlist(traj) %>% select(names(int_in))
in_flow <- rbind(int_in, traj)

#------------------------------------

test_that("international_flows produces the expected output", {
  expect_equivalent(international_flows(component=int_in, last_data_year=2005, years_to_avg=3, flow_col="int_in", n_proj_years=5),
                    in_flow)
})

#x <- international_flows(component=int_in, last_data_year=2005, years_to_avg=3, flow_col="int_in", n_proj_years=5)
#y <- left_join(x, in_flow, by = c("year", "gss_code", "age", "sex")) %>% mutate(diff = int_in.x - int_in.y)
