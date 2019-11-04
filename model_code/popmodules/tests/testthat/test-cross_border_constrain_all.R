in_constraint <- expand.grid(year=2000, age=c(20:21), sex=c("male","female"),
                             country = c("E","S","W","N"), stringsAsFactors = FALSE) %>%
  mutate(cross_in = ifelse(country == "E", 6000, 4000))

domestic_flow <- expand.grid(year=2000, age=c(20:21), sex=c("male","female"),
                             gss_out = c("E01", "E02", "S01", "W01","N01"),
                             gss_in = c("E01", "E02", "S01", "W01", "N01"),
                             flow = 100,
                             stringsAsFactors = FALSE) %>%
  filter(gss_out!=gss_in)

output <- domestic_flow %>%
  mutate(flow = ifelse(substr(gss_out,1,1)==substr(gss_in,1,1), flow, 1000)) %>%
  arrange(gss_out, gss_in, sex, age)

x <- cross_border_constrain_all(domestic_flow, in_constraint, col_flow = "flow")

test_that("cross_border_constrain can scale up and down",{
  expect_equivalent(x, output)
})
