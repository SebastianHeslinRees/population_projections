#Simple example
a_1 <- data.frame(gss_code="a",age_group="0_4",sex="male",popn=25)
b_1 <- data.frame(gss_code="a", age = 0:4, sex = "male", popn = 5)

output_1 <- distribute_within_age_band(popn_1 = a_1, popn_2 = b_1,
                                       popn_1_col = "popn", popn_2_col="popn",
                                       min_age = 0, max_age = 4,
                                       col_aggregation=c("gss_code","sex"))

expect_1 <- data.frame(gss_code="a", age_group= "0_4",sex="male",popn=5, age = 0:4)

expect_equivalent(output_1, expect_1,
                  msg="distribute_within_age_band failed simple test")

#Comlicated example
a_2 <- expand.grid(gss_code_ward=c("a","b","c"),
                   age_group="0_4",
                   sex=c("male","female"),
                   popn=100,
                   stringsAsFactors = FALSE) %>%
  mutate(gss_code = ifelse(gss_code_ward == "a", "X", "Y"))

b_2 <- expand_grid(gss_code=c("X","Y"),
                   age = 0:4,
                   sex = c("male", "female")) %>%
  mutate(popn = sample(1:100, 20))


expect_2 <- b_2 %>% group_by(gss_code, sex) %>% mutate(total = sum(popn),
                                                       dist = popn / total) %>%
  as.data.frame() %>%
  select(-popn) %>%
  left_join(a_2, by = c("gss_code", "sex")) %>%
  mutate(popn = popn*dist) %>%
  select(names(a_2), age) %>%
  arrange(gss_code_ward, sex, age)
  
  
output_2 <- distribute_within_age_band(popn_1 = a_2, popn_2 = b_2,
                                       popn_1_col = "popn", popn_2_col="popn",
                                       min_age = 0, max_age = 4,
                                       col_aggregation=c("gss_code","sex"))%>%
  arrange(gss_code_ward, sex, age)

expect_equivalent(output_2, expect_2,
                  msg = "distribute_within_age_band failed complicated test")
