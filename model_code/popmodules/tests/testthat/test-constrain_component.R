library(popmodules)
library(testthat)

#Test 1 check the function constrains properly with 1 country

popn <- expand.grid(year=2000, age=20:21, gss_code=c("E01","E02","S03"), sex=c("female","male"), popn = 100, stringsAsFactors = FALSE)

constraint <- expand.grid(year=2000, age=20:21, sex=c("female","male"), popn=400, country = "E", stringsAsFactors = FALSE)

output <- expand.grid(year=2000, age=20:21, gss_code=c("E01","E02"), sex=c("female","male"), popn = 200, stringsAsFactors = FALSE) %>%
  arrange(age, sex, gss_code) %>%
  rbind(expand.grid(year=2000, age=20:21, gss_code="S03", sex=c("female","male"), popn = 100, stringsAsFactors = FALSE))


x <- constrain_component(popn,
                    constraint,
                    col_aggregation = c("year", "sex", "age", "country"),
                    col_popn = "popn",
                    col_constraint = "popn")
x
test_that("constrain_component can scale up and down", {
  expect_equivalent(constrain_component(popn,
                                   constraint,
                                   col_aggregation = c("year", "sex", "age", "country"),
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)

  #test default parameters
  expect_equivalent(constrain_component(popn, constraint, col_popn="popn"),
                    output)

  #test scale down
  constraint_in <- dplyr::mutate(constraint, popn = popn/4)
  output_out <- dplyr::mutate(output, popn = ifelse(substr(gss_code,1,1)=="E", popn/4, popn))

  expect_equivalent(constrain_component(popn, constraint_in, col_popn = "popn"),
                    output_out)
})

#Test that passing no country works
output <-  mutate(output, popn = 400/3) %>%
  arrange(age, sex, gss_code)

test_that("constrain_component can scale up and down", {
  expect_equivalent(constrain_component(popn,
                                   constraint,
                                   col_aggregation = c("year", "sex", "age"),
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)
})

#Test it can hangle a single number
constraint <- data.frame(year = 2000, popn = 2400)
output <- mutate(output, popn = 200) %>%
  arrange(sex, gss_code, year)
test_that("constrain_component can scale up and down", {
  expect_equivalent(constrain_component(popn,
                                   constraint=constraint,
                                   col_aggregation = "year",
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)
})

#Force errors
test_that("constrain_component should throw an error", {
  expect_error(constrain_component(popn,
                              constraint=constraint,
                              col_aggregation = c("year","age"),
                              col_popn = "popn",
                              col_constraint = "popn"))

  expect_error(constrain_component(popn,
                              constraint=constraint,
                              col_aggregation = "year",
                              col_popn = "trevor",
                              col_constraint = "popn"))

  expect_error(constrain_component(popn,
                              constraint=constraint,
                              col_aggregation = "year",
                              col_popn = "popn",
                              col_constraint = "cindy"))
})

  #----------------------
  #Test 3: test the function constrains properly with multiple countries

  constraint_cty <- expand.grid(year=2000, age=20:21, sex=c("female","male"), popn=400, country = c("E","S"), stringsAsFactors = FALSE)
  popn_cty <- popn
  output_cty <- expand.grid(year=2000, age=20:21, gss_code=c("E01","E02"), sex=c("female","male"), popn = 200, stringsAsFactors = FALSE) %>%
    rbind(expand.grid(year=2000, age=20:21, gss_code="S03", sex=c("female","male"), popn = 400, stringsAsFactors = FALSE)) %>%
    arrange(age, sex, gss_code)

  x <- constrain_component(popn_cty,
                      constraint_cty,
                      col_aggregation = c("year", "sex", "age", "country"),
                      col_popn = "popn",
                      col_constraint = "popn")

  test_that("constrain_component can handle multiple countries", {
    expect_equivalent(x, output_cty)
  })

  #-----
  #borough constraints
  test_pop <- expand.grid(gss_code = c("a","b","c"), year = 2019,
                          sex = c("male","female"), age = 0:4,
                          popn = 10, stringsAsFactors = F) %>%
    as.data.frame()

  test_const <- expand.grid(gss_code = c("a","b"), year = 2019,
                            total_popn = 200,
                            stringsAsFactors = F) %>%
    as.data.frame()

  test_out <- test_pop <- expand.grid(gss_code = c("a","b","c"), year = 2019,
                                      sex = c("male","female"), age = 0:4,
                                               stringsAsFactors = F) %>%
    as.data.frame() %>% mutate(popn = ifelse(gss_code == "c", 10, 20)) %>%
    arrange(gss_code, sex, age)

  expect_equivalent(
    test_pop %>%
    filter(gss_code %in% test_const$gss_code) %>%
    constrain_component(constraint = test_const,
                        col_aggregation = c("year","gss_code"),
                        col_popn = "popn",
                        col_constraint = "total_popn") %>%
    rbind(filter(test_pop, !gss_code %in% test_const$gss_code)) %>%
      arrange(gss_code, sex, age),
    test_out)

