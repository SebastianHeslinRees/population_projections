library(popmodules)
library(testthat)

#Test 1 check the function constrains properly with 1 country

popn <- expand.grid(year=2000, age=20:21, gss_code=c("E01","E02","S03"), sex=c("female","male"), popn = 100, stringsAsFactors = FALSE)

constraint <- expand.grid(year=2000, age=20:21, sex=c("female","male"), popn=400, country = "E", stringsAsFactors = FALSE)

output <- expand.grid(year=2000, age=20:21, gss_code=c("E01","E02"), sex=c("female","male"), popn = 200, stringsAsFactors = FALSE) %>%
  rbind(expand.grid(year=2000, age=20:21, gss_code="S03", sex=c("female","male"), popn = 100, stringsAsFactors = FALSE))

x <- popn_constrain(popn,
                    constraint,
                    col_aggregation = c("year", "sex", "age", "country"),
                    col_popn = "popn",
                    col_constraint = "popn")
x
test_that("popn_constrain can scale up and down", {
  expect_equivalent(popn_constrain(popn,
                                   constraint,
                                   col_aggregation = c("year", "sex", "age", "country"),
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)
  
  #test default parameters
  expect_equivalent(popn_constrain(popn, constraint, col_popn="popn"),
                    output)
  
  #test scale down
  constraint_in <- dplyr::mutate(constraint, popn = popn/4)
  output_out <- dplyr::mutate(output, popn = ifelse(substr(gss_code,1,1)=="E", popn/4, popn))
  
  expect_equivalent(popn_constrain(popn, constraint_in, col_popn = "popn"),
                    output_out)
})

#Test that passing no country works
output <-  mutate(output, popn = 400/3) %>%
  arrange(sex, gss_code, age)

test_that("popn_constrain can scale up and down", {
  expect_equivalent(popn_constrain(popn,
                                   constraint,
                                   col_aggregation = c("year", "sex", "age"),
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)
})

#Test it can hangle a single number
constraint <- data.frame(year = 2000, popn = 2400)
output <- mutate(output, popn = 200)
test_that("popn_constrain can scale up and down", {
  expect_equivalent(popn_constrain(popn,
                                   constraint=constraint,
                                   col_aggregation = "year",
                                   col_popn = "popn",
                                   col_constraint = "popn"),
                    output)
})

#Force errors
test_that("popn_constrain should throw an error", {
  expect_error(popn_constrain(popn,
                              constraint=constraint,
                              col_aggregation = c("year","age"),
                              col_popn = "popn",
                              col_constraint = "popn"))
  
  expect_error(popn_constrain(popn,
                              constraint=constraint,
                              col_aggregation = "year",
                              col_popn = "trevor",
                              col_constraint = "popn"))
  
  expect_error(popn_constrain(popn,
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
    arrange(sex, gss_code, age)
  
  x <- popn_constrain(popn_cty,
                      constraint_cty,
                      col_aggregation = c("year", "sex", "age", "country"),
                      col_popn = "popn",
                      col_constraint = "popn")
  
  test_that("popn_constrain can handle multiple countries", {
    expect_equivalent(x, output_cty)
  })
  
  