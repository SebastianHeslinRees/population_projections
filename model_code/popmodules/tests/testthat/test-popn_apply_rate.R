context("apply_rate_to_population")
#devtools::load_all('model_code/popmodules')
library(testthat)
library(assertthat)

popn <- data.frame( gss_code=c("a","b"), popn = 100, stringsAsFactors = FALSE)
popn2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), popn = 100, stringsAsFactors = FALSE)

rate <- data.frame( gss_code=c("a","b"), rate = 0.5, stringsAsFactors = FALSE)
rate2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)

output  <- data.frame( gss_code=c("a","b"),  component = 50, stringsAsFactors = FALSE)
output2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), component = 50, stringsAsFactors = FALSE) %>% 
  data.frame()

test_that("apply_rate_to_population creates the expected output", {
  expect_equivalent(apply_rate_to_population(popn, rate, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(apply_rate_to_population(popn2, rate2),
                    output2)
})

test_that("apply_rate_to_population can work with rates at a coarser resolution than the population, but fails when many2one = FALSE", {
  expect_equivalent(apply_rate_to_population(popn2, rate, col_aggregation = "gss_code"),
                    output2)
  expect_error(apply_rate_to_population(popn2, rate, many2one = FALSE))
})

test_that("apply_rate_to_population fails when there's more than one death rate for each aggregation level", {
  expect_error(apply_rate_to_population(popn, rate2, col_aggregation = "gss_code"))
})

test_that("apply_rate_to_population handles mappings between column names in the population and rates data frames", {
  rate_in <- dplyr::rename(rate2, xage=age, xsex=sex, xgss_code=gss_code)
  expect_equivalent(apply_rate_to_population(popn2, rate_in, col_aggregation = c("age"="xage","gss_code"="xgss_code","sex"="xsex", "year")),
                    output2)
})

test_that("apply_rate_to_population doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equivalent(apply_rate_to_population(popn2, rate2, col_aggregation = c("gss_code","age","year","sex")),
                    output2)

  expect_error(apply_rate_to_population(popn2, rate2, col_aggregation = c("gss_code","gss_code","age","sex","year")))
  expect_error(apply_rate_to_population(popn2, rate2, col_aggregation = c("gss_code"="age","gss_code","sex","year")))
  expect_error(apply_rate_to_population(popn2, rate2, col_aggregation = c("gss_code"="age","age","sex","year")))
})

#TODO
test_that("apply_rate_to_population handles additional, unused input columns", {
  skip("TODO: is this functionality important?")
  #The expectation of this test is that the function drops the new columns
  #because they're not specified in some way in the function call
  popn_in <- dplyr::mutate(popn, fillpop = "fill") 
  rate_in <- dplyr::mutate(rate, fillrate = "fill")
  expect_equivalent(apply_rate_to_population(popn_in, rate_in, col_aggregation = "gss_code"),
                    output)
})


test_that("apply_rate_to_population handles factors, tibbles and groups", {

  popn_in  <- dplyr::mutate(popn,  gss_code=as.factor(gss_code))
  rate_in <- dplyr::mutate(rate, gss_code=as.factor(gss_code))
  output_out <- dplyr::mutate(output, gss_code=as.factor(gss_code))
  expect_equivalent(apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code"),
                    output)

  popn_in <-  dplyr::as_tibble(popn_in)
  rate_in <- dplyr::as_tibble(rate_in)
  output_out <- dplyr::as_tibble(output_out)
  expect_equivalent(apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(apply_rate_to_population(popn_in, rate_in, col_aggregation = "gss_code"),
                    output_out)

  popn_in <-  dplyr::group_by(popn_in,  gss_code)
  rate_in <- dplyr::group_by(rate_in, gss_code)
  output_out <- dplyr::group_by(output_out, gss_code)
  expect_equivalent(apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(apply_rate_to_population(popn_in, rate_in, col_aggregation = "gss_code"),
                    output_out)
})


# TODO
test_that("apply_rate_to_population handles joining to a larger, factored population", {
  skip("TODO: make this work :O :O (Chris F)")
  rate_in <- expand.grid(year=2000, age=20:21, gss_code=factor("a","b","c"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)
  expect_error(apply_rate_to_population(popn2, rate_in, pop1_is_subset = FALSE))
  expect_equivalent(apply_rate_to_population(popn2, rate_in, pop1_is_subset = TRUE))
})

test_that("apply_rate_to_population warns when factor levels don't match the input", {
  popn_in  <-  dplyr::mutate(popn,  gss_code=factor(gss_code, levels = c("a","b","c","d")))
  rate_in  <- dplyr::mutate(rate, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  output_out <- dplyr::mutate(output, gss_code=factor(gss_code, levels = c("a","b","c","d")))

  expect_warning( temp <- apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code"))
  expect_equivalent(temp, output) # due to differing factor levels, the output won't have a factor in the gss_code column

  expect_warning( temp <- apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code"))
  expect_equivalent(temp, output)
})

test_that("apply_rate_to_population warns with an empty input", {
  popn_in <- popn[NULL,]
  output_out <- output[NULL,]
  expect_warning( temp <- apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code", pop1_is_subset = TRUE))
  expect_equivalent(temp, output_out)
})


test_that("apply_rate_to_population warns when the input already has a col_out column and throws an error when it's an aggregation level", {
  popn_in <- dplyr::mutate(popn, component = 50)
  expect_error(temp <- apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code", col_out = "component") )
  expect_equivalent(temp, output)
  expect_error(apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code", col_out = "gss_code") )
})

test_that("apply_rate_to_population handles important data column names duplicated between the population and rates data", {
  popn_in     <- dplyr::rename(popn, component = popn)
  rate_in    <- dplyr::rename(rate, component = rate)

  expect_error(temp <- apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code", col_popn = "component", col_rate = "rate", col_out = "component"))
  expect_equivalent(temp,  output)

  expect_equivalent(apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code", col_popn = "popn", col_rate = "component", col_out = "component"),
                    output)

  expect_error(temp <- apply_rate_to_population(popn_in, rate_in, col_aggregation = "gss_code", col_popn = "component", col_rate = "component", col_out = "component"))
  expect_equivalent(temp, output)

  popn_in <- dplyr::rename(popn, component = gss_code)
  expect_error(apply_rate_to_population(popn_in, rate_in, col_aggregation = c("component"="gss_code"), col_popn = "popn", col_rate = "component", col_out = "component"))
})

#TODO
test_that("apply_rate_to_population handles unused columns in the inputs with column names from the other inputs", {
  skip("TODO: is this functionality important?")
  #See issue above at line 50
  popn_in <-  dplyr::mutate(popn, rate = 0.1)
  rate_in <- dplyr::mutate(rate, popn = 20)

  expect_equivalent(apply_rate_to_population(popn_in, rate_in, col_aggregation = "gss_code"), output)

  rate_in <- dplyr::mutate(rate, xgss_code = gss_code) # creates identical gss_code, xgss_code columns
  expect_warning(temp <- apply_rate_to_population(popn, rate_in, col_aggregation = c("gss_code"="xgss_code")))
  expect_equivalent(temp, output)
})

test_that("apply_rate_to_population throws an error with explicit missing aggregation values", {
  popn_in <- popn
  popn_in$gss_code[1] <- NA

  expect_error(apply_rate_to_population(popn_in, rate, col_aggregation = "gss_code"))

  rate_in <- rate
  rate_in$gss_code[1] <- NA

  expect_error(apply_rate_to_population(popn, rate_in, col_aggregation = "gss_code"))
})

test_that("apply_rate_to_population throws an error with implicit missing aggregation values", {

  popn_in <- popn2[-1,]
  output_out <- output2[-1,]

  expect_error(apply_rate_to_population(popn_in, rate2))
  expect_equivalent(apply_rate_to_population(popn_in, rate2, pop1_is_subset = TRUE, missing_levels_popn = TRUE),
                    output_out)

  rate_in <- rate2[-1,]
  output_out <- output2
  output_out$component[1] <- NA

  expect_error(apply_rate_to_population(popn2, rate_in, missing_levels_rate = FALSE))
  expect_equivalent(temp <- apply_rate_to_population(popn2, rate_in, missing_levels_rate = TRUE),
                    output_out)
})


test_that("apply_rate_to_population throws an error when there is more than one match in the rate data frame for a level", {
  expect_error(apply_rate_to_population(popn2, rate2, col_aggregation = c("gss_code","sex")))
})

test_that("apply_rate_to_population can deal with a join to a higher resolution (one2many) rate data and that it needn't be complete", {
  popn_in <- unique(dplyr::select(popn2, -sex))
  expect_error(apply_rate_to_population(popn2, rate2, one2many=FALSE))
  expect_equivalent(apply_rate_to_population(popn_in, rate2, col_aggregation = c("year", "gss_code", "age"), additional_rate_levels = "sex"),
                    dplyr::arrange(output2, year, gss_code, age, sex)) # Joining results in a different ordering here

  rate_in <- rate2[-1,]
  output_out <- output2[-1,]
  expect_error(apply_rate_to_population(popn_in, rate_in, col_aggregation = c("year", "gss_code", "age"), additional_rate_levels = "sex"))
  expect_equivalent(apply_rate_to_population(popn_in, rate_in, col_aggregation = c("year", "gss_code", "age"), additional_rate_levels = "sex", missing_levels_rate = TRUE),
                    dplyr::arrange(output_out, year, gss_code, age, sex)) # Joining results in a different ordering here

  # TODO test this but when missing_levels_rate is length >= 2
})

test_that("apply_rate_to_population can check that all rates are matched to", {
  rate_in  <- expand.grid(year=2000, age=20:21, gss_code=c("a","b","c"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)
  expect_equivalent(apply_rate_to_population(popn2, rate_in, pop1_is_subset = TRUE),
                    output2)
  expect_error(apply_rate_to_population(popn2, rate_in, pop1_is_subset = FALSE))
})
