context("popn_apply_rate")
library(popmodules)
library(testthat)

popn <- data.frame( gss_code=c("a","b"), count = 100, stringsAsFactors = FALSE)
popn2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), count = 100, stringsAsFactors = FALSE)

rate <- data.frame( gss_code=c("a","b"), rate = 0.5, stringsAsFactors = FALSE)
rate2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)

output  <- data.frame( gss_code=c("a","b"),  value = 50, stringsAsFactors = FALSE)
output2 <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), value = 50, stringsAsFactors = FALSE)

# -------------------------------------------------------------

# The function being tested uses default values:
# popn_apply_rate <- function(popn,
#                            popn_rate,
#                            col_aggregation = c("year", "gss_code", "sex", "age"),
#                            col_count = "count",
#                            col_rate = "rate",
#                            col_out = "value") {

#--------------------------------------------------------------
# The tests here use expect_equivalent. This is expect_equal (i.e. objects must be the same) but doesn't compare object attributes
# TODO find out whether the attributes matter, and whether it matters that they don't match

test_that("popn_apply_rate creates the expected output", {
  expect_equivalent(popn_apply_rate(popn, rate, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(popn_apply_rate(popn2, rate2),
                    output2)
})

test_that("popn_apply_rate can work with rates at a coarser resolution than the population", {
  expect_equivalent(popn_apply_rate(popn2, rate),
                    output2)
})

test_that("popn_apply_rate fails when there's more than one death rate for each aggregation level", {
  expect_error(popn_apply_rate(popn, rate2, col_aggregation = "gss_code"))
})

test_that("popn_apply_rate handles mappings between column names in the population and rates data frames", {
  rate_in <- dplyr::rename(rate2, xage=age, xsex=sex, xgss_code=gss_code)
  expect_equivalent(popn_apply_rate(popn2, rate_in, col_aggregation = c("age"="xage","gss_code"="xgss_code","sex"="xsex", "year")),
                    output2)
})

test_that("popn_apply_rate doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equivalent(popn_apply_rate(popn2, rate2, col_aggregation = c("gss_code","age","year","sex")),
                    output2)

  expect_error(popn_apply_rate(popn2, rate2, col_aggregation = c("gss_code","gss_code","age","sex","year")))
  expect_error(popn_apply_rate(popn2, rate2, col_aggregation = c("gss_code"="age","gss_code","sex","year")))
  expect_error(popn_apply_rate(popn2, rate2, col_aggregation = c("gss_code"="age","age","sex","year")))
})

test_that("popn_apply_rate handles additional, unused input columns", {
  popn_in <- dplyr::mutate(popn, fillpop = "fill")  # fillers gonna fill
  rate_in <- dplyr::mutate(rate, fillrate = "fill")
  expect_equivalent(popn_apply_rate(popn_in, rate_in, col_aggregation = "gss_code"),
                    output)
})

test_that("popn_apply_rate handles factors, tibbles and groups", {
  popn_in  <- dplyr::mutate(popn,  gss_code=as.factor(gss_code))
  rate_in <- dplyr::mutate(rate, gss_code=as.factor(gss_code))
  output_out <- dplyr::mutate(output, gss_code=as.factor(gss_code))
  expect_equivalent(popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"),
                    output)

  popn_in <-  dplyr::as_tibble(popn_in)
  rate_in <- dplyr::as_tibble(rate_in)
  output_out <- dplyr::as_tibble(output_out)
  expect_equivalent(popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(popn_apply_rate(popn_in, rate_in, col_aggregation = "gss_code"),
                    output_out)

  popn_in <-  dplyr::group_by(popn_in,  gss_code)
  rate_in <- dplyr::group_by(rate_in, gss_code)
  output_out <- dplyr::group_by(output_out, gss_code)
  expect_equivalent(popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"),
                    output_out)
  expect_equivalent(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"),
                    output)
  expect_equivalent(popn_apply_rate(popn_in, rate_in, col_aggregation = "gss_code"),
                    output_out)
})

test_that("popn_apply_rate warns when factor levels don't match the input", {
  popn_in  <-  dplyr::mutate(popn,  gss_code=factor(gss_code, levels = c("a","b","c","d")))
  rate_in  <- dplyr::mutate(rate, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  output_out <- dplyr::mutate(output, gss_code=factor(gss_code, levels = c("a","b","c","d")))

  expect_warning( temp <- popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"))
  expect_equivalent(temp, output) # due to differing factor levels, the output won't have a factor in the gss_code column

  expect_warning( temp <- popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"))
  expect_equivalent(temp, output)
})

test_that("popn_apply_rate warns with an empty input", {
  popn_in <- popn[NULL,]
  output_out <- output[NULL,]
  expect_warning( temp <- popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"))
  expect_equivalent(temp, output_out)
})


test_that("popn_apply_rate warns when the input already has a col_out column and throws an error when it's an aggregation level", {
  popn_in <- dplyr::mutate(popn, value = 50)
  expect_warning(temp <- popn_apply_rate(popn_in, rate, col_aggregation = "gss_code", col_out = "value") )
  expect_equivalent(temp, output)
  expect_error(popn_apply_rate(popn_in, rate, col_aggregation = "gss_code", col_out = "gss_code") )
})

test_that("popn_apply_rate handles important data column names duplicated between the population and rates data", {
  popn_in     <- dplyr::rename(popn, value = count)
  rate_in    <- dplyr::rename(rate, value = rate)

  expect_warning(temp <- popn_apply_rate(popn_in, rate, col_aggregation = "gss_code", col_count = "value", col_rate = "rate", col_out = "value"))
  expect_equivalent(temp,  output)

  expect_equivalent(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code", col_count = "count", col_rate = "value", col_out = "value"),
                    output)

  expect_warning(temp <- popn_apply_rate(popn_in, rate_in, col_aggregation = "gss_code", col_count = "value", col_rate = "value", col_out = "value"))
  expect_equivalent(temp, output)

  popn_in <- dplyr::rename(popn, value = gss_code)
  expect_error(popn_apply_rate(popn_in, rate_in, col_aggregation = c("value"="gss_code"), col_count = "count", col_rate = "value", col_out = "value"))
})

test_that("popn_apply_rate handles unused columns in the inputs with column names from the other inputs", {
  popn_in <-  dplyr::mutate(popn, rate = 0.1)
  rate_in <- dplyr::mutate(rate, count = 20)

  expect_equivalent(popn_apply_rate(popn_in, rate_in, col_aggregation = "gss_code"), output)

  rate_in <- dplyr::mutate(rate, xgss_code = gss_code) # creates identical gss_code, xgss_code columns
  expect_warning(temp <- popn_apply_rate(popn, rate_in, col_aggregation = c("gss_code"="xgss_code")))
  expect_equivalent(temp, output)
})

test_that("popn_apply_rate throws an error with explicit missing aggregation values", {
  popn_in <- popn
  popn_in$gss_code[1] <- NA

  rate_in <- rate
  rate_in$gss_code[1] <- NA

  expect_error(popn_apply_rate(popn_in, rate, col_aggregation = "gss_code"))
  expect_error(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"))
})

test_that("popn_apply_rate throws an error with implicit missing aggregation values", {
  rate_in <- rate[-1,]
  expect_error(popn_apply_rate(popn, rate_in, col_aggregation = "gss_code"))

  popn_in <- popn2[-1,]
  expect_error(popn_apply_rate(popn_in, rate2))
})


test_that("popn_apply_rate throws an error with duplicate aggregation values", {
  expect_error(popn_apply_rate(popn2, rate2, col_aggregation = c("gss_code","sex")))
})

