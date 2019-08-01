context("births_from_popn_fert")
library(births)
library(testthat)

popn        <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21, sex=c("f","m"), count = 410, stringsAsFactors = FALSE)
popn_no_age <- expand.grid( year = 2000, gss_code=c("a","b"),            sex=c("f","m"), count = 820, stringsAsFactors = FALSE)
popn_no_sex <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21,                 count = 410, stringsAsFactors = FALSE)
popn_no_age_sex <- expand.grid( year = 2000, gss_code=c("a","b"),                        count = 820, stringsAsFactors = FALSE)

fert <- expand.grid( age=20:21, gss_code=c("a","b"), sex=c("f","m"), year = 2000, stringsAsFactors = FALSE)
fert$rate <- ifelse(fert$sex == "f", 0.5, 0)
fert_no_age <- unique( dplyr::select(fert, -age))
fert_no_sex <- unique( dplyr::select(fert, -sex))
fert_no_age_sex <- unique( dplyr::select(fert, -age, -sex))

births <- expand.grid( age=0, gss_code=c("a","b"), sex=c("f","m"), year = 2000, stringsAsFactors = FALSE)
births$births <- ifelse(births$sex == "f", 100, 105)

# TESTING:

# births_from_popn_fert <- function(pop,
#                                   fertility,
#                                   col_aggregation = c("year", "gss_code", "age", "sex"),
#                                   col_age = "age",
#                                   col_sex = "sex",
#                                   col_count = "count",
#                                   col_rate = "rate",
#                                   col_births = "births",
#                                   birthrate_m2f = 1.05)

# we'll be using these default values a lot

#--------------------------------------------------------------
# TODO find out whether data frame attributes matter, and whether it matters that the function changes them

test_that("births_from_popn_fert creates the expected output", {
  expect_equal(births_from_popn_fert(popn, fert),
               births,
               check.attributes=FALSE)
})

test_that("births_from_popn_fert works when age and/or sex aren't supplied", {
  expect_equal(births_from_popn_fert(popn_no_age, fert_no_age, col_aggregation = c("year", "gss_code", "sex")),
               births,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn_no_sex, fert_no_sex, col_aggregation = c("year", "gss_code", "age")),
               births,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn_no_age_sex, fert_no_age_sex, col_aggregation = c("year", "gss_code")),
               births,
               check.attributes=FALSE)
})


test_that("births_from_popn_fert can work with fertility at a coarser resolution than the population", {
  expect_equal(births_from_popn_fert(popn, fert_no_age, col_aggregation = c("year", "gss_code", "sex")),
               births,
               check.attributes=FALSE)
})


test_that("births_from_popn_fert fails when there's more than one birth rate for each aggregation level", {
  expect_error(births_from_popn_fert(popn_no_age, fert, col_aggregation = c("year", "gss_code", "age")))
})


test_that("births_from_popn_fert doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equal(births_from_popn_fert(popn, fert, col_aggregation = c("gss_code","age","sex", "year")),
               births,
               check.attributes=FALSE)

  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "year", "gss_code", "age", "sex")))
  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "gss_code"="gss_code", "gss_code"="age", "sex")))
})


test_that("births_from_popn_fert handles additional, unused input columns", {
  pop_in <- dplyr::mutate(pop, fillpop = "fill")  # fillers gonna fill
  fert_in <- dplyr::mutate(fert, fillfert = "fill")
  expect_equal(births_from_popn_fert(pop_in, fert_in),
               births,
               check.attributes=FALSE)
})


test_that("births_from_popn_fert warns and corrects when it suspects positive fertility rates are being applied to males", {

  expect_warning(temp <- births_from_popn_fert(popn, fert_no_sex))
  expect_equal(temp,
               births,
               check.attributes=FALSE)

})


test_that("births_from_popn_fert handles factors, tibbles and groups", {
  popn_in <- dplyr::mutate(popn, gss_code=as.factor(gss_code))
  fert_in <- dplyr::mutate(fert, gss_code=as.factor(gss_code))
  births_out <- dplyr::mutate(births, gss_code=as.factor(gss_code))
  expect_equal(births_from_popn_fert(popn_in, fert),
               births_out,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn, fert_in),
               births,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn_in, fert_in),
               births_out,
               check.attributes=FALSE)


  popn_in <- dplyr::as_tibble(popn_in)
  fert_in <- dplyr::as_tibble(fert_in)
  births_out <- dplyr::as_tibble(births_out)
  expect_equal(births_from_popn_fert(popn_in, fert),
               births_out,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn, fert_in),
               births,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn_in, fert_in),
               births_out,
               check.attributes=FALSE)

  popn_in <- dplyr::group_by(popn_in, gss_code)
  fert_in <- dplyr::group_by(fert_in, gss_code)
  births_out <- dplyr::group_by(births_out, gss_code)
  expect_equal(births_from_popn_fert(popn_in, fert),
               births_out,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn, fert_in),
               births,
               check.attributes=FALSE)
  expect_equal(births_from_popn_fert(popn_in, fert_in),
               births_out,
               check.attributes=FALSE)
})

test_that("births_from_popn_fert warns when factor levels don't match the input", {
  popn_in  <- dplyr::mutate(popn, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  fert_in  <- dplyr::mutate(fert, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  births_out <- dplyr::mutate(births, gss_code=factor(gss_code, levels = c("a","b","c","d")))

  expect_warning( temp <- births_from_popn_fert(popn_in, fert) )
  expect_equal(temp,
               births,
               check.attributes=FALSE) # due to differing factor levels, the output won't have a factor in the gss_code column

  expect_warning( temp <- births_from_popn_fert(popn, fert_in))
  expect_equal(temp,
               births,
               check.attributes=FALSE)
})

test_that("births_from_popn_fert warns with an empty input", {
  popn_in <- popn[NULL,]
  births_out <- births[NULL,]
  expect_warning( temp <- births_from_popn_fert(popn_in, fert) )
  expect_equal(temp,
               births_out,
               check.attributes=FALSE)
})

test_that("births_from_pop_fert can calculate different ratios of male to female assigned births", {
  fert_in <- dplyr::mutate(fert, rate = 2) # quadruple the birth rate from 0.5
  births_out <- dplyr::mutate(births, births = births*4)
  expect_equal(births_from_popn_fert(popn, fert_in),
               births_out,
               check.attributes=FALSE)

  fert_in <- dplyr::mutate(fert, rate = 0)
  births_out <- dplyr::mutate(births, births = 0)
  expect_equal(births_from_popn_fert(popn, fert_in),
               births_out,
               check.attributes=FALSE)

  fert_in <- dplyr::mutate(fert, rate = -1)
  expect_error(births_from_popn_fert(popn, fert_in))
})

test_that("births_from_popn_fert handles mappings between column names in the population and fertility data frames", {
  popn_in <- dplyr::rename(popn, xage=age, xsex=sex, xgss_code=gss_code, xyear=year)
  expect_equal(births_from_popn_fert(pop_in, fert, col_age = "xage", col_aggregation = c("xage"="age","xgss_code"="gss_code","xsex"="sex", "xyear"="year")),
               births,
               check.attributes=FALSE)
})

test_that("births_from_popn_fert warns when the input already has a births column and throws an error when it's an aggregation level", {
  popn_in <- dplyr::mutate(popn, births = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert) )
  expect_equal(temp,
               births,
               check.attributes=FALSE)

  expect_error(births_from_popn_fert(pop_in, fert, col_births = "gss_code") )
})

test_that("births_from_popn_fert warns when the input has an age or sex column which isn't used for aggregation", {
  popn_in <- dplyr::mutate(popn_no_age, age = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_age, col_aggregation = c("year", "gss_code", "sex"), col_age = "age") )
  expect_equal(temp,
               births,
               check.attributes=FALSE)

  popn_in <- dplyr::mutate(popn_no_sex, sex = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_sex, col_aggregation = c("year", "gss_code", "age"), col_sex = "sex") )
  expect_equal(temp,
               births,
               check.attributes=FALSE)

  popn_in <- dplyr::mutate(popn_no_age_sex, age = "fill", sex = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_age_sex, col_aggregation = c("year", "gss_code"), col_age = "age", col_sex = "sex") )
  expect_equal(temp,
               births,
               check.attributes=FALSE)
})

test_that("births_from_popn_fert handles important data column names duplicated between the population and fertility data", {
  popn_in    <- dplyr::rename(popn, value = count)
  fert_in    <- dplyr::rename(fert, value = rate)
  births_out <- dplyr::rename(births, value = births)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert, col_count = "value", col_births = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)

  expect_equal(births_from_popn_fert(popn, fert_in, col_rate = "value", col_births = "value"),
               births_out,
               check.attributes = FALSE)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert_in, col_count = "value", col_rate = "value", col_births = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)


  births_out <- dplyr::rename(births, value = age)

  expect_error(births_from_popn_fert(popn_in, fert, col_age = "value", col_births = "value"))
  expect_error(births_from_popn_fert(popn_no_age, fert_no_age, col_age = "gss_code"))
  expect_error(births_from_popn_fert(popn_no_age, fert_no_age, col_age = "value", col_births = "value"))

  expect_warning(temp <- births_from_popn_fert(popn_in, fert, col_age = "value", col_count = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)

  expect_equal(births_from_popn_fert(popn, fert_in, col_age = "value", col_rate = "value"),
               births_out,
               check.attributes = FALSE)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert_in, col_age = "value", col_count = "value", col_rate = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)


  births_out <- dplyr::rename(births, value = sex)

  expect_error(births_from_popn_fert(popn_in, fert, col_sex = "value", col_births = "value"))
  expect_error(births_from_popn_fert(popn_no_sex, fert_no_sex, col_sex = "gss_code"))
  expect_error(births_from_popn_fert(popn_no_sex, fert_no_sex, col_sex = "value", col_births = "value"))

  expect_warning(temp <- births_from_popn_fert(popn_in, fert, col_sex = "value", col_count = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)

  expect_equal(births_from_popn_fert(popn, fert_in, col_sex = "value", col_rate = "value"),
               births_out,
               check.attributes = FALSE)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert_in,  col_sex = "value", col_count = "value", col_rate = "value"))
  expect_equal(temp,
               births_out,
               check.attributes = FALSE)
})

test_that("births_from_popn_fert can handle unused columns in the inputs that have names from the other inputs", {
  popn_in <- dplyr::mutate(popn, rate = 0.1)
  fert_in <- dplyr::mutate(fert, count = 20)

  expect_equal(births_from_popn_fert(popn_in, fert_in),
               births,
               check.attributes = FALSE)

  fert_in <- dplyr::mutate(fert, xgss_code = gss_code) # creates identical gss_code, xgss_code columns
  expect_equal(births_from_popn_fert(popn, fert_in, col_aggregation = c("gss_code"="xgss_code")),
               births,
               check.attributes = FALSE)
})

test_that("births_from_popn_fert throws an error when it finds explicit missing aggregation values", {
  popn_in <- popn
  popn_in$gss_code[1] <- NA

  fert_in <- fert
  fert_in$gss_code[1] <- NA

  expect_error(births_from_popn_fert(popn_in, fert))
  expect_error(births_from_popn_fert(popn, fert_in))
})

test_that("births_from_popn_fert throws an error when it finds implicit missing aggregation values", {
  popn_in <- popn[-1,]
  fert_in <- fert[-1,]

  expect_error(births_from_popn_fert(popn_in, fert))
  expect_error(births_from_popn_fert(popn, fert_in))
})


test_that("births_from_popn_fert throws an error with duplicate aggregation values", {
  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("gss_code","sex")))
})

test_that("births_from_popn_fert can produce output without sex data", {
  # TODO tests for when col_sex = NA
})
