context("births_from_popn_fert")
library(popmodules)
library(testthat)

# example inputs
popn        <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21, sex=c("female","male"), popn = 205, stringsAsFactors = FALSE)
popn_no_age <- expand.grid( year = 2000, gss_code=c("a","b"),            sex=c("female","male"), popn = 410, stringsAsFactors = FALSE)
popn_no_sex <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21,                 popn = 205, stringsAsFactors = FALSE)
popn_no_age_sex <- expand.grid( year = 2000, gss_code=c("a","b"),                        popn = 410, stringsAsFactors = FALSE)

popn_detailed <- data.frame(
  year = rep(c(2018, 2019), 5),
  gss_code = rep(c("E01", "E02"), 5),
  sex = rep(c("male", "female"), 5),
  age = rep(c(0,1,2,3,4), 2))

popn_detailed <- as.data.frame(tidyr::complete(popn_detailed, year, gss_code, sex, age))
popn_detailed <- dplyr::mutate(popn_detailed, popn = 1:40)

fert <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21, sex=c("female","male"), stringsAsFactors = FALSE)
fert$rate <- ifelse(fert$sex == "female", 0.5, 0)

fert_no_age <- expand.grid( year = 2000, gss_code=c("a","b"), sex=c("female","male"), stringsAsFactors = FALSE)
fert_no_age$rate <- ifelse(fert_no_age$sex == "female", 0.5, 0)

fert_no_sex <- expand.grid( year = 2000, gss_code=c("a","b"), age=20:21, rate = 0.5, stringsAsFactors = FALSE)
fert_no_age_sex <- expand.grid( year = 2000, gss_code=c("a","b"), rate = 0.5, stringsAsFactors = FALSE)

fert_detailed <-
  as.data.frame(dplyr::mutate(popn_detailed, rate = dplyr::case_when(
    sex == "male" ~ 0,
    age %in% c(0,1) ~ 0,
    TRUE ~ 0.1
  )))

fert_detailed <- dplyr::select(fert_detailed, -popn)

# outputs to match inputs above
births <- expand.grid( year = 2000, gss_code=c("a","b"), age=0, sex=c("female","male"), stringsAsFactors = FALSE)
births$births <- ifelse(births$sex == "female", 100, 105)
births <- dplyr::arrange(births, year, gss_code, age, sex)

births_no_sex <- expand.grid( year = 2000, gss_code=c("a","b"), age=0, births = 205, stringsAsFactors = FALSE)
births_no_sex <- dplyr::arrange(births_no_sex, year, gss_code, age)

births_detailed <- data.frame(year = c(2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019),
                              gss_code = structure(c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c("E01", "E02"), class = "factor"),
                              sex = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), .Label = c("female", "male"), class = "factor"),
                              age = c(0, 0, 0, 0, 0, 0, 0, 0),
                              births = c(0.585365853658537, 0.614634146341464, 2.04878048780488, 2.15121951219512, 3.51219512195122, 3.68780487804878, 4.97560975609756, 5.22439024390244)
)

# TESTING:

# births_from_popn_fert <- function(pop,
#                                   fertility,
#                                   col_aggregation = c("year", "gss_code", "age", "sex"),
#                                   col_age = "age",
#                                   col_sex = "sex",
#                                   col_popn = "popn",
#                                   col_rate = "rate",
#                                   col_births = "births",
#                                   birthrate_m2f = 1.05)

# we'll be using these default values a lot

#--------------------------------------------------------------
# The tests here use expect_equivalent. This is expect_equal (i.e. objects must be the same) but doesn't compare object attributes
# TODO find out whether the attributes matter, and whether it matters that they don't match

test_that("births_from_popn_fert creates the expected output", {
  expect_equivalent(births_from_popn_fert(popn, fert),
                    births)
})

test_that("births_from_popn_fert creates expected output on more complex data", {
  expect_equivalent(births_from_popn_fert(popn_detailed, fert_detailed), births_detailed)
})

test_that("births_from_popn_fert works when age and/or sex aren't supplied", {
  expect_equivalent(births_from_popn_fert(popn_no_age, fert_no_age, col_aggregation = c("year", "gss_code", "sex")),
                    births[c("year", "gss_code", "sex", "age", "births")])
  expect_equivalent(births_from_popn_fert(popn_no_sex, fert_no_sex, col_aggregation = c("year", "gss_code", "age")),
                    births)
  expect_equivalent(births_from_popn_fert(popn_no_age_sex, fert_no_age_sex, col_aggregation = c("year", "gss_code")),
                    births)
})


test_that("births_from_popn_fert can work with fertility at a coarser resolution than the population", {
  expect_equivalent(births_from_popn_fert(popn, fert_no_age),
                    births)
})


test_that("births_from_popn_fert fails when there's more than one birth rate for each aggregation level", {
  expect_error(births_from_popn_fert(popn_no_age, fert, col_aggregation = c("year", "gss_code", "age")))
})


test_that("births_from_popn_fert doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equivalent(births_from_popn_fert(popn, fert, col_aggregation = c("gss_code","age","sex", "year")),
                    births)

  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "year", "gss_code", "age", "sex")))
  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "gss_code", "gss_code"="age", "sex")))
  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "gss_code"="age", "age", "sex")))
})


test_that("births_from_popn_fert ignores additional, unused input columns", {
  popn_in <- dplyr::mutate(popn, fillpop = "fill")  # fillers gonna fill
  fert_in <- dplyr::mutate(fert, fillfert = "fill")
  expect_equivalent(births_from_popn_fert(popn_in, fert_in),
                    births)
})


test_that("births_from_popn_fert warns and corrects when it suspects positive fertility rates are being applied to males", {
  expect_warning(temp <- births_from_popn_fert(popn, fert_no_sex))
  expect_equivalent(temp, births)
})


test_that("births_from_popn_fert handles factors, tibbles and groups", {
  popn_in <- dplyr::mutate(popn, gss_code=as.factor(gss_code))
  fert_in <- dplyr::mutate(fert, gss_code=as.factor(gss_code))
  births_out <- dplyr::mutate(births, gss_code=as.factor(gss_code))
  expect_equivalent(births_from_popn_fert(popn_in, fert),
                    births_out)
  expect_equivalent(births_from_popn_fert(popn, fert_in),
                    births)
  expect_equivalent(births_from_popn_fert(popn_in, fert_in),
                    births_out)


  popn_in <- dplyr::as_tibble(popn_in)
  fert_in <- dplyr::as_tibble(fert_in)
  births_out <- dplyr::as_tibble(births_out)
  # Comparisons of double numerics don't work in tibbles, so we have to compare the births columns separately in the following tests
  # See https://github.com/tidyverse/tibble/issues/287
  expect_equivalent(births_from_popn_fert(popn_in, fert)[,-5],
                    births_out[,-5])
  expect_equal(births_from_popn_fert(popn_in, fert)[["births"]], births_out[["births"]])

  expect_equivalent(births_from_popn_fert(popn, fert_in),
                    births)

  expect_equivalent(births_from_popn_fert(popn_in, fert_in)[,-5],
                    births_out[,-5])
  expect_equal(births_from_popn_fert(popn_in, fert_in)[["births"]], births_out[["births"]])


  popn_in <- dplyr::group_by(popn_in, gss_code)
  fert_in <- dplyr::group_by(fert_in, gss_code)
  births_out <- dplyr::group_by(births_out, gss_code)
  expect_equivalent(births_from_popn_fert(popn_in, fert)[,-5],
                    births_out[,-5])

  expect_equal(births_from_popn_fert(popn_in, fert)[["births"]], births_out[["births"]])
  expect_equivalent(births_from_popn_fert(popn, fert_in),
                    births)

  expect_equivalent(births_from_popn_fert(popn_in, fert_in)[,-5],
                    births_out[,-5])
  expect_equal(births_from_popn_fert(popn_in, fert_in)[["births"]], births_out[["births"]])
})

test_that("births_from_popn_fert warns when factor levels don't match the input", {
  popn_in  <- dplyr::mutate(popn, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  fert_in  <- dplyr::mutate(fert, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  #births_out <- dplyr::mutate(births, gss_code=factor(gss_code, levels = c("a","b","c","d")))

  expect_warning( temp <- births_from_popn_fert(popn_in, fert) )
  expect_equivalent(temp, births)

  expect_warning( temp <- births_from_popn_fert(popn, fert_in))
  expect_equivalent(temp, births)
})

test_that("births_from_popn_fert throws an error with an empty input", {
  popn_in <- popn[NULL,]
  births_out <- births[NULL,]
  expect_error( births_from_popn_fert(popn_in, fert) )
})

test_that("births_from_pop_fert can calculate different ratios of male to female assigned births", {
  skip(message = "Reinstate this test once there's a proper population_apply_rate function")
  fert_in <- dplyr::mutate(fert, rate = 2) # quadruple the birth rate from 0.5
  births_out <- dplyr::mutate(births, births = births*4)
  expect_equivalent(births_from_popn_fert(popn, fert_in),
                    births_out)

  fert_in <- dplyr::mutate(fert, rate = 0)
  births_out <- dplyr::mutate(births, births = 0)
  expect_equivalent(births_from_popn_fert(popn, fert_in),
                    births_out)

  fert_in <- dplyr::mutate(fert, rate = -1)
  expect_error(births_from_popn_fert(popn, fert_in))
})

test_that("births_from_popn_fert handles mappings between column names in the population and fertility data frames", {
  popn_in <- dplyr::rename(popn, xage=age, xsex=sex, xgss_code=gss_code, xyear=year)
  expect_equivalent(births_from_popn_fert(popn_in, fert, col_age = "xage", col_sex = "xsex", col_aggregation = c("xage"="age","xgss_code"="gss_code","xsex"="sex", "xyear"="year")),
                    births)
})

test_that("births_from_popn_fert warns when the input already has a births column and throws an error when it's an aggregation level", {
  popn_in <- dplyr::mutate(popn, births = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert) )
  expect_equivalent(temp, births)

  expect_error(births_from_popn_fert(popn_in, fert, col_births = "gss_code") )
})

test_that("births_from_popn_fert warns when the input has an age or sex column which isn't used for aggregation", {
  popn_in <- dplyr::mutate(popn_no_age, age = "fill")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_age, col_aggregation = c("year", "gss_code", "sex"), col_age = "age") )
  expect_equivalent(temp, births[c("year", "gss_code", "sex", "age", "births")])

  popn_in <- dplyr::mutate(popn_no_sex, sex = "male")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_sex, col_aggregation = c("year", "gss_code", "age"), col_sex = "sex") )
  expect_equivalent(temp, births)

  popn_in <- dplyr::mutate(popn_no_age_sex, age = "fill", sex = "male")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_age_sex, col_aggregation = c("year", "gss_code"), col_age = "age", col_sex = "sex") )
  expect_equivalent(temp, births)
})

test_that("births_from_popn_fert handles important data column names duplicated between the population and fertility data", {

  #  1: Different combinations of popn/rate/births all set to "value"
  popn_in    <- dplyr::rename(popn, value = popn)
  fert_in    <- dplyr::rename(fert, value = rate)
  births_out <- dplyr::rename(births, value = births)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert, col_popn = "value", col_births = "value"))
  expect_equivalent(temp, births_out)

  expect_equivalent(births_from_popn_fert(popn, fert_in, col_rate = "value", col_births = "value"),
                    births_out)

  expect_warning(temp <- births_from_popn_fert(popn_in, fert_in, col_popn = "value", col_rate = "value", col_births = "value"))
  expect_equivalent(temp, births_out)

  #  2: Different combinations of popn/age/births all set to "value"
  popn_in    <- dplyr::rename(popn, value = age)

  expect_error(births_from_popn_fert(popn_in, fert, col_age = "value", col_births = "value", col_aggregation = c("year", "gss_code", "value"="age", "sex")))
  expect_error(births_from_popn_fert(popn_no_age, fert_no_age, col_age = "value", col_births = "value", col_aggregation = c("year", "gss_code", "sex")))
  expect_error(births_from_popn_fert(popn_in, fert, col_age = "value", col_popn = "value", col_aggregation = c("year", "gss_code", "value"="age", "sex")))
  expect_error(births_from_popn_fert(popn_in, fert_in, col_age = "value", col_rate = "value", col_aggregation = c("year", "gss_code", "value"="age", "sex")))

  #  3: Different combinations of popn/age/sex all set to "value"
  popn_in    <- dplyr::rename(popn, value = sex)

  expect_error(births_from_popn_fert(popn_in, fert, col_sex = "value", col_births = "value", col_aggregation = c("year", "gss_code", "age", "value"="sex")))
  expect_error(births_from_popn_fert(popn_no_sex, fert_no_sex, col_sex = "value", col_births = "value", col_aggregation = c("year", "gss_code", "sex")))
  expect_error(births_from_popn_fert(popn_in, fert, col_sex = "value", col_popn = "value", col_aggregation = c("year", "gss_code", "age", "value"="sex")))
  expect_error(births_from_popn_fert(popn_in, fert_in, col_sex = "value", col_rate = "value", col_aggregation = c("year", "gss_code", "age", "value"="sex")))

  #  4: But it works when age/sex columns aren't created until the end
  fert_in <- dplyr::rename(fert_no_sex, value = rate)
  births_out <- dplyr::rename(births, value = sex)
  expect_equivalent(births_from_popn_fert(popn_no_sex, fert_in, col_sex = "value", col_rate = "value", col_aggregation = c("year", "gss_code", "age")),
                    births_out)

  fert_in <- dplyr::rename(fert_no_age, value = rate)
  births_out <- dplyr::rename(births, value = age)
  expect_equivalent(births_from_popn_fert(popn_no_age, fert_in, col_age = "value", col_rate = "value", col_aggregation = c("year", "gss_code", "sex")),
                    births_out[c("year", "gss_code", "sex", "value", "births")])

  fert_in <- dplyr::rename(fert_no_age_sex, value=rate)
  births_out <- dplyr::rename(births, value1 = sex, value2 = age)
  expect_equivalent(births_from_popn_fert(popn_no_age_sex, fert_in, col_sex = "value1", col_age = "value2", col_rate = "value", col_aggregation = c("year", "gss_code")),
                    births_out)

})

test_that("births_from_popn_fert can handle unused columns in the inputs that have names from the other inputs", {
  popn_in <- dplyr::mutate(popn, rate = 0.1)
  fert_in <- dplyr::mutate(fert, popn = 20)

  expect_equivalent(births_from_popn_fert(popn_in, fert_in),
                    births)

  fert_in <- dplyr::mutate(fert, xgss_code = gss_code) # creates identical gss_code, xgss_code columns
  expect_warning(temp <- births_from_popn_fert(popn, fert_in, col_aggregation = c("gss_code"="xgss_code", "year", "age", "sex")))
  expect_equivalent(temp, births)
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


test_that("births_from_popn_fert throws an error with duplicate aggregation rows", {
  expect_error(births_from_popn_fert(popn, fert, col_aggregation = c("year", "age","sex")))
})

test_that("births_from_popn_fert can produce output without sex data", {
  expect_equivalent(births_from_popn_fert(popn_no_sex, fert_no_sex, col_sex=NULL, col_aggregation = c("year", "gss_code", "age")),
                    births_no_sex)

  popn_in <- dplyr::mutate(popn_no_sex, sex="male")
  expect_warning(temp <- births_from_popn_fert(popn_in, fert_no_sex, col_sex=NULL, col_aggregation = c("year", "gss_code", "age")))
  expect_equivalent(temp, births_no_sex)

  births_out <- dplyr::mutate(births, births=205) # If you run this naturally you're probably doing something very wrong
  expect_warning(temp <- births_from_popn_fert(popn, fert_no_sex, col_sex=NULL, col_aggregation = c("year", "gss_code", "age", "sex")))
  expect_equivalent(temp, births_out)

  births_out <- dplyr::mutate(births, births = ifelse(sex == "female", 205, 0)) # And again, *shudder*. Maybe this should just throw an error???
  expect_warning(temp <- births_from_popn_fert(popn, fert, col_sex=NULL, col_aggregation = c("year", "gss_code", "age", "sex")))
  expect_equivalent(temp, births_out)
})

