context("popn_age_on")
library(popmodules)
library(testthat)

# Simple test population
popn <- expand.grid( year = 2000, gss_code=c("a","b","c"), age=20:22, sex=c("f","m"), popn = 100, stringsAsFactors = FALSE)
# Simple test population with an ordered factor for age
popn_banded <- expand.grid( year = 2000, gss_code=c("a","b","c"), age=factor(c("x","y","z"), ordered=TRUE), sex=c("f","m"), popn = 100, stringsAsFactors = FALSE)

# The same populations aged on
aged <- expand.grid( year = 2001, gss_code=c("a","b","c"), age=21:22, sex=c("f","m"), stringsAsFactors = FALSE)
aged$popn <- ifelse(aged$age == 22, 200, 100)
aged_banded <- expand.grid( year = 2001, gss_code=c("a","b","c"), age=factor(c("y","z"), ordered=TRUE), sex=c("f","m"), stringsAsFactors = FALSE)
aged_banded$popn <- ifelse(aged_banded$age == "z", 200, 100)

arrange_popn <- function(popn) dplyr::arrange(popn, year, gss_code, age, sex)
popn <- arrange_popn(popn)
aged <- arrange_popn(aged)
popn_banded <- arrange_popn(popn_banded)
aged_banded <- arrange_popn(aged_banded)

# The function being tested is:
#
#popn_age_on <- function(popn,
#                        col_aggregation=c("year","gss_code","age","sex"),
#                        col_age = "age",
#                        col_year = "year",
#                        timestep = 1,
#                        template_age_levels = NULL)
#
# These default values will be used int eh test unless otherwise specified


#--------------------------------------------------------------
# The tests here use expect_equivalent. This is expect_equal (i.e. objects must be the same) but doesn't compare object attributes
# TODO find out whether the attributes matter, and whether it matters that they don't match

test_that("popn_age_on works on a simple population", {
  expect_equivalent(popn_age_on(popn),
                    aged)
})

test_that("popn_age_on handles age when it's an ordered factor", {
  expect_equivalent(popn_age_on(popn_banded),
                    aged_banded)
  # ... and fails when it's not ordered
  popn_in <- dplyr::mutate(popn_banded, age = factor(age, levels = c("x","y","z"), ordered = FALSE))
  expect_error(popn_age_on(popn_in))
})

test_that("popn_age_on is happy to validate against template age data", {
  expect_equivalent(popn_age_on(popn, template_age_levels = 20:22),
                    aged)
  expect_equivalent(popn_age_on(popn_banded, template_age_levels = c("x","y","z")),
                    aged_banded)
  expect_equivalent(popn_age_on(popn_banded, template_age_levels = factor(c("x","y","z"))),
                    aged_banded)
})

test_that("popn_age_on handles factors, tibbles and groups", {
  # CASE: age is numeric
  # Factors
  popn_in  <- dplyr::mutate(popn, gss_code=as.factor(gss_code), sex=as.factor(sex))
  aged_out <- dplyr::mutate(aged, gss_code=as.factor(gss_code), sex=as.factor(sex))
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)

  # Tibbles
  popn_in  <- tibble::as_tibble(popn_in)
  aged_out <- tibble::as_tibble(aged_out)
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)

  # Groups
  popn_in  <- dplyr::group_by(popn_in,  year, gss_code, age, sex)
  aged_out <- dplyr::group_by(aged_out, year, gss_code, age, sex)
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)

  # CASE: age is factor
  # Factors
  popn_in  <- dplyr::mutate(popn_banded, gss_code=as.factor(gss_code), sex=as.factor(sex))
  aged_out <- dplyr::mutate(aged_banded, gss_code=as.factor(gss_code), sex=as.factor(sex))
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)
  # Tibbles
  popn_in  <- tibble::as_tibble(popn_in)
  aged_out <- tibble::as_tibble(aged_out)
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)
  # Groups
  popn_in  <- dplyr::group_by(popn_in,  year, gss_code, age, sex)
  aged_out <- dplyr::group_by(aged_out, year, gss_code, age, sex)
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)
})

test_that("popn_age_on warns when factor levels don't match the input, and throws an error when it's the age column", {
  popn_in  <- dplyr::mutate(popn, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  aged_out <- dplyr::mutate(aged, gss_code=factor(gss_code, levels = c("a","b","c","d")))
  expect_warning( temp <- popn_age_on(popn_in))
  expect_equivalent(temp, aged_out)

  popn_in  <- dplyr::mutate(popn_banded, age=factor(age, levels = c("w","x","y","z")))
  expect_error(popn_age_on(popn_in))

  template_age_in <- factor(c("x","y","z"), levels = c("w","x","y","z"))
  expect_error(popn_age_on(popn, template_age_levels = template_age_in))
})

test_that("popn_age_on throws an error with an empty input", {
  popn_in <- popn[NULL,]
  expect_error(popn_age_on(popn_in))
})

test_that("popn_age_on works with multiple data columns, including text data", {
  popn_in  <- dplyr::mutate(popn, popn2 = popn, fill="fill")
  aged_out <- dplyr::mutate(aged, popn2 = popn, fill="fill")
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)
})

test_that("popn_age_on preserves columns that aren't aggregation levels or data", {
  popn_in  <- dplyr::mutate(popn, filler="fill")
  aged_out <- dplyr::mutate(aged, filler="fill")
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)

  popn_in  <- dplyr::mutate(popn_in,  filler=as.factor(filler))
  aged_out <- dplyr::mutate(aged_out, filler=as.factor(filler))
  expect_equivalent(popn_age_on(popn_in),
                    aged_out)

  # TODO decide how it should deal with data grouped by non-aggregation columns (unlikely)
})

test_that("popn_age_on throws an error when it can't aggregate age-dependent, non-numeric properly", {
  popn_in <- dplyr::mutate(popn, fill = letters[1:nrow(popn)])
  expect_error(popn_age_on(popn_in))
})


test_that("popn_age_on handles custom column names", {
  popn_in  <- dplyr::rename(popn, xyear=year, xgss_code=gss_code, xage=age, xsex=sex, xpopn=popn)
  aged_out <- dplyr::rename(aged, xyear=year, xgss_code=gss_code, xage=age, xsex=sex, xpopn=popn)
  expect_equivalent(popn_age_on(popn_in, col_aggregation = c("xyear","xgss_code","xage","xsex"), col_age = "xage", col_year = "xyear"),
                    aged_out)
})

test_that("popn_age_on doesn't care about the order of col_aggregation", {
  expect_equivalent(popn_age_on(popn, col_aggregation = c("sex","age","gss_code","year")),
                    aged)
})

test_that("popn_age_on throws an error with explicit missing aggregation values", {
  popn_in <- popn
  popn_in$gss_code[1] <- NA
  expect_error(popn_age_on(popn_in))
})

test_that("popn_age_on throws an error with implicit missing aggregation values", {
  popn_in <- popn[-1,]
  expect_error(popn_age_on(popn_in))
})

test_that("popn_age_on throws an error when there's only one age level in the input", {
  popn_in <- expand.grid( year = 2000, gss_code=c("a","b","c"), age=20, sex=c("f","m"), popn = 100, stringsAsFactors = FALSE)
  expect_error(popn_age_on(popn_in))
})

test_that("popn_age_on throws an error with duplicate aggregation values", {
  popn_in <- dplyr::select(popn, -sex)
  expect_error(popn_age_on(popn_in, col_aggregation = c("year","gss_code","age")))
})

test_that("popn_age_on works on data without a year column", {
  popn_in  <- dplyr::select(popn, -year)
  aged_out <- dplyr::select(aged, -year)
  expect_equivalent(popn_age_on(popn_in, col_aggregation = c("gss_code","age","sex"), col_year = NULL),
                    aged_out)
})

test_that("popn_age_on doesn't modify the year when col_year is NULL", {
  aged_out <- dplyr::mutate(aged, year = 2000)
  expect_equivalent(popn_age_on(popn, col_aggregation = c("year","gss_code","age","sex"), col_year = NULL),
                    aged_out)
})

test_that("popn_age_on can handle custom timesteps when the age column is a factor)", {
  aged_out <- dplyr::mutate(aged_banded, year = 2010)
  expect_equivalent(popn_age_on(popn_banded, timestep = 10),
                    aged_out)

  aged_out <- dplyr::mutate(aged_banded, year = 2010.5)
  expect_warning(temp <- popn_age_on(popn_banded, timestep = 10.5))
  expect_equivalent(temp, aged_out)

  expect_error(popn_age_on(popn_banded, timestep = 0))
  expect_error(popn_age_on(popn, timestep = 10))
})

test_that("popn_age_on can handle custom timesteps when the age column isn't a factor)", {
  popn_in  <- dplyr::mutate(popn, age = age * 2)
  aged_out <- dplyr::mutate(aged, age = age * 2, year = 2002)
  expect_equivalent(popn_age_on(popn_in, timestep = 2),
                    aged_out)

  # Throws an error when age input doesn't match the timestep
  expect_error(popn_age_on(popn, timestep = 2))
})



