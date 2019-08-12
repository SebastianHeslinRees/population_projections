context("deaths_null")
library(popmodules)
library(testthat)

pop <- data.frame( area=c("a","b","c"), count = 100, stringsAsFactors = FALSE)
pop2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), count = 100, stringsAsFactors = FALSE)

deaths  <- data.frame( area=c("a","b", "c"),  deaths = 0, stringsAsFactors = FALSE)
deaths2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), deaths = 0, stringsAsFactors = FALSE)



#--------------------------------------------------------------
# TODO find out whether data frame attributes matter, and whether it matters that the function changes them

test_that("deaths_null creates the expected output", {
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count"), deaths, check.attributes=FALSE)
  expect_equal(deaths_null(pop2, col_aggregation = c("age","area","sex"), count = "count"), deaths2, check.attributes=FALSE)
})

test_that("deaths_null doesn't care about the order of aggregation columns and warns if there are duplicates", {
  expect_equal(deaths_null(pop2, col_aggregation = c("area","age","sex"), count = "count"), deaths2, check.attributes=FALSE)

  expect_warning(temp <- deaths_null(pop2, col_aggregation = c("area","age","sex","sex"), count = "count"))
  expect_equal(temp, deaths2, check.attributes=FALSE)
})

test_that("deaths_null handles additional, unused input columns", {
  pop_in <- dplyr::mutate(pop, filler = "fill")  # fillers gonna fill
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths, check.attributes=FALSE)
})

test_that("deaths_null handles factors, tibbles and groups", {
  pop_in <- dplyr::mutate(pop, area=as.factor(area))
  deaths_out <- dplyr::mutate(deaths, area=as.factor(area))
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out, check.attributes=FALSE)

  pop_in <-  dplyr::as_tibble(pop_in)
  deaths_out <- dplyr::as_tibble(deaths_out)
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out, check.attributes=FALSE)

  pop_in <-  dplyr::group_by(pop_in, area)
  deaths_out <- dplyr::group_by(deaths_out, area)
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out, check.attributes=FALSE)
})

test_that("deaths_null warns when factor levels don't match the input", {
  pop_in  <- dplyr::mutate(pop, area=factor(area, levels = c("a","b","c","d")))
  deaths_out <- dplyr::mutate(deaths, area=factor(area, levels = c("a","b","c","d")))
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths_out, check.attributes=FALSE)
})

test_that("deaths_null warns with an empty input", {
  pop_in <- pop[NULL,]
  deaths_out <- deaths[NULL,]
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths_out, check.attributes=FALSE)
})

test_that("deaths_null deals with non-standard column names", {
  pop_in     <- dplyr::rename(pop2,    xage = age, xarea = area, xsex = sex)
  deaths_out <- dplyr::rename(deaths2, xage = age, xarea = area, xsex = sex)
  # TODO find out whether data frame attributes matter and whether it matters that they're different here
  expect_equal(deaths_null(pop_in, col_aggregation = c("xage", "xarea", "xsex"), count = "count"), deaths_out, check.attributes = FALSE)
})

test_that("deaths_null warns when the input already has a deaths column and throws an error when it's an aggregation level", {
  pop_in <- dplyr::mutate(pop, deaths = 20)
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths, check.attributes=FALSE)
  expect_error(deaths_null(pop_in, col_aggregation = c("area","deaths"), count = "count") )
})

test_that("deaths_null throws an error with missing aggregation values", {
  pop_in <- pop
  pop_in$area[1] <- NA
  expect_error(deaths_null(pop_in, col_aggregation = "area", count = "count"))
})

test_that("deaths_null throws an error with duplicate aggregation values", {
  expect_error(deaths_null(pop2, col_aggregation = c("area","sex"), count = "count"))
})


test_that("deaths_null throws an error or (requested) warning when the result would create a negative population", {
  expect_error(deaths_null(pop, col_aggregation = "area", count = "count", const = 1000))

  deaths_out <- dplyr::mutate(deaths, deaths=100)
  expect_warning( temp <- deaths_null(pop, col_aggregation = "area",
                                      count = "count", const = 1000,
                                      error_negative_pop = FALSE))
  expect_equal(temp, deaths_out, check.attributes=FALSE)
})



# These tests are specific to deaths_null
test_that("deaths_null can give custom numbers per aggregation level", {
  deaths_out <- dplyr::mutate(deaths, deaths = 100)
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count", const = 0), deaths)
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count", const = 100), deaths_out)
})

test_that("deaths_null hates negative death rates", {
  expect_error(deaths_null(pop, col_aggregation = "area", count = "count", const = -1))
})

