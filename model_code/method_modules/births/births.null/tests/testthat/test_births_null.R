context("births_null")
library(births.null)
library(testthat)

pop <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), count = 100, stringsAsFactors = FALSE)

pop_out_by_area <- expand.grid( area=c("a","b","c"), births = 0, age = 0, stringsAsFactors = FALSE)
pop_out_by_area_and_sex <- expand.grid( area=c("a","b","c"), sex=c("f","m"), births = 0, age = 0, stringsAsFactors = FALSE)


#--------------------------------------------------------------

test_that("births_null creates the expected output", {
  expect_equal(births_null(pop, col_aggregation = "area"), pop_out_by_area)
  expect_equal(births_null(pop, col_aggregation = c("area","sex")), pop_out_by_area_and_sex)
})

test_that("births_null handles factors, tibbles and groups", {
  pop_in <- dplyr::mutate(pop, area=as.factor(area))
  pop_out <- dplyr::mutate(pop_out_by_area, area=as.factor(area))
  expect_equal(births_null(pop_in, col_aggregation = "area"), pop_out)

  pop_in <-  dplyr::as_tibble(pop_in)
  pop_out <- dplyr::as_tibble(pop_out)
  expect_equal(births_null(pop_in, col_aggregation = "area"), pop_out)

  pop_in <-  dplyr::group_by(pop_in, area)
  pop_out <- dplyr::group_by(pop_out, area)
  expect_equal(births_null(pop_in, col_aggregation = "area"), pop_out)
})

test_that("births_null warns when factor levels don't match the input", {
  pop_in  <- dplyr::mutate(pop, area=factor(area, levels = c("a","b","c","d")))
  pop_out <- dplyr::mutate(pop_out_by_area, area=factor(area, levels = c("a","b","c","d")))
  expect_warning( temp <- births_null(pop_in, col_aggregation = "area") )
  expect_equal(temp, pop_out)
})

test_that("births_null warns with an empty input", {
  pop_in <- pop[NULL,]
  pop_out <- pop_out_by_area[NULL,]
  expect_warning( temp <- births_null(pop_in, col_aggregation = "area") )
  expect_equal(temp, pop_out)
})

test_that("births_null throws an error with missing aggregation values", {
  pop_in <- pop
  pop_in$area[1] <- NA
  expect_error(births_null(pop_in, col_aggregation = "area"))
})


# These tests are specific to births_null
test_that("births_null can give custom birth numbers per geography", {
  pop_out <- dplyr::mutate(pop_out_by_area, births = 1000)
  expect_equal(births_null(pop, col_aggregation = "area", const = 0), pop_out_by_area)
  expect_equal(births_null(pop, col_aggregation = "area", const = 1000), pop_out)
})

test_that("births_null hates negative birth rates", {
  expect_error(births_null(pop, col_aggregation = "area", const = -1))
})

test_that("births_null warns if it is asked to aggregate over age", {
  expect_warning( temp <- births_null(pop, col_aggregation = c("area", "age")) )
  expect_equal(temp, pop_out_by_area)
})

# Other tests in more complex functions might include
# - trying out different fertility rates (zero, negative...)
# - checking the function can hand the presence (or lack of) a 'sex' column
