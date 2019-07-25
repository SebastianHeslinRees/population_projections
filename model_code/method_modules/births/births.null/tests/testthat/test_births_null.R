context("births_null")
library(births.null)
library(testthat)

pop <- data.frame(area = c("a","b","c"), count = 100, stringsAsFactors = FALSE)
pop2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), count = 100, stringsAsFactors = FALSE)

births <- data.frame(area = c("a","b","c"), births = 0, age = 0, stringsAsFactors = FALSE)
births2 <- expand.grid( area=c("a","b","c"), sex=c("f","m"), births = 0, age = 0, stringsAsFactors = FALSE)


#--------------------------------------------------------------

test_that("births_null creates the expected output", {
  expect_equal(births_null(pop, col_aggregation = "area"), births)
  expect_equal(births_null(pop2, col_aggregation = c("area","sex","age")), births2)
})

test_that("births_null doesn't care about the order of input aggregation columns and warns for duplicates", {
  expect_equal(births_null(pop2, col_aggregation = c("sex","age","area")), births2)

  expect_warning(temp <- births_null(pop2, col_aggregation = c("sex", "age", "area", "area")))
  expect_equal(temp, births2)
})

test_that("births_null handles factors, tibbles and groups", {
  pop_in <- dplyr::mutate(pop, area=as.factor(area))
  births_out <- dplyr::mutate(births, area=as.factor(area))
  expect_equal(births_null(pop_in, col_aggregation = "area"), births_out)

  pop_in <-  dplyr::as_tibble(pop_in)
  births_out <- dplyr::as_tibble(births_out)
  expect_equal(births_null(pop_in, col_aggregation = "area"), births_out)

  pop_in <-  dplyr::group_by(pop_in, area)
  births_out <- dplyr::group_by(births_out, area)
  expect_equal(births_null(pop_in, col_aggregation = "area"), births_out)
})

test_that("births_null warns when factor levels don't match the input", {
  skip_if(!requireNamespace("validatepop", quietly=TRUE), message = "validatepop package is not installed")
  pop_in  <- dplyr::mutate(pop, area=factor(area, levels = c("a","b","c","d")))
  births_out <- dplyr::mutate(births, area=factor(area, levels = c("a","b","c","d")))
  expect_warning( temp <- births_null(pop_in, col_aggregation = "area") )
  expect_equal(temp, births_out)
})

test_that("births_null warns with an empty input", {
  pop_in <- pop[NULL,]
  births_out <- births[NULL,]
  expect_warning( temp <- births_null(pop_in, col_aggregation = "area") )
  expect_equal(temp, births_out)
})

test_that("births_null throws an error with missing aggregation values", {
  pop_in <- pop2
  pop_in$area[1] <- NA
  expect_error(births_null(pop_in, col_aggregation = "area"))
})

test_that("births_null warns when the input already has a births column and throws an error when it's an aggregation level", {
  pop_in <- dplyr::mutate(pop, births = 20)
  expect_warning( temp <- births_null(pop_in, col_aggregation = "area") )
  expect_equal(temp, births)
  expect_error(births_null(pop_in, col_aggregation = c("area","births")) )
})


# These tests are specific to births_null
test_that("births_null can give custom birth numbers per geography", {
  births_out <- dplyr::mutate(births, births = 1000)
  expect_equal(births_null(pop, col_aggregation = "area", const = 0), births)
  expect_equal(births_null(pop, col_aggregation = "area", const = 1000), births_out)
})

test_that("births_null hates negative birth rates", {
  expect_error(births_null(pop, col_aggregation = "area", const = -1))
})


# Other tests in more complex functions might include
# - trying out different fertility rates (zero, negative...)
# - checking the function can hand the presence (or lack of) a 'sex' column
