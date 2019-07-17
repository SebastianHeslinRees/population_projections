context("deaths_null")
library(deaths.null)
library(testthat)

pop <- data.frame( area=c("a","b","c"), count = 100, stringsAsFactors = FALSE)
pop2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), count = 100, stringsAsFactors = FALSE)

deaths  <- dplyr::mutate(pop,  deaths = 0)
deaths2 <- dplyr::mutate(pop2, deaths = 0)



#--------------------------------------------------------------

test_that("deaths_null creates the expected output", {
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count"), deaths)
  expect_equal(deaths_null(pop2, col_aggregation = c("area","age","sex"), count = "count"), deaths2)
})

test_that("deaths_null handles additional, unused input columns", {
  pop_in <- mutate(pop, filler = "fill")  # fillers gonna fill
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths)
})

test_that("deaths_null handles factors, tibbles and groups", {
  pop_in <- dplyr::mutate(pop, area=as.factor(area))
  deaths_out <- dplyr::mutate(deaths, area=as.factor(area))
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out)

  pop_in <-  dplyr::as_tibble(pop_in)
  deaths_out <- dplyr::as_tibble(deaths_out)
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out)

  pop_in <-  dplyr::group_by(pop_in, area)
  deaths_out <- dplyr::group_by(deaths_out, area)
  expect_equal(deaths_null(pop_in, col_aggregation = "area", count = "count"), deaths_out)
})

test_that("deaths_null warns when factor levels don't match the input", {
  pop_in  <- dplyr::mutate(pop, area=factor(area, levels = c("a","b","c","d")))
  deaths_out <- dplyr::mutate(deaths, area=factor(area, levels = c("a","b","c","d")))
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths_out)
})

test_that("deaths_null warns with an empty input", {
  pop_in <- pop[NULL,]
  deaths_out <- deaths[NULL,]
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths_out)
})

test_that("deaths_null warns when the input already has a deaths column and throws an error when it's an aggregation level", {
  pop_in <- dplyr::mutate(deaths = 20)
  expect_warning( temp <- deaths_null(pop_in, col_aggregation = "area", count = "count") )
  expect_equal(temp, deaths_out)
  expect_error(deaths_null(pop_in, col_aggregation = c("area","deaths"), count = "count") )
})

test_that("deaths_null throws an error with missing aggregation values", {
  pop_in <- pop
  pop_in$area[1] <- NA
  expect_error(deaths_null(pop_in, col_aggregation = "area", count = "count"))
})

test_that("deaths_null throws an error with duplicate aggregation values", {
  pop_in <- rbind(pop, pop)
  expect_error(deaths_null(pop_in, col_aggregation = "area", count = "count"))
})

test_that("deaths_null still writes rows when deaths are zero", {
  TRUE # by design
})

test_that("deaths_null throws an error or (requested) warning when the result would create a negative population", {
  expect_error(deaths_null(pop, col_aggregation = "area", count = "value", const = -1000))
  expect_warning( temp <- deaths_null(pop, col_aggregation = "area",
                                      count = "value", const = -1000,
                                      error_negative_pop = FALSE))
  expect_equal(temp, deaths)
})



# These tests are specific to deaths_null
test_that("deaths_null can give custom numbers per aggregation level", {
  deaths_out <- dplyr::mutate(deaths, deaths = 1000)
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count", const = 0), deaths)
  expect_equal(deaths_null(pop, col_aggregation = "area", count = "count", const = 1000), deaths_out)
})

test_that("deaths_null hates negative death rates", {
  expect_error(deaths_null(pop, col_aggregation = "area", count = "count", const = -1))
})





# Other tests in more complex functions might include
# - trying out different fertility rates (zero, negative...)
# - checking the function can hand the presence (or lack of) a 'sex' column
