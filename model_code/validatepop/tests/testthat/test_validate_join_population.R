
context("validate_join_population")
library(testthat)
library(validatepop)

# Set up valid population data frame with default column names
pop_test1 <- data.frame( area=c("a","b","c","d"), stringsAsFactors = FALSE)
pop_test1_partial <- dplyr::filter(pop_test1, area!="a")

pop_test2 <- dplyr::mutate(pop_test1, age = 0:3)
pop_test2 <- as.data.frame( tidyr::complete(pop_test2, age, area) )
pop_test2_partial <- dplyr::filter(pop_test2, area!="a")

pop_test3 <- dplyr::mutate(pop_test2, count = 10000)

pop <- dplyr::mutate(pop_test3, count2 = "fill")



# Run tests
test_that("validate_join_population validates good joins" , {
  expect_invisible(
    validate_join_population(pop_test1, pop_test1, cols_common_aggregation="area"))

  expect_invisible(
    validate_join_population(pop_test2, pop_test1, cols_common_aggregation="area"))

  expect_invisible(
    validate_join_population(pop_test2, pop_test2, cols_common_aggregation=c("area","age")))

  expect_invisible(
    validate_join_population(pop, pop_test1, cols_common_aggregation="area"))
})

test_that("validate_join_population can spot common column names" , {
  expect_invisible(
    validate_join_population(pop, pop))

  expect_invisible(
    validate_join_population(pop_test3, pop_test2))
})

test_that("validate_join_population fails when pop1 isn't a subset", {
  expect_error(
    validate_join_population(pop_test1, pop_test1_partial, cols_common_aggregation="area"))
})

test_that("validate_join_population can require complete coverage" , {
  expect_invisible(
    validate_join_population(pop_test1_partial, pop, cols_common_aggregation="area", pop1_is_subset=TRUE))

  expect_error(
    validate_join_population(pop_test1_partial, pop, cols_common_aggregation="area", pop1_is_subset=FALSE))
})

test_that("validate_join_population can require many-to-one and one-to-many mapping" , {
  expect_invisible(
    validate_join_population(pop_test2_partial, pop_test1, cols_common_aggregation="area", many2one = TRUE))

  expect_error(
    validate_join_population(pop_test2_partial, pop_test1, cols_common_aggregation="area", many2one = FALSE))

  expect_invisible(
    validate_join_population(pop_test1_partial, pop_test2, cols_common_aggregation="area", one2many = TRUE))

  expect_error(
    validate_join_population(pop_test1_partial, pop_test2, cols_common_aggregation="area", one2many = FALSE))

  expect_invisible(
    validate_join_population(pop_test2_partial, pop_test2, cols_common_aggregation=c("area","age"), one2many = TRUE, many2one=TRUE))
})


test_that("validate_join_population handles empty data frames", {
  expect_warning(
    temp <- validate_join_population(pop_test2[NULL,], pop_test1, cols_common_aggregation="area"))
  expect_equal(pop_test2[NULL,], temp)

})

test_that("validate_join_population can map column names to each other", {
  pop_in <- dplyr::rename(pop_test2, new_age=age)
  expect_invisible(
    validate_join_population(pop_in, pop_test2, cols_common_aggregation=c("area", "new_age"="age")))
})


test_that("validate_join_population handles (potentially differing) factors and tibbles", {
  pop_in <- dplyr::mutate(pop, age=as.factor(age), area=as.factor(area))
  expect_invisible(
    validate_join_population(pop_in, pop_in, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_in, pop_test2, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_test2, pop_in, cols_common_aggregation = c("area","age")))

  pop_in <- dplyr::as_tibble(pop_in)
  expect_invisible(
    validate_join_population(pop_in, pop_in, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_in, pop_test2, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_test2, pop_in, cols_common_aggregation = c("area","age")))

  pop_in <- dplyr::group_by(pop_in, area, age)
  expect_invisible(
    validate_join_population(pop_in, pop_in, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_in, pop_test2, cols_common_aggregation = c("area","age")))
  expect_invisible(
    validate_join_population(pop_test2, pop_in, cols_common_aggregation = c("area","age")))
})


test_that("validate_join_population handles unused factor levels, with appropriate warnings", {
  pop_in <- data.frame(area = factor(c("a","b","c","d"), levels=c("a","b","c","d","e")))
  expect_invisible(
    validate_join_population(pop_in, pop_test1, cols_common_aggregation="area"))
  expect_invisible(
    validate_join_population(pop_test1, pop_in, cols_common_aggregation="area"))
})


test_that("validate_join_population throws an error when it can't find common column names", {
  expect_error(
    validate_join_population(pop_test2, pop_test2, cols_common_aggregation=c()))

  expect_error(
    validate_join_population(pop_test2, pop_test2, cols_common_aggregation=c("area","ABSENT")))
})
