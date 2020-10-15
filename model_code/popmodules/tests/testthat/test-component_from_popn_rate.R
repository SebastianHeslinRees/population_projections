context("component_from_popn_rate")
library(popmodules)
library(testthat)

skip_if(TRUE, message = "component_from_popn_rate deprecated tests moved to apply_rate_to_population")

pop <- data.frame( area=c("a","b","c"), popn = 100, stringsAsFactors = FALSE)
pop2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), popn = 100, stringsAsFactors = FALSE)

mort <- data.frame( area=c("a","b","c"), rate = 0.5, stringsAsFactors = FALSE)
mort2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)

int_out_rate <- data.frame( area=c("a","b","c"), rate = 0.5, stringsAsFactors = FALSE)
int_out_rate2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), rate=0.5, stringsAsFactors = FALSE)

deaths  <- data.frame( area=c("a","b", "c"),  deaths = 50, stringsAsFactors = FALSE)
deaths2 <- expand.grid( age=20:23, area=c("a","b","c"), sex=c("f","m"), deaths = 50, stringsAsFactors = FALSE)

int_out <- data.frame(area=c("a","b", "c"),  int_out = 50, stringsAsFactors = FALSE)
int_out2 <- expand.grid(age=20:23, area=c("a","b","c"), sex=c("f","m"), int_out = 50, stringsAsFactors = FALSE)


#--------------------------------------------------------------
# The tests here use expect_equivalent. This is expect_equal (i.e. objects must be the same) but doesn't compare object attributes
# TODO find out whether the attributes matter, and whether it matters that they don't match

test_that("component_from_popn_rate creates the expected output", {
  expect_equivalent(component_from_popn_rate(pop, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)
  expect_equivalent(component_from_popn_rate(pop2, mort2, col_aggregation = c("age","area","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths2)
  expect_equivalent(component_from_popn_rate(pop, int_out_rate, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "int_out"),
                    deaths)
  expect_equivalent(component_from_popn_rate(pop2, int_out_rate2, col_aggregation = c("age","area","sex"), col_popn = "popn", col_rate = "rate", col_component = "int_out"),
                    deaths2)
})

test_that("component_from_popn_rate can work with component_rate at a coarser resolution than the population", {
  expect_equivalent(component_from_popn_rate(pop2, mort, col_aggregation = c("age", "area", "sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths2)
})

test_that("component_from_popn_rate fails when there's more than one component rate for each aggregation level", {
  expect_error(component_from_popn_rate(pop, mort2, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))
})

test_that("component_from_popn_rate doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equivalent(component_from_popn_rate(pop2, mort2, col_aggregation = c("area","age","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths2)

  expect_error(component_from_popn_rate(pop2, mort2, col_aggregation = c("area","age","sex","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
  expect_error(component_from_popn_rate(pop2, mort2, col_aggregation = c("area"="age","area","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
  expect_error(component_from_popn_rate(pop2, mort2, col_aggregation = c("area"="age","age","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
})

test_that("component_from_popn_rate handles additional, unused input columns", {
  pop_in <- dplyr::mutate(pop, fillpop = "fill")  # fillers gonna fill
  mort_in <- dplyr::mutate(mort, fillmort = "fill")
  expect_equivalent(component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)
})

test_that("component_from_popn_rate handles factors, tibbles and groups", {
  pop_in  <- dplyr::mutate(pop,  area=as.factor(area))
  mort_in <- dplyr::mutate(mort, area=as.factor(area))
  deaths_out <- dplyr::mutate(deaths, area=as.factor(area))
  expect_equivalent(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths_out)
  expect_equivalent(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)

  pop_in <-  dplyr::as_tibble(pop_in)
  mort_in <- dplyr::as_tibble(mort_in)
  deaths_out <- dplyr::as_tibble(deaths_out)
  expect_equivalent(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths_out)
  expect_equivalent(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)
  expect_equivalent(component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths_out)

  pop_in <-  dplyr::group_by(pop_in,  area)
  mort_in <- dplyr::group_by(mort_in, area)
  deaths_out <- dplyr::group_by(deaths_out, area)
  expect_equivalent(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths_out)
  expect_equivalent(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)
  expect_equivalent(component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths_out)
})

test_that("component_from_popn_rate warns when factor levels don't match the input", {
  pop_in  <-  dplyr::mutate(pop,  area=factor(area, levels = c("a","b","c","d")))
  mort_in  <- dplyr::mutate(mort, area=factor(area, levels = c("a","b","c","d")))
  deaths_out <- dplyr::mutate(deaths, area=factor(area, levels = c("a","b","c","d")))

  expect_warning( temp <- component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") )
  expect_equivalent(temp, deaths) # due to differing factor levels, the output won't have a factor in the area column

  expect_warning( temp <- component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") )
  expect_equivalent(temp, deaths)
})

test_that("component_from_popn_rate fails with an empty input", {
  pop_in <- pop[NULL,]
  mort_in <- mort[NULL,]
  expect_warning(expect_error(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") ))
  expect_warning(expect_error(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") ))
  expect_warning(component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") )
})

test_that("component_from_popn_rate handles mappings between column names in the population and component rate data frames", {
  pop_in <- dplyr::rename(pop2, xage=age, xsex=sex, xarea=area)
  expect_equivalent(component_from_popn_rate(pop_in, mort2, col_aggregation = c("xage"="age","xarea"="area","xsex"="sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths2)
})

test_that("component_from_popn_rate warns when the input already has a component column and throws an error when it's an aggregation level", {
  pop_in <- dplyr::mutate(pop, deaths = 50)
  expect_warning(temp <- component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths") )
  expect_equivalent(temp, deaths)
  expect_error(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "area") )
})

test_that("component_from_popn_rate handles important data column names duplicated between the population and mortality data", {
  pop_in     <- dplyr::rename(pop, value = popn)
  mort_in    <- dplyr::rename(mort, value = rate)
  deaths_out <- dplyr::rename(deaths, value = deaths)

  expect_warning(temp <- component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "value", col_rate = "rate", col_component = "value"))
  expect_equivalent(temp,  deaths_out)

  expect_equivalent(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "value", col_component = "value"),
                    deaths_out)

  expect_warning(temp <- component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "value", col_rate = "value", col_component = "value"))
  expect_equivalent(temp, deaths_out)

  pop_in <- dplyr::rename(pop, value = area)
  expect_error(component_from_popn_rate(pop_in, mort_in, col_aggregation = c("value"="area"), col_popn = "popn", col_rate = "value", col_component = "deaths"))
})

test_that("component_from_popn_rate handles unused columns in the inputs with column names from the other inputs", {
  pop_in <-  dplyr::mutate(pop, rate = 0.1)
  mort_in <- dplyr::mutate(mort, popn = 20)

  expect_equivalent(component_from_popn_rate(pop_in, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"),
                    deaths)

  mort_in <- dplyr::mutate(mort, xarea = area) # creates identical area, xarea columns
  expect_warning(temp <- component_from_popn_rate(pop, mort_in, col_aggregation = c("area"="xarea"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
  expect_equivalent(temp, deaths)
})

test_that("component_from_popn_rate throws an error with explicit missing aggregation values", {
  pop_in <- pop
  pop_in$area[1] <- NA

  mort_in <- mort
  mort_in$area[1] <- NA

  expect_error(component_from_popn_rate(pop_in, mort, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))
  expect_error(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))
})

test_that("component_from_popn_rate throws an error with implicit missing aggregation values", {
  mort_in <- mort[-1,]
  expect_error(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))

  pop_in <- pop2[-1,]
  expect_error(component_from_popn_rate(pop_in, mort2, col_aggregation = c("age","area","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
})


test_that("component_from_popn_rate throws an error with duplicate aggregation values", {
  expect_error(component_from_popn_rate(pop2, mort2, col_aggregation = c("area","sex"), col_popn = "popn", col_rate = "rate", col_component = "deaths"))
})

test_that("component_from_popn_rate throws an error or (requested) warning when the rates would create a negative population or component rate", {
  mort_in <- dplyr::mutate(mort, rate=2)
  expect_error(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))

  mort_in <- dplyr::mutate(mort, rate=-1)
  expect_error(component_from_popn_rate(pop, mort_in, col_aggregation = "area", col_popn = "popn", col_rate = "rate", col_component = "deaths"))

  # I don't think we *can* make a -ve pop?? The restrictions on the death rate prevent it (though the code still checks)
})


