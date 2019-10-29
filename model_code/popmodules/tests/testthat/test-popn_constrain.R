library(popmodules)
library(testthat)

popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("female","male"), popn = 100, stringsAsFactors = FALSE)

constraint <- expand.grid(year=2000, age=20:21, sex=c("female","male"), popn=400, stringsAsFactors = FALSE)

output <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("female","male"), popn = 200, stringsAsFactors = FALSE)

# -------------------------------------------------------------

# The function being tested uses default values:
# popn_constrain <- function(popn,
#                            constraint,
#                            col_aggregation = c("year", "gss_code", "sex", "age"),
#                            col_popn = "popn",
#                            pop1_is_subset = FALSE,
#                            additional_rate_levels = NA,
#                            missing_levels_popn = FALSE,
#                            missing_levels_constraint = FALSE) {

#--------------------------------------------------------------
# The tests here use expect_equivalent. This is expect_equal (i.e. objects must be the same) but doesn't compare object attributes
# TODO find out whether the attributes matter, and whether it matters that they don't match

test_that("popn_constrain can scale up and down", {
  expect_equivalent(popn_constrain(popn,
                                   constraint,
                                   col_aggregation = c("year", "gss_code", "sex", "age"),
                                   col_popn = "popn",
                                   pop1_is_subset = FALSE,
                                   missing_levels_popn = FALSE,
                                   missing_levels_constraint = FALSE),
                    output)
  # equivalently,
  expect_equivalent(popn_constrain(popn, constraint),
                    output)

  constraint_in <- dplyr::mutate(constraint, popn = popn/4)
  output_out <- dplyr::mutate(output, popn = popn/4)

  expect_equivalent(popn_constrain(popn, constraint_in),
                    output_out)
})

test_that("popn_constrain throws a warning when the mapping is one-to-one", {
  constraint_in <- expand.grid(age=20:21, gss_code=c("a","b"), sex=c("female","male"), popn = 200, stringsAsFactors = FALSE)
  expect_warning(temp <- popn_constrain(popn, constraint_in))
  expect_equivalent(temp, output)
})

test_that("popn_constrain fails when there's more than one constraint for each aggregation level", {
  expect_error(popn_constrain(popn, constraint, col_aggregation = "sex"))
})

test_that("popn_constrain handles mappings between column names in the population and constraints data frames", {
  constraint_in <- dplyr::rename(constraint, xage=age, xsex=sex, xpopn=popn)
  expect_equivalent(popn_constrain(popn,
                                   constraint_in,
                                   col_aggregation = c("gss_code", "age"="xage", "sex"="xsex", "year"),
                                   col_popn = c("popn"="xpopn")),
                    output)
})

test_that("popn_constrain doesn't care about the order of aggregation columns and throws errors if there are duplicates", {
  expect_equivalent(popn_constrain(popn, constraint, col_aggregation = c("gss_code","age","year","sex")),
                    output)

  expect_error(popn_constrain(popn, constraint, col_aggregation = c("gss_code","gss_code","age","sex","year")))
  expect_error(popn_constrain(popn, constraint, col_aggregation = c("gss_code"="age","gss_code","sex","year")))
  expect_error(popn_constrain(popn, constraint, col_aggregation = c("gss_code"="age","age","sex","year")))
})

test_that("popn_constrain handles additional, unused input columns", {
  popn_in <- dplyr::mutate(popn, fillpop = "fill")  # fillers gonna fill
  constraint_in <- dplyr::mutate(constraint, fillconstraint = "fill")
  expect_equivalent(popn_constrain(popn_in, constraint_in),
                    output)
})

test_that("popn_constrain handles factors, tibbles and groups", {
  popn_in  <- dplyr::mutate(popn,  sex=as.factor(sex))
  constraint_in <- dplyr::mutate(constraint, sex=as.factor(sex))
  output_out <- dplyr::mutate(output, sex=as.factor(sex))
  expect_equivalent(popn_constrain(popn_in, constraint),
                    output_out)
  expect_equivalent(popn_constrain(popn, constraint_in),
                    output)

  popn_in <-  dplyr::as_tibble(popn_in)
  constraint_in <- dplyr::as_tibble(constraint_in)
  output_out <- dplyr::as_tibble(output_out)
  expect_equivalent(popn_constrain(popn_in, constraint),
                    output_out)
  expect_equivalent(popn_constrain(popn, constraint_in),
                    output)
  expect_equivalent(popn_constrain(popn_in, constraint_in),
                    output_out)

  popn_in <-  dplyr::group_by(popn_in,  sex)
  constraint_in <- dplyr::group_by(constraint_in, sex)
  output_out <- dplyr::group_by(output_out, sex)
  expect_equivalent(popn_constrain(popn_in, constraint),
                    output_out)
  expect_equivalent(popn_constrain(popn, constraint_in),
                    output)
  expect_equivalent(popn_constrain(popn_in, constraint_in),
                    output_out)
})


# TODO
test_that("popn_constrain handles joining to a larger, factored population", {
  skip("TODO: make this work :O :O (Chris F)")
  constraint_in <- expand.grid(year=2000, age=20:21, gss_code=factor("a","b","c"), sex=c("female","male"), constraint=0.5, stringsAsFactors = FALSE)
  expect_error(popn_constrain(popn, constraint_in, pop1_is_subset = FALSE))
  expect_equivalent(popn_constrain(popn, constraint_in, pop1_is_subset = TRUE))
})

test_that("popn_constrain warns when factor levels don't match the input", {
  popn_in  <-  dplyr::mutate(popn,  sex=factor(sex, levels = c("female","male","X")))
  constraint_in  <- dplyr::mutate(constraint, sex=factor(sex, levels = c("female","male","X")))
  output_out <- dplyr::mutate(output, sex=factor(sex, levels = c("female","male","X")))

  expect_warning( temp <- popn_constrain(popn_in, constraint))
  expect_equivalent(temp, output) # due to differing factor levels, the output won't have a factor in the gss_code column

  expect_warning( temp <- popn_constrain(popn, constraint_in))
  expect_equivalent(temp, output)
})

test_that("popn_constrain warns with an empty input", {
  popn_in <- popn[NULL,]
  output_out <- output[NULL,]
  expect_warning( temp <- popn_constrain(popn_in, constraint, pop1_is_subset = TRUE))
  expect_equivalent(temp, output_out)
})


test_that("popn_constrain throws an error with explicit missing aggregation values", {
  popn_in <- popn
  popn_in$gss_code[1] <- NA

  expect_error(popn_constrain(popn_in, constraint, col_aggregation = "gss_code"))

  constraint_in <- constraint
  constraint_in$gss_code[1] <- NA

  expect_error(popn_constrain(popn, constraint_in, col_aggregation = "gss_code"))
})

test_that("popn_constrain throws an error with implicit missing aggregation values", {

  popn_in <- popn[-1,]
  output_out <- output[-1,]
  output_out$popn[2] <- 400  # this value is scaled higher because other values in the group are missing

  expect_error(popn_constrain(popn_in, constraint))
  expect_equivalent(popn_constrain(popn_in, constraint, pop1_is_subset = TRUE, missing_levels_popn = TRUE),
                    output_out)

  constraint_in <- constraint[-1,]
  output_out <- output
  output_out$popn[c(1,3)] <- NA # 20-year-old females missing

  expect_error(popn_constrain(popn, constraint_in, missing_levels_constraint = FALSE))
  expect_equivalent(temp <- popn_constrain(popn, constraint_in, missing_levels_constraint = TRUE),
                    output_out)
})


test_that("popn_constrain throws an error when there is more than one match in the constraint data frame for a level", {
  expect_error(popn_constrain(popn, constraint, col_aggregation = c("sex")))
})

test_that("popn_constrain can check that all constraints are matched to", {
  constraint_in  <- expand.grid(year=2000, age=20:21, sex=c("female","male", "X"), popn=400, stringsAsFactors = FALSE)
  expect_equivalent(popn_constrain(popn, constraint_in, pop1_is_subset = TRUE),
                    output)
  expect_error(popn_constrain(popn, constraint_in, pop1_is_subset = FALSE))
})

test_that("popn_constrain automatically works out the highest common resolution of datasets", {
  skip("Implement this when we need it")
  popn_in        <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), gss2 = c("c","d"), sex=c("female","male"), popn=50, stringsAsFactors = FALSE)
  constraint_in  <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), gss3 = c("e","f"), popn=200, stringsAsFactors = FALSE)
  expect_equivalent(popn_constrain(popn_in, constraint_in, col_aggregation = c("year","gss_code","gss2","sex","age")),
                    output)
})


test_that("popn_constrain can also constrain to a single scalar target population", {
  skip("TODO")
  expect_error(popn_constrain(popn, 1600))
})
