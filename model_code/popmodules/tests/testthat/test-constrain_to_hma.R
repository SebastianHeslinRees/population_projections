library(popmodules)
library(testthat)

hma_list <- list(a = c(1:3), b=c(4:6))

constraint <- data.frame(hma=c("a","b"), births=c(60,60))

popn <- data.frame(gss_code = 1:10, births = rep(2,10))

test_output <- data.frame(gss_code = 1:10, births = c(rep(20,6),rep(2,4)))

test_that("constraint_to_hma creates the expected output", {
testthat::expect_equal(
  constrain_to_hma(popn, constraint, hma_list,
                   col_aggregation = "hma",
                   col_popn = "births", col_constraint = col_popn),
  test_output)
})
