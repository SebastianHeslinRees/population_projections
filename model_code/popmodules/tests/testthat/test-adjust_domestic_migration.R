library(testthat)
library(popmodules)

popn <- data.frame(gss_code = letters[1:6],
                   borough = c(1,1,2,2,3,3),
                   popn = 1)

target <- data.frame(borough = 1:3,
                     popn = c(2, 4, 0))

dom_in <- dplyr::rename(popn, dom_in = popn)
dom_out <- dplyr::rename(popn, dom_out = popn)

output <- list(
  dom_in <- data.frame(gss_code = letters[1:6],
                       borough = c(1,1,2,2,3,3),
                       dom_in = c(1,1,2,2,0,0)),
  dom_out <- data.frame(gss_code = letters[1:6],
                        borough = c(1,1,2,2,3,3),
                        dom_out = c(1,1,1,1,1,1))
)

test_that("adjust_domestic_migration works", {
  expect_equal(adjust_domestic_migration(popn, target,
                                         col_aggregation = c("borough"),
                                         dom_in, dom_out,
                                         col_popn = "popn", col_target = "popn"),
               output)
})
