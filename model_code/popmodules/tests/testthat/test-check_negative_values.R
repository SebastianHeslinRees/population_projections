library(popmodules)

popn <- data.frame(gss_code = letters[1:5], popn = -2:2)
fixed <- data.frame(gss_code = letters[1:5], popn = c(0, 0, 0, 1, 2))


test_that("check_negative_values works", {
  expect_warning(out <- check_negative_values(popn, "popn", set_to_zero = FALSE))
  expect_equal(out, popn)

  expect_warning(out <- check_negative_values(popn, "popn", set_to_zero = TRUE))
  expect_equal(out, fixed)

  expect_equal(fixed, check_negative_values(fixed, "popn", set_to_zero = TRUE))
})
