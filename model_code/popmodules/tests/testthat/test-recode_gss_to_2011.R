context("recode_gss_codes")
library(popmodules)
library(testthat)
library(dplyr)

df_1 <- data.frame(area=c("E07000013","E07000014","E07000015","E07000013","E07000015"),
           sex= rep("male", 5),
           age = rep(0, 5),
           value = c(1,1,1,2,2),
           stringsAsFactors = F)


df_2 <- data.frame(area=c("E07000013","E07000014","E07000015","E07000013","E07000015"),
                   sex= rep("male", 5),
                   age = rep(0, 5),
                   value_1 = c(1,1,1,2,2),
                   value_2 = c(2,2,2,4,4),
                   stringsAsFactors = F)

expect_1 <- data.frame(area=c("E06000049","E06000050"),
                       sex=c("male","male"),
                       age = rep(0, 2),
                       value = c(4, 3),
                       stringsAsFactors = F)

expect_2 <- data.frame(area=c("E06000049","E06000050"),
                       sex=c("male","male"),
                       age = rep(0, 2),
                       value_1 = c(4, 3),
                       value_2 = c(8,6),
                       stringsAsFactors = F)

expect_3 <- data.frame(area=c("E06000049","E06000050"),
                       sex=c("male","male"),
                       age = rep(0, 2),
                       value_1 = c(4/3, 1.5),
                       value_2 = c(8/3,3),
                       stringsAsFactors = F)

#test with 1 var
test_that("recode_gss_codes produces the expected output", {
  # for why we use data.table::copy() here, see the FIXME within the function
  expect_equivalent(recode_gss_codes(data.table::copy(df_1), "area", c("area","sex","age")), expect_1)
})

#test with 2 vars
  test_that("recode_gss_codes produces the expected output", {
    expect_equivalent(recode_gss_codes(data.table::copy(df_2), "area", c("area","sex","age")), expect_2)
  })


#test with fun = mean
  #test with 2 vars
  test_that("recode_gss_codes produces the expected output", {
    expect_equivalent(recode_gss_codes(data.table::copy(df_2), "area", c("area","sex","age"), fun=list(mean)), expect_3)
  })

