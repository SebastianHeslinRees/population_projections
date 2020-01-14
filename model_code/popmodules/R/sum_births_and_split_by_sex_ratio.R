#' Sum a data frame with births by mother's age, and split babies by a sex ratio
#'
#'
#'
#'
#' @import assertthat
#' @import dplyr
#'
#' @export

sum_births_and_split_by_sex_ratio <- function(births,
                                              birthratio_m2f = 1.05,
                                              geog_cols = "gss_code") {

  # TODO decide what checks we need to do!!
  # TODO decide if we need flexbility with column names and aggregation levels

  check_sum_births_input(births, birthratio_m2f, geog_cols)

  prop_male <- birthratio_m2f / (1 + birthratio_m2f)
  prop_female <- 1 / (1 + birthratio_m2f)

  this_year <- births$year[1]

  #Slightly different way of doing the same thing
   births <- group_by_at(births, geog_cols) %>%
     summarise(births = sum(births)) %>%
     ungroup() %>%
     mutate(age = 0, year = this_year, male = births * prop_male, female = births * prop_female) %>%
   select(-births) %>%
   tidyr::pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "births") %>%
   select_at(c("year", geog_cols, "sex", "age", "births")) %>%
   data.frame()

  return(births)
}




check_sum_births_input <- function(births, birthratio_m2f, geog_cols) {

  assert_that(is.data.frame(births),
              msg = "sum_births_and_split_by_sex_ratio needs a data frame as input")
  assert_that(is.numeric(birthratio_m2f),
              msg = "sum_births_and_split_by_sex_ratio needs a numeric value as the male:female birth ratio")

  assert_that(all(geog_cols %in% names(births)),
              msg = paste("sum_births_and_split_by_sex_ratio needs input with", geog_cols, " column(s)"))

  assert_that(length(unique(births$year)) == 1,
              msg = "sum_births_and_split_by_sex_ratio currently expects only one year of data as input")
}
