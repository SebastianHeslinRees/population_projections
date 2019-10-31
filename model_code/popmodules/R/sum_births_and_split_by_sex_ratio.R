#' Sum a data frame with births by mother's age, and split babies by a sex ratio
#'
#'
#'
#'
#' @import assertthat

sum_births_and_split_by_sex_ratio <- function(births,
                                              birthratio_m2f = 1.05) {

  # TODO decide what checks we need to do!!
  # TODO decide if we need flexbility with column names and aggregation levels

  check_sum_births_input(births, birthratio_m2f)

  prop_male <- birthratio_m2f / (1 + birthratio_m2f)
  prop_female <- 1 / (1 + birthratio_m2f)

  this_year <- births$year[1]

  births <- group_by(births, gss_code) %>%
    summarise(births = sum(births)) %>%
    ungroup() %>%
    right_join(expand.grid(gss_code = unique(births$gss_code), sex = c("female","male"), age = 0, year = this_year, stringsAsFactors=FALSE), by="gss_code") %>%
    mutate(births = ifelse(sex == "male", births * prop_male, births * prop_female)) %>%
    select(year, gss_code, sex, age, births) %>%
    data.frame()

  return(births)
}




check_sum_births_input <- function(births, birthratio_m2f) {

  assert_that(is.data.frame(births),
              msg = "sum_births_and_split_by_sex_ratio needs a data frame as input")
  assert_that(is.numeric(birthratio_m2f),
              msg = "sum_births_and_split_by_sex_ratio needs a numeric value as the male:female birth ratio")

  assert_that(all(c("gss_code", "age", "year", "births") %in% names(births)),
              msg = "sum_births_and_split_by_sex_ratio needs input with gss_code, age, year and births columns")

  assert_that(length(unique(births$year)) == 1,
              msg = "sum_births_and_split_by_sex_ratio currently expects only one year of data as input")
}
