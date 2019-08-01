#' Template births function that returns a fixed number of births (default zero)
#' for each aggregation level, and sets their age to zero
#'
#' This is designed to be used as a placeholder births function during model
#' testing and a template for other births functions, along with its tests.
#'
#' @param pop A data frame containing population data
#' @param col_aggregation A string giving the names of columns to which the
#'   output births will be aggregated to. Default \code{c("gss_code","sex",
#'   "age")}
#' @param const Numeric. Number of births to return per geography. Defaults to
#'   zero, but can be set to any positive number
#' @param col_age A string giving the name of the age column, if it exists in
#'   the input, or if it needs to be created for the output. It needn't be an
#'   aggregation level.
#'
#' @return A data frame of births with one row for each distinct value of the
#'   \code{col_aggregation} columns, a column named births with value
#'   \code{const} and a column named \code{col_age} with value 0.
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise mutate
#'
#' @export
#'
births_from_popn_fert <- function(popn,
                                  fertility,
                                  col_aggregation = c("year", "gss_code", "age", "sex"),
                                  col_age = "age",
                                  col_sex = "sex",
                                  col_count = "count",
                                  col_rate = "rate",
                                  col_births = "births",
                                  birthratio_m2f = 1.05) {


  # Validate input
  # --------------
  if(identical(col_sex, NA)) col_sex <- NULL

  validate_births_from_popn_input(popn, fertility, col_aggregation, col_age, col_sex, col_count, col_rate, col_births, birthratio_m2f)

  # Standardise input
  # -----------------
  # (Most of the standardisation work is done by the apply rates function)

  # If sex is specified but is not a column in the fertility data, try to add a sex column and a warn we're making this assumption
  output_sex <- !is.null(col_sex)
  if( output_sex & !col_sex %in% names(fertility) ) {
    warning(paste("births_from_popn_fert has column", col_sex, "for population sex,",
                  "but no corresponding column in the fertility data. The function will assume",
                  "that the fertility rates are per women, not per person."))
    fertility[[col_sex]] <- "f"
    male_fertility <- dplyr::mutate(fertility, !!sym(col_rate) := 0, !!sym(col_sex) := "m")
    fertility <- rbind(fertility, male_fertility)
  }

  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and fertility aggregation levels

  # Apply rates and sum
  # -------------------

  #births <- population_apply_rates(popn,
  #                                 fertility,
  #                                 col_aggregation = col_aggregation,
  #                                 col_count = col_count,
  #                                 col_rate = col_rate,
  #                                 col_out = col_births)

  # TODO replace this with the above
  births <- deaths::deaths_from_popn_mort(popn,
                                          fertility,
                                          col_aggregation = col_aggregation,
                                          col_count = col_count,
                                          col_rate = col_rate,
                                          col_deaths = col_births)

  total_births <- sum(births[[col_births]])

  cols_grouping <- setdiff(names(col_aggregation), c(col_age, col_sex))

  births <- births %>%
    group_by( !!!syms(cols_grouping) ) %>%
    summarise( !!sym(col_births) := sum(!!sym(col_births)) )

  # Split births by sex
  # -------------------
  if(output_sex) {
    prop_male <- birthratio_m2f / (1 + birthratio_m2f)
    prop_female <- 1 / (1 + birthratio_m2f)

    births_names <- names(births)
    births <- tidyr::expand(births, !!!syms(births_names), sex = c("f","m")) %>%
      mutate( !!sym(col_births) := ifelse( !!sym(col_sex) == "m", !!sym(col_births) * prop_male, !!sym(col_births) * prop_female))
  }
  assert_that(total_births == sum(births[[col_births]]))

  births[[col_age]] <- 0

  # Standardise and validate output
  # -------------------------------
  # Reorder output columns to match input
  output_cols <- intersect(names(popn), names(col_aggregation)) %>%
    union(c(col_age, col_sex, col_births))

  assert_that(setequal(names(births), output_cols))
  births <- births[output_cols]

  validate_births_from_popn_output(births, popn, col_aggregation, col_age, col_sex, col_births)

  births
}





validate_births_from_popn_input <- function(popn,
                                            fertility,
                                            col_aggregation,
                                            col_age,
                                            col_sex,
                                            col_count,
                                            col_rate,
                                            col_births,
                                            birthratio_m2f) {

  # Type checking
  assert_that(is.data.frame(popn),
              msg = "births_from_popn_fert needs a data frame as input")
  assert_that(is.data.frame(fertility),
              msg = "births_from_popn_fert needs a data frame of fertility data")
  assert_that(is.character(col_aggregation),
              msg = "births_from_popn_fert needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_age),
              msg = "births_from_popn_fert needs a string as the col_age parameter")
  assert_that(is.null(col_sex) | is.string(col_sex),
              msg = "births_from_popn_fert needs a string as the col_sex parameter")
  assert_that(is.string(col_count),
              msg = "births_from_popn_fert needs a string as the col_count parameter")
  assert_that(is.string(col_rate),
              msg = "births_from_popn_fert needs a string as the col_rate parameter")
  assert_that(is.string(col_births),
              msg = "births_from_popn_fert needs a string as the col_births parameter")
  assert_that(is.number(birthratio_m2f) | is.null(col_sex),
              msg = "births_from_popn_fert needs a number as the birthratio_m2f parameter")

  # Other checks
  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and fertility aggregation levels
  assert_that(!col_count %in% names(col_aggregation),
              msg = "births_from_popn_fert was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% col_aggregation,
              msg = "births_from_popn_fert was given a fertility rate column name that is also a named aggregation column")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in births_from_popn_fert, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to births_from_popn_fert")
  assert_that(!any(duplicated(col_aggregation)),
              msg = "duplicated fertility column names were provided to births_from_popn_fert")
  assert_that(is.null(col_sex) | all(popn[[col_sex]] %in% c("m", "f")),
              msg = paste("births_from_popn_fert needs values of 'm' or 'f' in the specified sex col:", col_sex))
  assert_that(is.numeric(popn[[col_count]]),
              msg = paste("births_from_popn_fert needs a numeric column in the specified population count col:", col_count))
  assert_that(is.numeric(fertility[[col_rate]]),
              msg = paste("births_from_popn_fert needs a numeric column in the specified fertility rate col:", col_rate))
  assert_that(all(fertility[[col_rate]] >= 0 && fertility[[col_rate]] <= 1),
              msg = "fertility rates in births_from_popn_fert must be between 0 and 1 (an arbitrary upper limit here)")
  assert_that(is.null(col_sex) | (birthratio_m2f >= 0 && birthratio_m2f <= 2),
              msg = "fertility rates in births_from_popn_fert must be between 0 and 2 (an arbitrary upper limit)")
  assert_that(!col_births %in% names(col_aggregation),
              msg = paste("births_from_popn_fert can't handle a births column with the same name as one of the aggregation columns",
                          "\nbirths column:", col_births,
                          "\nAggregation columns:", col_aggregation))
  assert_that(col_births != col_age,
              msg = paste("births_from_popn_fert can't output births and age data with the same column names"))
  assert_that(col_births != col_sex | is.null(col_sex),
              msg = paste("births_from_popn_fert can't output births and sex data with the same column names"))
  if(col_births %in% names(popn)) {
    warning(paste("births_from_popn_fert is writing births to a column name that was also in the input:", col_births,
                  "\nThe output will contain the calculated births in this column, so be careful with subsequent joins or binds."))
  }
  assert_that( length(setdiff(names(col_aggregation), c(col_age, col_sex))) > 0,
               msg = "births_from_popn_fert can't currently deal with aggregation only over age and/or sex. If we need to change this it shouldn't be too hard to update the code.")

  join_by <- col_aggregation[ col_aggregation %in% names(fertility) ]
  assert_that(length(join_by) > 0,
              msg = "births_from_popn_fert must share some aggregation column names with the input fertility, or a column mapping must be included in the col_aggregation parameter")

  if(requireNamespace("validatepop", quietly=TRUE)) {
    validatepop::validate_population(popn,
                                     col_aggregation = names(col_aggregation),
                                     col_data = col_count,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE)
    validatepop::validate_population(fertility,
                                     col_aggregation = join_by,
                                     col_data = col_rate,
                                     test_complete = TRUE,
                                     test_unique = TRUE,
                                     check_negative_values = TRUE)
    validatepop::validate_join_population(popn,
                                          fertility,
                                          cols_common_aggregation = join_by,
                                          pop1_is_subset = TRUE,
                                          many2one = TRUE,
                                          one2many = FALSE)
  }

  invisible(TRUE)
}




validate_births_from_popn_output <- function(births,
                                             popn,
                                             col_aggregation,
                                             col_age,
                                             col_sex,
                                             col_births) {

  output_names <- c(names(col_aggregation), col_age, col_sex, col_births)
  assert_that(setequal( output_names, names(births) ))

  validatepop::validate_population(births,
                                   col_aggregation = col_aggregation,
                                   col_data = col_births)

  # TODO: update validate_join_population so that pop1_is_subset can take a vector of columns that are subsets in pop1_is_subset
  validatepop::validate_join_population(births,
                                        unique(dplyr::select(popn, -!!sym(col_age), -!!sym(col_sex))), # input popn without age and sex
                                        cols_common_aggregation = setdiff(col_aggregation, c(col_age, col_sex)),
                                        pop1_is_subset = TRUE,
                                        many2one = TRUE,
                                        one2many = FALSE)
}


# Function: convert character vector (unnamed or partially named) to one where every element is named
# TODO split this out into the general or helper_functions package. it's used in validate_pop::validate_join_population as well, and will be in births
convert_to_named_vector <- function(vec) {
  assert_that(is.vector(vec))

  if(is.null(names(vec))) {
    names(vec) <- vec
  } else {
    ix <- names(vec) == ""
    names(vec)[ix] <- vec[ix]
  }

  return(vec)
}



