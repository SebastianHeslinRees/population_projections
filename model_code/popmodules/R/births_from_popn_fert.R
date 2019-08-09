#' Apply fertility rates to a population.
#'
#' Given a cohort population and a data frame of fertility rates that can be
#' joined to the population, return a data table with the population's
#' aggregation levels, a births count split by a provided male:female births
#' ratio, and age set to zero.
#'
#' Sex data (if used) must be provided as "m" and "f".
#'
#' @param popn A data frame containing population data.
#' @param fertility A data frame containing fertiity rates per person per year.
#'   If \code{popn} contains sex data specified by \code{col_sex} and this does
#'   not, it will be assumed the rates are per woman (with a warning).
#' @param col_aggregation A character vector giving the names of columns to
#'   which the output births will be aggregated. Each column must be present in
#'   \code{popn}. The vector will be used to establish a mapping to any columns
#'   that are also in \code{fertility}, so use a named vector if names differ,
#'   e.g.\code{c("gss_code"="LSOA11CD")}. At least one column must be in
#'   \code{fertility} to enable a join. Defaults to \code{c("year", "gss_code",
#'   "sex", "age")}.
#' @param col_age String. Name for age column in inputs (if present) and output.
#'   Default "age".
#' @param col_sex String. Name for sex column in inputs (if present) and output.
#'   Set to NULL if you are not working with sex. Default "sex".
#' @param col_count String. Name for population count column in \code{popn}.
#'   Default "count".
#' @param col_rate String. Name for fertility rate column (per person per year)
#'   in \code{fertility}. Default "rate".
#' @param col_births String. Name for births column in output. Default "births".
#' @param birthratio_m2f Numeric. Ratio of male births to female births (if
#'   outputting sex). Default 1.05.
#' @return A data frame of births with one row for each distinct value of the
#'   \code{col_aggregation} columns of the input, plus a \code{col_births}
#'   column containing births, and a \code{col_age} column with value 0.
#'   All other input columns are discarded.
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by ungroup summarise mutate
#'
#' @examples
#'
#' library(births)
#'
#' popn <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), sex=c("f","m"), count = 100)
#' fert <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), rate = 0.01)
#'
#' pop_births <- births_from_popn_fert(popn,
#'                                     fert,
#'                                     colname_aggregation = c("year", "gss_code", "age", "sex"),
#'                                     col_age = "age",
#'                                     col_sex = "sex",
#'                                     col_count = "count",
#'                                     col_rate = "rate",
#'                                     col_births = "births",
#'                                     birthratio_m2f = 1.05)
#' # equivalent to
#' pop_births <- births_from_popn_fert(popn, fert)
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

  # TODO Adapt so that this can work with banded age data (i.e. age != 0 for births)


  # Validate input
  # --------------
  if(identical(col_sex, NA)) {
    col_sex <- NULL
  }

  validate_births_from_popn_input(popn, fertility, col_aggregation, col_age, col_sex, col_count, col_rate, col_births, birthratio_m2f)

  # Standardise input
  # -----------------
  # (Most of the standardisation work is done by the apply rates function)

  # Save some properties of the input so we can recreate it later
  popn_is_tibble <- "tbl" %in% class(popn)
  popn_groups <- dplyr::groups(popn)

  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and fertility aggregation levels

  # If col_sex is specified and is an aggregation column, but is not in the
  # fertility data, set fertility sex to "f" and warn we're making this assumption
  output_sex <- !is.null(col_sex)
  if( output_sex &&
      col_sex %in% names(col_aggregation) &&
      !col_aggregation[col_sex] %in% names(fertility) ) {
    warning(paste("births_from_popn_fert input has column", col_sex, "for population sex,",
                  "but no corresponding column in the fertility data. The function will assume",
                  "that the fertility rates are per women, not per person."))
    fertility[[col_sex]] <- "f"
    male_fertility <- dplyr::mutate(fertility, !!sym(col_rate) := 0, !!sym(col_sex) := "m")
    fertility <- rbind(fertility, male_fertility)
  }
  # In the case where there's no sex data in the popn aggregation
  # levels, and there is sex data in the fertility. The code will crash if
  # there's more than one kind of sex (due to ambiguous joining)

  # Store a one-row data frame so we can recover factors later
  popn_empty <- popn[1,]


  # Apply rates and sum
  # -------------------

  births <- popn_apply_rate(popn,
                            fertility,
                            col_aggregation = col_aggregation,
                            col_count = col_count,
                            col_rate = col_rate,
                            col_out = col_births)


  total_births <- sum(births[[col_births]])

  cols_grouping <- setdiff(names(col_aggregation), c(col_age, col_sex))

  births <- births %>%
    group_by( !!!syms(cols_grouping) ) %>%
    summarise( !!sym(col_births) := sum(!!sym(col_births)) ) %>%
    ungroup()

  # Split births by sex
  # -------------------
  if(output_sex) {
    prop_male <- birthratio_m2f / (1 + birthratio_m2f)
    prop_female <- 1 / (1 + birthratio_m2f)

    births_names <- names(births)
    births <- tidyr::expand(births, tidyr::nesting(!!!syms(births_names)), !!sym(col_sex) := c("f","m")) %>%
      mutate( !!sym(col_births) := ifelse( !!sym(col_sex) == "m", !!sym(col_births) * prop_male, !!sym(col_births) * prop_female))
  }
  assert_that(all.equal(total_births, sum(births[[col_births]])))

  births[[col_age]] <- 0

  # Standardise and validate output
  # -------------------------------
  # Reorder output columns to match input
  output_cols <- intersect(names(popn), names(col_aggregation)) %>%
    union(c(col_age, col_sex, col_births))

  assert_that(setequal(names(births), output_cols))
  births <- births[output_cols]

  if(!popn_is_tibble) {
    births <- as.data.frame(births)
  }
  if(popn_is_tibble & !is.null(popn_groups)) {
    popn <- group_by(popn, !!!syms(popn_groups))
  }

  # Recover columns that were factors
  births <- match_factors(popn_empty, births, intersect(names(popn_empty), names(births)))

  validate_births_from_popn_output(births, popn, col_aggregation, col_age, col_sex, col_births)

  births
}



# -------------------------------------------------------

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

  # Check for naming conflicts
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
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "births_from_popn_fert can't have a col_rate that is also a named aggregation column in the input")
  assert_that(!col_births %in% names(col_aggregation),
              msg = paste(c("births_from_popn_fert can't handle a births column with the same name as one of the aggregation columns",
                            "\nbirths column:", col_births,
                            "\nAggregation columns:", col_aggregation), collapse = " "))
  assert_that(col_births != col_age,
              msg = paste("births_from_popn_fert can't output births and age data with the same column names"))
  assert_that(is.null(col_sex) || col_births != col_sex,
              msg = paste("births_from_popn_fert can't output births and sex data with the same column names"))
  assert_that(col_age != col_count,
              msg = paste("births_from_popn_fert won't allow col_age to equal col_count.",
                          "If you really care about this you could definitely adapt the code to handle it, but my advice is rename the columns."))
  assert_that(is.null(col_sex) || col_count != col_sex,
              msg = paste("births_from_popn_fert won't allow col_sex to equal col_count.",
                          "If you really care about this you could definitely adapt the code to handle it, but my advice is rename the columns."))

  # Check columns contain the correct data
  assert_that(nrow(popn) > 0,
              msg = "births_from_popn_fert was given a table with 0 rows of input")
  assert_that(is.null(col_sex) || all(popn[[col_sex]] %in% c("m", "f")),
              msg = paste("births_from_popn_fert needs values of 'm' or 'f' in the specified sex col:", col_sex))
  assert_that(is.numeric(popn[[col_count]]),
              msg = paste("births_from_popn_fert needs a numeric column in the specified population count col:", col_count))
  assert_that(is.numeric(fertility[[col_rate]]),
              msg = paste("births_from_popn_fert needs a numeric column in the specified fertility rate col:", col_rate))
  assert_that(all(fertility[[col_rate]] >= 0 && fertility[[col_rate]] <= 2),
              msg = "fertility rates in births_from_popn_fert must be between 0 and 2 (an arbitrary upper limit)")
  assert_that(is.null(col_sex) || (birthratio_m2f >= 0 && birthratio_m2f <= 2),
              msg = "fertility rates in births_from_popn_fert must be between 0 and 2 (an arbitrary upper limit)")


  # Other checks
  if(col_births %in% names(popn)) {
    warning(paste("births_from_popn_fert is writing births to a column name that was also in the input:", col_births,
                  "\nThe output will contain the calculated births in this column, so be careful with subsequent joins or binds."))
  }
  if(col_age %in% setdiff(names(popn), names(col_aggregation))) {
    warning(paste("births_from_popn_fert is ignoring input age data in column", col_age,
                  "since it is not included in col_aggregation. Data will still be written out to this column."))
  }
  if(!is.null(col_sex) && col_sex %in% setdiff(names(popn), names(col_aggregation))) {
    warning(paste("births_from_popn_fert is ignoring input sex data in column", col_sex,
                  "since it is not included in col_aggregation. Data will still be written out to this column."))
  }
  assert_that( length(setdiff(names(col_aggregation), c(col_age, col_sex))) > 0,
               msg = "births_from_popn_fert can't currently deal with aggregation only over age and/or sex. If we need to change this it shouldn't be too hard to update the code.")

  join_by <- col_aggregation[ col_aggregation %in% names(fertility) ]
  assert_that(length(join_by) > 0,
              msg = "births_from_popn_fert must share some aggregation column names with the input fertility, or a column mapping must be included in the col_aggregation parameter")


  # Deal with the case that col_sex = NULL but sex is an input column
  if(is.null(col_sex) & "sex" %in% names(popn)) {
    if("sex" %in% names(col_aggregation)) {
      warning(paste("births_from_popn_fert was requested not to output births by sex,",
                    "but the input aggregation levels contain a sex column. Note that the output data will",
                    "contain a sex column, but this will be as an aggregation level, and not assigned as a birth ratio",
                    "as you might expect. It will look odd and it's not obvious to me why you'd want to run the code like this.",
                    "\nIf you want to model the sex ratio of children set col_sex = 'sex'",
                    "\nIf you want to apply fertility by sex and then sum over sex, set col_sex = 'sex' and then sum over sex"))
    } else {
      warning("births_from_popn_fert was requested not to output births by sex, but the input contained a sex column. This will be ignored")
    }
  }

  # Other checks (by the more expensive validate_* functions) are done within popn_apply_rates

  invisible(TRUE)
}


# -------------------------------------------------------

validate_births_from_popn_output <- function(births,
                                             popn,
                                             col_aggregation,
                                             col_age,
                                             col_sex,
                                             col_births) {

  output_names <- c(names(col_aggregation), col_age, col_sex, col_births)
  assert_that(setequal( output_names, names(births) ))
  aggregation_levels <- union(names(col_aggregation), c(col_age, col_sex))

  validate_population(births,
                      col_aggregation = aggregation_levels,
                      col_data = col_births)

  # TODO: update validate_join_population so that pop1_is_subset can take a vector of columns that are subsets in pop1_is_subset
  validation_comparison_cols <- setdiff(names(col_aggregation), c(col_age, col_sex))
  validate_join_population(births,
                           unique(popn[validation_comparison_cols]),
                           cols_common_aggregation = validation_comparison_cols,
                           pop1_is_subset = TRUE,
                           many2one = TRUE,
                           one2many = FALSE,
                           warn_unused_shared_cols = TRUE)
}

# -------------------------------------------------------

# Function: convert character vector (unnamed or partially named) to one where every element is named
# TODO split this out into the general or helper_functions package. it's used in popmodules::validate_join_population as well, and will be in births
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



# ---------------------------------------------------

# Function: given source and target data frames with a column mapping, add or
# remove factoring in the target to match the source
# TODO split this out into a helper function
match_factors <- function(dfsource, dftarget, col_mapping) {
  col_mapping <- convert_to_named_vector(col_mapping)
  for(i in  seq_along(col_mapping)) {
    icol <- col_mapping[i]
    if(is.factor(dfsource[[names(icol)]]) & !is.factor(dftarget[[icol]])) {
      dftarget[[icol]] <- as.factor(dftarget[[icol]])
    }

    source_col <- dfsource[[names(icol)]]
    target_col <- dftarget[[icol]]
    if(!is.factor(source_col) & is.factor(target_col)) {
      col_class <- class(source_col)
      if(col_class == "numeric") {
        dftarget[[names(icol)]] <- levels(target_col)[target_col] %>%
          as.numeric()
      } else {
        dftarget[[names(icol)]] <- as.character(target_col)
      }
    }
  }
  return(dftarget)
}
