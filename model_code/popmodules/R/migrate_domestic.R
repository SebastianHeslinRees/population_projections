#' Apply origin-destination migration rates to a population
#'
#' Given a cohort population and a data frame of migration rates that can be
#' joined to the population, return a data table of origin-destination flows.
#'
#' The output data frame will use the names of the migration dataset for the
#' origin and destination columns (if origin columns differ).
#'
#' The function tries to maintain tibbles, factors and grouping to match the
#' inputs, with the input population taking precedence when there is a conflict.
#'
#' The function can't yet handle migration data at a lower geographic resolution than
#' the population data, but it's on the to do list.
#'
#' Despite its fancy appearance, it's really just a little wrapper around
#' popn_apply_rate
#'
#' @param popn A data frame containing population data.
#' @param mign_rate A data frame containing origin-destination migration at the
#'   same or lower resolution than \code{popn}.
#' @param col_aggregation A string or named character vector giving the names of
#'   columns in \code{popn} which the output will be aggregated to, and how they
#'   join to columns in \code{mign_rate}. In particular, the mapping from the
#'   population geography to the migration origin geography columns must be
#'   specified, e.g. \code{c("year", "gss_code"="gss_out", "sex", "age")}. The
#'   inflow GSS is specified in the next parameter. All elements must give
#'   columns in \code{popn} but not all need to be in \code{mign_rate}, (that
#'   is, \code{mign_rate} can be at a coarser resolution). If there is more than
#'   one geographic resolution, only specify the finest resolution common to the
#'   population data frame and the migration origin data. Default
#'   \code{c("year", "gss_code"="gss_out", "sex", "age")}.
#' @param col_gss_destination String or character vector. Column(s) in
#'   \code{mign_rate} containing migration rate destination geographies, plus
#' any other aggregation levels not present in \code{popn} that you want to join
#' to. Usually this just will be the desination geographies column. If it is of
#' length two or more you will need to specify \code{col_outflow_inflow} below.
#' Default "gss_in".
#' @param col_popn String. Name of column in \code{popn} containing population
#'   counts. Default "popn"
#' @param col_rate String. Name of column in \code{mign_rate} containing
#'   migration rates. Default "rate"
#' @param col_flow String. Name of column to write output flows to. Default
#'   "flow".
#' @param pop1_is_subset Logical. If the two input data frames cover the same
#'   domain and you expect every level of \code{mign_rate} to be matched to by a
#'   level in \code{popn} set this to TRUE, and this will be checked. Default
#'   FALSE.
#' @param many2one Logical. Setting this to FALSE will check that no more than
#'   one level from \code{popn} matches to each level of \code{mign_rate}.
#'   Default TRUE.
#' @param missing_levels_rate Logical. Setting this to TRUE will assume missing
#'   origin-destination flows are zero. Note that your origin-destination flows
#'   must contain rates from each geography to itself before this can be set to
#'   FALSE. If FALSE then an error will be thrown if missing levels are
#'   detected. Default TRUE.
#' @param col_origin_destination Character vector. Names of the origin and
#'   destination columns in \code{mign_rate}. Only required when
#'   \code{col_gss_destination} is of length two or more, though providing it
#'   (marginally) speeds things up. The code is able to infer its value
#'   otherwise. Default NA.
#'
#' @return A data frame of origin-destination flows at the aggregation levels
#'   provided by \code{col_aggregation}.
#'
#' @import assertthat
#'
#' @examples
#'
#' TODO examples
#'
#' @export
#'

# TODO: make this a more general CoC from rates, popn
# TODO: add a by argument to the function, with default c("gss_code", "sex", "age", "year")

# TODO: case: deal with origin and destination data being at different resolutions?
# TODO: deal with origin destination data being coarser than the input
# TODO: add nesting functionality :O
# TODO: add functionality to deal with incomplete input populations or rates
# (currently it checks that the rate is complete at all levels matching to
# population, but doesn't need origin-destination data to be complete)

migrate_domestic <- function(popn,
                             mign_rate,
                             col_aggregation = c("year", "gss_code"="gss_out", "sex", "age"),
                             col_gss_destination = "gss_in",
                             col_popn = "popn",
                             col_rate = "rate",
                             col_flow = "flow",
                             pop1_is_subset = FALSE,
                             many2one = FALSE,
                             missing_levels_rate = TRUE,
                             col_origin_destination = NA) {

  # Validate input
  # --------------
  validate_migrate_domestic_input(popn, mign_rate, col_aggregation, col_gss_destination,
                                  col_popn, col_rate, col_flow,
                                  pop1_is_subset, many2one, missing_levels_rate, col_origin_destination)

  # identify origin and destination columns if we don't have them
  if(identical(col_origin_destination, NA)) {
    col_origin_destination <- c(find_matching_column_data(popn, mign_rate, col_gss_destination, col_aggregation), col_gss_destination)
  }
  col_aggregation <- convert_to_named_vector(col_aggregation)

  # limit to columns we're interested in, fill missing if requested,
  # and add zero migration rates from each geography to itself
  mign_rate <- standardise_migration_domestic_rates(mign_rate, col_aggregation, col_gss_destination,
                                                    col_rate, missing_levels_rate, col_origin_destination)
  popn <- popn[ c(names(col_aggregation), col_popn)]

  # Calculate deaths
  # ----------------
  # This will deal with most checks on the input and output
  migration <- popn_apply_rate(popn,
                               mign_rate,
                               col_aggregation,
                               col_popn,
                               col_rate,
                               col_flow,
                               pop1_is_subset = pop1_is_subset,
                               many2one = many2one,
                               additional_rate_levels = col_gss_destination,
                               missing_levels_popn = FALSE,
                               missing_levels_rate = missing_levels_rate)

  # rename input gss column to match origin column
  old_name <- names(col_aggregation)[col_aggregation == col_origin_destination[1]]
  names(migration)[names(migration) == old_name] <- col_origin_destination[1]


  # Validate output
  # ---------------

  # All that's left is to make sure there are no rates < 0 or rates that exceed
  # the population Note: we *won't* check that the sum total population leaving
  # each geography is less than the total population: the calculation is a
  # little pricey and it'll be checked later when we sum the rates (and we
  # already checked outmigration rates sum to <1 )
  assert_that(!any(migration[[col_popn]] < migration[[col_flow]]),
              msg = "migrate_domestic flows exceeding the population size")
  # TODO surely there's some other validation we should do here

  return(migration)
}


# ---------------------------------------------------------------------------


# Check the function input is valid
validate_migrate_domestic_input <- function(popn,
                                            mign_rate,
                                            col_aggregation,
                                            col_gss_destination,
                                            col_popn,
                                            col_rate,
                                            col_flow,
                                            pop1_is_subset,
                                            many2one,
                                            missing_levels_rate,
                                            col_origin_destination) {


  # Type checking
  assert_that(is.data.frame(popn),
              msg = "migrate_domestic needs a data frame as input")
  assert_that(is.data.frame(mign_rate),
              msg = "migrate_domestic needs a data frame of origin-destination data")
  assert_that(is.character(col_aggregation),
              msg = "migrate_domestic needs a string or character vector as the col_aggregation parameter")
  assert_that(is.character(col_gss_destination),
              msg = "migrate_domestic needs a string as the col_gss_destination parameter")
  assert_that(is.string(col_popn),
              msg = "migrate_domestic needs a string as the col_popn parameter")
  assert_that(is.string(col_rate),
              msg = "migrate_domestic needs a string as the col_rate parameter")
  assert_that(is.string(col_flow),
              msg = "migrate_domestic needs a string as the col_flow parameter")
  assert_that(identical(col_origin_destination, NA) | is.character(col_origin_destination),
              msg = "migrate_domestic needs NA or a character vector for the col_origin_destination parameter")
  assert_that(rlang::is_bool(pop1_is_subset),
              msg = "migrate_domestic needs a logical value for pop1_is_subset")
  assert_that(rlang::is_bool(many2one),
              msg = "migrate_domestic needs a logical value for many2one")
  assert_that(rlang::is_bool(missing_levels_rate),
              msg = "migrate_domestic needs a logical value for missing_levels_rate")

  # Other checks
  # TODO most of these checks are actually done by popn_apply_rates and maybe shouldn't be duplicated here?
  col_aggregation <- convert_to_named_vector(col_aggregation) # convert to named vector mapping between popn and mign_rate aggregation levels

  assert_that(nrow(popn) > 0,
              msg = "migrate_domestice was given an empty population data frame")
  assert_that(nrow(mign_rate) > 0,
              msg = "migrate_domestice was given an empty origin-destination rates data frame")
  assert_that(!col_popn %in% names(col_aggregation),
              msg = "migrate_domestic was given a population count column name that is also a named aggregation column")
  assert_that(!col_rate %in% names(col_aggregation),
              msg = "migrate_domestic can't have a col_rate that is also a named aggregation column in the input")
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in migrate_domestic, all columns named in col_aggregation must be columns in the popn table")
  assert_that(!any(duplicated(names(col_aggregation))),
              msg = "duplicated population column names were provided to migrate_domestic")
  assert_that(!any(duplicated(as.character(col_aggregation))),
              msg = "duplicated mign_rate column names were provided to migrate_domestic")
  assert_that(all(col_gss_destination %in% names(mign_rate)),
              msg = "migrate_domestic was given column names in col_gss_destination that aren't in the mign_rate data frame")
  if(any(col_gss_destination %in% col_aggregation)) {
    warning("migrate_domestic found values of col_aggregation repeated in col_gss_destination: these will be removed")
    assert_that(!all(col_gss_destination %in% col_aggregation),
                msg="migrate_domestic needs columns specfied in col_gss_destination that aren't in col_aggregate")
  }
  assert_that(!col_rate %in% col_gss_destination,
              msg = "migrate_domestic can't have a col_rate that is also a named aggregation column in the rate data")
  assert_that(is.numeric(popn[[col_popn]]),
              msg = paste("migrate_domestic needs a numeric column in the specified population count col:", col_popn))
  assert_that(is.numeric(mign_rate[[col_rate]]),
              msg = paste("migrate_domestic needs a numeric column in the specified mign_rate rate col:", col_rate))
  assert_that(all(mign_rate[[col_rate]] >= 0) && all(mign_rate[[col_rate]] <= 1),
              msg = "mign_rate rates in migrate_domestic must be between 0 and 1")
  assert_that(!col_flow %in% c(names(col_aggregation), col_gss_destination),
              msg = paste(c("migrate_domestic can't handle a flow column with the same name as one of the requested aggregation column names",
                            "\nFlow column:", col_flow,
                            "\nAggregation columns:", c(names(col_aggregation), col_gss_destination)), collapse = " "))
  if(col_flow %in% names(popn)) {
    warning(paste("migrate_domestic is writing flow data to a column name that was also in the input:", col_flow,
                  "\nThe output will contain the calculated flow in this column, so be careful with subsequent joins or binds."))
  }

  col_mign_agg <- intersect(names(mign_rate), c(as.character(col_aggregation), col_gss_destination))
  assert_that(all(complete.cases(mign_rate[col_mign_agg])),
              msg = "migrate_domestic found missing values in the input migration aggregation levels")

  assert_that(!identical(col_origin_destination, NA) | length(col_gss_destination) < 2,
              msg = "migrate_domestic requires a character vector for col_origin_destination when joining to multiple additional destination columns (specified in col_gss_destination)")
  if(!identical(col_origin_destination, NA)) {
    assert_that(length(col_origin_destination) == 2,
                msg = "migrate_domestic's col_origin_destination parameter must be NA or of length 2")
    assert_that(all(col_origin_destination %in% names(mign_rate)),
                msg = "migrate_domestic needs col_origin_destination to specify columns in the mign_rate table")
    assert_that(col_origin_destination[1] %in% col_aggregation,
                msg = "migrate_domestic needs col_origin_destination's first value also to be present in col_aggregation")
  }

  # TODO do some validation on the input since popn_apply_rate will expect missing levels and skip the join checks
  # we can at least test that mign[col_agg] is joinable and complete


  # If we weren't told the origin-destination data columns, figure them out and validate
  # TODO this might be quite slow: see if it shows up in a profvis analysis
  if(identical(col_origin_destination, NA)) {
    col_origin_destination <- c(find_matching_column_data(popn, mign_rate, col_gss_destination, col_aggregation), col_gss_destination)
  }
  # Check the origin and destination columns have the same levels in them
  popn_out_col <- names(col_aggregation)[col_aggregation == col_origin_destination[1]]
  popn_out_in_origin <- intersect(popn[[popn_out_col]], mign_rate[[col_origin_destination[1]]])
  popn_out_in_destination <- intersect(popn[[popn_out_col]], mign_rate[[col_origin_destination[2]]])
  assert_that(setequal(popn_out_in_origin, popn_out_in_destination),
              msg = paste(c("migrate_domestic seems to have different origin and destination levels in the rate columns",
                            col_origin_destination), collapse = " "))

  # We run a couple extra checks here that will be missed in popn_apply_rate due to the (potentially) incomplete
  # origin-destination data. It works on the assumption that there are no missing levels in the migration data
  # other than missing origin-destination rates.
  if(missing_levels_rate) {
    validation_agg_levels <- col_aggregation[col_aggregation %in% names(mign_rate)]

    validate_population(popn,
                        names(col_aggregation),
                        col_data = NA,
                        test_complete = TRUE,
                        test_unique = TRUE)

    tryCatch({
      mign_validation <- unique(data.table::as.data.table(mign_rate[validation_agg_levels]))
      validate_population(mign_validation,
                          col_aggregation = validation_agg_levels,
                          col_data = NA,
                          test_complete = TRUE,
                          test_unique = FALSE)

      validate_join_population(popn,
                               mign_validation,
                               cols_common_aggregation = validation_agg_levels,
                               pop1_is_subset = pop1_is_subset,
                               many2one = many2one,
                               one2many = FALSE,
                               warn_unused_shared_cols = TRUE)},
      warning = function(w) {
        warning(paste("migrate_domestic threw a warning while validating the migration inputs:\n", w))
      },
      error = function(e) {
        stop(paste(c("migrate_domestic failed to validate the migration inputs.",
                     "\nNote: Since missing_levels_rate is TRUE, the validation is only against the population aggregation columns -",
                     as.character(validation_agg_levels), "- which are expected to pass validate_population and validate_join_population",
                     "with the input popn data frame.",
                     "\n\nError message:\n", e), collapse = " "))
      })
  }

  # Other checks (by the more expensive validatepop functions) are done within popn_apply_rates

  invisible(TRUE)
}


# ---------------------------------------------

# find columns in one data frame that have the same contents as those in another
find_matching_column_data <- function(popn, mign_rate, col_known_gss, col_aggregation) {
  test_gss <- mign_rate[[col_known_gss]][1]
  ix <- sapply(popn, function(col_data) test_gss %in% col_data) %>%
    which() # find columns with gss data
  if(length(ix) == 0) {ix <- 1:ncol(popn)} # in the case that the test_gss isn't actually in the popn data: just test everything

  popn_col_match <- sapply(ix, function(i) { length(setdiff(popn[[i]], mign_rate[[col_known_gss]])) == 0 }) # find ones with a full match to destination data
  popn_col_match <- names(popn)[ix][popn_col_match]
  assert_that(!length(popn_col_match) >  1,
              msg = paste(c("migrate_domestic was unable to identify the origin and destination columns in the migration data unambiguously.",
                            "Please specify them with the col_origin_destination parameter.",
                            "\nAmbiguous origin column matches:", popn_col_match), collapse=" "))
  assert_that(!length(popn_col_match) <  1,
              msg = paste("migrate_domestic was unable to identify the origin and destination columns in the migration data.",
                          "This is most likely due to geographies in the origin data that aren't present anywhere in the destination data."))
  mign_col_match <- col_aggregation[names(col_aggregation) %in% popn_col_match]
}

# ---------------------------------------------

standardise_migration_domestic_rates <- function(mign_rate,
                                                 col_aggregation,
                                                 col_gss_destination,
                                                 col_rate,
                                                 missing_levels_rate,
                                                 col_origin_destination) {
  # limit to columns we care about
  col_mign_agg <- c(as.character(col_aggregation), col_gss_destination) %>%
    intersect(names(mign_rate))

  mign_rate <- mign_rate[ c(col_mign_agg, col_rate) ]

  # check for missing rates if we're not assuming they're zero
  if(!missing_levels_rate) {
    expected_rows <- prod(sapply(mign_rate[col_mign_agg],
                                 function(col) length(unique(col))))
    assert_that(nrow(mign_rate) == expected_rows,
                msg = paste("migrate_domestic's origin-destination migration rates seem to be incomplete:",
                            "\nExpected number of rows:", expected_rows,
                            "\nOutput:", nrow(mign_rate),
                            "\nIf this is ok, and missing rates should be zero, set missing_levels_rate to TRUE.",
                            "Note that this check also expects migration rates from each geography to itself."))
  }

  return(mign_rate)
}


