#' Divide a component of change by population to give historical rates
#'
#' Takes filepaths to the mid year estimates for the desired component of
#' change, total population, and birth rates, and returns a data frame with
#' rates.
#'
#' The function assumes that MYE data has the standard mid-year-estimate
#' aggregation columns: \code{c("gss_code","sex","age","year")}. The component
#' file is assumed to have the same columns unless this is specified in the
#' \code{col_aggregation} parameter and with data stored in the \code{value}
#' column Note, however, that a column named "year" is currently required in all
#' input datasets.
#'
#' If the components of change data is known to be at fewer levels than the
#' population mid-year estimates, then the \code{col_partial_match} parameter
#' should name the columns where this is the case. This will be true for example
#' with fertility data, which is limited to a certain age range.
#'
#' The name of the component of change in question need not be specified if it
#' is one of these ONS default MYE components: "births", "deaths", "popn",
#' "int_in", "int_out", "int_net", "dom_in", "dom_out", "dom_net". Otherwise,
#' specify it via the \code{col_component} parameter.
#'
#' Due to the way data comparisons work, the population must be aged on as part
#' of the calculations, and the function also needs births estimates as input.
#'
#' @param component_mye_path Path to the mid-year estimates of the desired
#'   component of change.
#' @param popn_mye_path Path to population mid-year estimates.
#' @param births_mye_path Path to births mid-year estimates.
#' @param col_partial_match List of columns in \code{component_mye_path} where
#'   levels are a subset of those in \code{popn_mye_path}.
#' @param col_aggregation Vector of column names used for aggregation in the
#'   input in \code{component_mye_path}. If column names are different from MYE
#'   columns, specify the mapping from MYE to the component data with a named
#'   character vector. e.g. in origin-destination data,
#'   \code{c("gss_code"="gss_out", "gss_in")}. Default
#'   \code{c("year","gss_code","age","sex")}.
#' @param col_component String. Column name for data in the
#'   \code{component_mye_path} data frame. Need not be specified when one of
#'   "births", "deaths", "popn", "int_in", "int_out", "int_net", "dom_in",
#'   "dom_out", "dom_net".
#'
#' @return A data frame containing rates for the component of change (in the
#'   "rate" column)
#'
#' @importFrom dplyr mutate filter rename select left_join group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#'
#' @export

get_rate_backseries <- function(component_mye_path,
                                popn_mye_path,
                                births_mye_path,
                                col_partial_match = NULL,
                                col_aggregation = c("year","gss_code","age","sex"),
                                col_component = NULL) {

  # List valid names of population components
  usual_col_component <- c("births", "deaths", "popn", "int_in", "int_out", "int_net", "dom_in", "dom_out", "dom_net")


  # LOAD AND SET UP POPULATION DATA
  # -------------------------------

  # population is aged on due to definitions of MYE to ensure the correct denominator
  # population is population at 30th June
  # changes are changes that occured in the 12 months up to 30th June
  # age is the age the cohort is at 30th June
  # TODO add link to ONS documentation for the above
  popn <- readRDS(popn_mye_path) %>%
    popmodules::popn_age_on()

  # TODO change the filter so that it can take any age band: will need to factorise and order age data when read in
  births <- readRDS(births_mye_path) %>%
    filter(age == 0)

  common_years <- intersect(popn[["year"]], births[["year"]])

  popn <- rename(births, popn = births) %>%
    rbind(popn) %>%
    filter(year %in% common_years) %>%
    popmodules::validate_population(col_data = "popn")



  # LOAD AND SET UP COMPONENT DATA
  # ------------------------------

  component <- readRDS(component_mye_path)

  if(is.null(col_component)) {
    col_component <- usual_col_component
  }
  col_component <- intersect(names(component), col_component)

  assert_that(length(col_component) == 1,
              msg = paste(c("get_rate_backseries couldn't find a unique column for the component in the file",
                            component_mye_path,
                            "\nColumn names:", names(component),
                            "\nComponent names searched for:", col_component), collapse=" "))

  if(class(component[["year"]]) == "character") {
    component$year <- as.integer(component$year)
  }



  # WRANGLE AND VALIDATE THE DATASETS FOR A JOIN
  # --------------------------------------------

  # Filter to common years
  common_years <- intersect(common_years, component[["year"]])
  assert_that(length(common_years) > 0,
              msg = "get_rates_backseries didn't find any common levels in the MYE and component data frames' year column")
  popn <- filter(popn, year %in% common_years)
  component <- filter(component, year %in% common_years)

  # Fill missing values as zero
  ix <- is.na(component[[col_component]])
  component[ix, col_component] <- 0

  # Set negative values to zero
  if(any(component[,col_component] < 0)) {
    warning(paste("get_rate_backseries found negative counts in the components in", component_mye_path,
                  "- these will be set to zero"))
    component[[col_component]] <- ifelse(component[[col_component]] < 0, 0, component[[col_component]])
  }

  # If there are missing levels expected in the component data, check all *other* levels are complete and match
  levels <- col_aggregation[ !col_aggregation %in% col_partial_match]
  if(!is.null(col_partial_match)) {
    n_levels_popn <- popn[levels] %>%
      sapply(function(col) length(unique(col))) %>%
      prod()
    n_levels_component <- component[levels] %>%
      sapply(function(col) length(unique(col))) %>%
      prod()
    assert_that(n_levels_component == n_levels_popn,
                msg = paste(c("get_rate_backseries found differering aggregation levels in the populations at",
                              component_mye_path, "and", popn_mye_path, "when comparing on", levels), collapse=" "))
  }

  # validate the component population and check levels match popn for the join
  col_aggregation <- .convert_to_named_vector(col_aggregation)
  join_by <- col_aggregation[ names(col_aggregation) %in% names(popn)]

  popmodules::validate_population(popn)
  popmodules::validate_population(component,
                                  col_aggregation = unname(col_aggregation),
                                  col_data = col_component,
                                  test_complete = is.null(col_partial_match),
                                  test_unique = TRUE)

  # TODO investigate another way to validate this: currently it's failing due to insufficient memory
  # (when it's run as part of the model with a bunch of other memory usage)
  #popmodules::validate_join_population(popn,
  #                                     component,
  #                                     cols_common_aggregation = join_by,
  #                                     pop1_is_subset = !is.null(col_partial_match),
  #                                     many2one = !any(names(popn) %in% col_aggregation),
  #                                     one2many = !any(col_aggregation %in% names(popn)),
  #                                     warn_unused_shared_cols = FALSE)

  # TODO check the methodological decision to set value to 0 if popn is 0



  # JOIN THE DATASETS
  # -----------------

  # we're gonna use data.table because the data are huge when we're dealing with origin-destination data
  # see below for tidyverse equivalent

  popn <- data.table::setDT(popn)
  #data.table::setkeyv(popn, c("year","gss_code","age","sex"))
  component <- data.table::setDT(component)
  #data.table::setkeyv(component, unname(col_aggregation))

  rates <- merge(popn, component, by.x = names(join_by), by.y = unname(join_by))
  rates[, rate := ifelse(popn == 0, 0, get(col_component)/popn)]
  data.table::setnames(rates, names(join_by), unname(join_by))
  data.table::setDF(rates)
  rates <- select(rates, -popn, -!!sym(col_component))

  if(FALSE) { # the above but with tidyverse
    rates <- left_join(popn, component, by=col_aggregation) %>%
      mutate(rate = ifelse(popn == 0, 0, !!sym(col_component)/popn)) %>%
      select(-popn, -!!sym(col_component)) %>%
      rename(setNames(names(join_by), unname(join_by))) # check this...
  }

  # Set max rate to 1 (this is mostly for mortality purposes, but nothing we're dealing with should be > 1 right?)
  if(any(rates$rate > 1)) {
    n <- sum(rates$rate > 1)
    warning(paste(c("get_rate_backseries found", n, "rates > 1 - these will be set to 1.",
                    "\nInput was", component_mye_path, "and", popn_mye_path,
                    "\nLocations:", unique(rates[rates$rate > 1, "gss_code"])), collapse = " "))
    rates$rate[rates$rate > 1] <- 1
  }

  return(rates)

  # TODO add module function rules checks - must return fert/mort df

}
