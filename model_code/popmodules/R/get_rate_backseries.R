#' Divide a component of change by population to give historical rates
#'
#' Takes filepaths to the mid year estimates for the desired component of
#' change, total population, and birth rates, and returns a data frame with
#' rates.
#'
#' The function assumes input has the standard mid-year-estimate column names:
#' \code{c("gss_code","gss_name","geography","country","sex","age","year")} with
#' data stored in a \code{value} column.
#'
#' If the components of change data is known to be at fewer levels than the
#' population mid-year estimates, then the \code{col_partial_match} parameter
#' should name the columns where this is the case. This will be true for example
#' with fertility data, which is limited to a certain age range.
#'
#' Due to the way data comparisons work, the population must be agerd on as part
#' of the calculations, and the function also needs births estimates as input.
#'
#' @param component_mye_path Path to the mid-year estimates of the desired
#'   component of change.
#' @param popn_mye_path Path to poplation mid-year estimates.
#' @param births_mye_path Path to births mid-year estimates.
#' @param col_partial_match List of columns in \code{component_mye_path} where
#'   levels are a subset of those in \code{popn_mye_path}.
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
                                     col_partial_match = NULL) {

  # List valid names of population components
  component_names <- c("births", "deaths", "popn", "int_in", "int_out", "int_net", "dom_in", "dom_out", "dom_net")

  # population is aged on due to definitions of MYE to ensure the correct denominator
  # population in population at 30th June
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

  component <- readRDS(component_mye_path)

  component_name <- intersect(names(component), component_names)
  assert_that(length(component_name) == 1,
              msg = paste(c("get_rate_backseries couldn't find a unique column for the component in the file",
                            component_mye_path,
                            "\nColumn names:", names(component),
                            "\nComponent names searched for:", component_names), collapse=" "))

  common_years <- intersect(common_years, component[["year"]])
  popn <- filter(popn, year %in% common_years)
  component <- filter(component, year %in% common_years)

  if(any(component[,component_name] < 0)) {
    warning(paste("get_rate_backseries found negative counts in the components in", component_mye_path,
                  "- these will be set to zero"))
    component[[component_name]] <- ifelse(component[[component_name]] < 0, 0, component[[component_name]])
  }

  # If there are missing levels expected in the component data, check all *other* levels are complete and match
  if(!is.null(col_partial_match)) {
    levels <- setdiff( c("year", "gss_code", "age", "sex"), col_partial_match)
    n_levels_popn <- select_at(popn, levels) %>%
      group_by_at(levels) %>%
      summarise() %>%
      nrow()
    n_levels_component <- select_at(component, levels) %>%
      group_by_at(levels) %>%
      summarise() %>%
      nrow()
    assert_that(n_levels_component == n_levels_popn,
                msg = paste(c("get_rate_backseries found differering aggregation levels in the populations at",
                              component_mye_path, "and", popn_mye_path, "when comparing on", levels), collapse=" "))
  }

  # validate the component population and check levels match popn for the join
  popmodules::validate_population(component, col_data = component_name)
  popmodules::validate_join_population(component, popn,
                                       cols_common_aggregation = c("year","gss_code", "age", "sex"),
                                       pop1_is_subset = !is.null(col_partial_match),
                                       many2one = FALSE,
                                       one2many = FALSE,
                                       warn_unused_shared_cols = FALSE)

  # TODO split the rates calculation out into a separate function
  # TODO check the methodological decision to set value to 0 if popn is 0

  join_cols <- intersect(names(popn), names(component))
  # TODO add these join cols to the log

  rates <- left_join(popn, component, by=join_cols) %>%
    mutate(!!sym(component_name) := ifelse(is.na(!!sym(component_name)), 0, !!sym(component_name))) %>%
    mutate(rate = ifelse(popn == 0, 0, !!sym(component_name)/popn)) %>%
    select(-popn, -!!sym(component_name))

  # Set max rate to 1 (this is mostly for mortality purposes, but nothing we're dealing with should be > 1 right?)
  if(any(rates$rate > 1)) {
    n <- sum(rates$rate > 1)
    warning(paste(c("get_rate_backseries found", n, "rates > 1 - these will be set to 1.",
                    "\nInput was", component_mye_path, "and", popn_mye_path,
                    "\nLocations:", unique(rates[rates$rate > 1, "gss_name"])), collapse = " "))
    rates$rate[rates$rate > 1] <- 1
  }

  return(rates)

  # TODO add module function rules checks - must return fert/mort df

}
