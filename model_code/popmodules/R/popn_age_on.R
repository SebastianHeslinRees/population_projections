#' Age on population
#'
#' Given a cohort population, advance the cohort's age by one timestep/age band
#' and (if specified) increase the date year.
#'
#' The function works with cohort age data as either numeric data or as an
#' ordered factor (e.g. age bands). If age is numeric then all age bands must be
#' equally spaced by the interval given in \code{timestep}, usually 1.
#'
#' Ageing of the population is done by either adding the timestep to the age
#' (for a numeric age) or by incrementing to the next level of the factor (for
#' age as an ordered factor)./ If \code{col_year} is given, the year will also
#' be increased by \code{timestep}.
#'
#' The highest age/age band is considered to be unbounded. This means that when
#' a population is aged on, the population in this band remains in this band,
#' and the age/band below it is added to it. An ageing population will all
#' eventually be moved into this age band if no other processes are operating.
#'
#' @param popn Data frame containing a cohort population.
#' @param col_aggregation Character vector of columns in \code{popn} acting as
#'   aggregation levels. Default \code{c("year","gss_code","age","sex")}. In the case
#'   of multiple geography columns, only supply the highest resolution column;
#'   other columns will be dropped.
#' @param col_age String denoting the age column. Default "age".
#' @param col_year String denoting the year column. Can be NULL. Default "year".
#' @param col_data String or Character denoting the column(s) containing data.
#'   For multiple columns pass as a vector \code{c("popn","popn_2")}.
#'   Default "popn".
#' @param timestep Numeric denoting the model time step in years. If age data
#'   are numeric, they must be spaced by this interval. If they are an ordered
#'   factor, the function will advance the age by one age band regardless of
#'   this parameter. The \code{col_year} column, if present, will also be
#'   increased by \code{timestep} years. Default 1.
#' @param template_age_levels Numeric vector or a factor giving the expected
#'   levels for the population's ages. This is an optional validation tool that
#'   runs an extra check to make sure all expected levels are present. Default
#'   NULL.
#' @param births Data frame, 0 or \code{NULL}. Births data at the same
#'   aggregation levels as \code{popn} but coded to the lowest age band that can
#'   be joined to the output with \code{rbind}. If zero, then the output rows for
#'   zero-year-olds, all of which are zero. If \code{NULL} then no
#'   zero-year-olds are included in the output. Default \code{NULL}.
#' @param col_births String denoting the name of the column containing data in 
#'   the \code{births} dataframe.
#'
#' @return A data frame containing the population with age advanced by one time
#'   step (or age band). The lowest age will not be present in this output. Rows
#'   are arranged iby \code{col_aggregation}, in that order.
#'
#' @import assertthat
#' @import dplyr
#' @importFrom dtplyr lazy_dt
#' @importFrom plyr mapvalues
#' @importFrom stringr str_detect
#'
#' @export

popn_age_on <- function(popn,
                        col_aggregation=c("year","gss_code","age","sex"),
                        col_age = "age",
                        col_year = "year",
                        col_data = "popn",
                        timestep = 1,
                        template_age_levels = NULL,
                        births=NULL,
                        col_births="births") {
  
  # Validate inputs
  validate_popn_age_on_input(popn,
                             col_aggregation,
                             col_age,
                             col_year,
                             col_data,
                             timestep,
                             template_age_levels,
                             births,
                             col_births)
  
  # Standardise data
  if(!col_age %in% col_aggregation) {
    col_aggregation <- c(col_aggregation, col_age)
  }
  
  #TODO: Why is this necessary?
  if(floor(timestep)==timestep) {
    timestep <- as.integer(timestep)
  }
  
  # reorder col_aggregation to match input column order
  col_aggregation <- intersect(names(popn), col_aggregation)
  
  # Save some properties of the input for later
  popn_is_tibble <- "tbl" %in% class(popn)
  popn_groups <- group_vars(popn)
  popn_factors <- names(popn)[ sapply(popn, is.factor) ]
  age_is_integer <- is.integer(popn[[col_age]]) && (is.null(col_year) || floor(timestep)==timestep)
  
  # Increment the year
  # We do this here so we can use this population for validation later
  if(!is.null(col_year)) {
    popn <- ungroup(popn) %>% 
      mutate(!!col_year := !!sym(col_year) + timestep)
  }
  
  aged <- popn
  
  # If age is numeric, increment age
  if(is.numeric(popn[[col_age]])) {
    max_age <- max(popn[[col_age]])
    aged[[col_age]] <- ifelse(popn[[col_age]] == max_age, max_age, popn[[col_age]] + timestep)
  }
  
  # Otherwise build vector of unique age bands and what they map to
  # (NB this method would also work for numeric age, but it's slower)
  else {
    old_age_values <- levels(popn[[col_age]]) # we've checked this is an ordered factor
    
    n <- length(old_age_values)
    max_age <- old_age_values[n]
    new_age_values <- c( old_age_values[-1], old_age_values[n] )  # Last two age bands map to the same band
    
    # Map old ages to new ages
    aged[[col_age]] <- plyr::mapvalues(aged[[col_age]], old_age_values, new_age_values)
  }
  

  aged <- aged %>%
    lazy_dt() %>% 
    group_by_at(col_aggregation, add=FALSE) %>%
    summarise_at(col_data, sum) %>%
    data.frame()
  
  #Add births
  
  if(!is.null(births)) {
    
    if(is.numeric(births) && births == 0) {
      
      births <- aged %>% 
        select_at(col_aggregation) %>% 
        mutate(!!col_age := 0) %>% 
        mutate(!!col_births := 0) %>% 
        unique()
      
    }
    
    births <- births %>%
      filter(!!sym(col_age) == 0) %>% 
      rename(!!col_data := col_births) %>%
      select(names(aged))
    
    aged <- rbind(births, aged) %>%
      arrange_at(col_aggregation)
    
    if(!is.null(col_year)){
      common_years <- intersect(births[[col_year]], popn[[col_year]])
      aged <- aged %>% filter(!!sym(col_year) %in% common_years)
    }
  }
  
  # Standardise and validate output
  # Match input formatting
  aged <- mutate_at(aged, .vars = popn_factors, .funs = as.factor)
  popn_not_factors <- setdiff(names(aged), popn_factors)
  aged <- mutate_at(aged, .vars = popn_not_factors, .funs = as.vector)
  
  if(age_is_integer) {
    aged[[col_age]] <- as.integer(aged[[col_age]])
  }
  
  if(popn_is_tibble) {
    aged <- tibble::as_tibble(aged)
    if(length(popn_groups) > 0) {
      aged <- group_by_at(aged, popn_groups, add=FALSE)
    }
  }
  
  validate_popn_age_on_output(popn,
                              aged,
                              col_aggregation,
                              col_age,
                              col_year,
                              col_data,
                              births)
  
  return(aged)
}



# -------------------------------------------------------------------------



validate_popn_age_on_input <- function(popn,
                                       col_aggregation,
                                       col_age,
                                       col_year,
                                       col_data,
                                       timestep,
                                       template_age_levels,
                                       births,
                                       col_births) {
  
  # Type checking
  assert_that(is.data.frame(popn),
              msg = "popn_age_on needs a data frame as input")
  assert_that(is.character(col_aggregation),
              msg = "popn_age_on needs a string or character vector as the col_aggregation parameter")
  assert_that(is.string(col_age),
              msg = "popn_age_on needs a string as the col_age parameter")
  assert_that(is.null(col_year) || is.string(col_year),
              msg = "popn_age_on needs NULL or a string as the col_year parameter")
  assert_that(is.number(timestep),
              msg = "popn_age_on needs a number as the timestep parameter")
  assert_that(is.null(template_age_levels) || is.vector(template_age_levels) || is.factor(template_age_levels)  ,
              msg = "popn_age_on needs a vector, a factor, or NULL as the template_age_levels parameter")
  assert_that(is.null(births) || (is.numeric(births) && births == 0) || is.data.frame(births),
              msg = "popn_age_on needs NULL, 0 or a data frame as the births parameter")
  
  # Check for naming conflicts
  assert_that(all(names(col_aggregation) %in% names(popn)),
              msg = "in popn_age_on, all columns named in col_aggregation must be columns in the popn table")
  assert_that(col_age %in% names(popn),
              msg = "popn_age_on was given a col_age parameter that isn't a column in the input popn data frame")
  assert_that(is.null(col_year) || col_year %in% names(popn),
              msg = "popn_age_on was given a col_year parameter that isn't a column in the input popn data frame")
  assert_that(!any(duplicated(col_aggregation)),
              msg = "duplicated aggregation column names were provided to popn_age_on")
  
  # Validate template age levels (if supplied)
  if(!is.null(template_age_levels)) {
    template_age_levels <- unique(template_age_levels)
    
    if(is.factor(template_age_levels)) {
      assert_that(setequal(template_age_levels, levels(template_age_levels)),
                  msg = "popn_age_on's template_age_levels is a factor with the levels mismatching the contents")
    }
    
    missing_popn_age_levels <- setdiff(template_age_levels, popn[[col_age]])
    assert_that(length(missing_popn_age_levels) == 0,
                msg = paste(c("popn_age_on's input popn is missing levels present in the provided template_age_levels:",
                              missing_popn_age_levels), collapse = " "))
    
    missing_template_age_levels <- setdiff(popn[[col_age]], template_age_levels)
    assert_that(length(missing_template_age_levels) == 0,
                msg = paste(c("popn_age_on's input popn contains levels not present in the provided template_age_levels:",
                              missing_template_age_levels), collapse = " "))
  }
  
  # Other checks
  assert_that(nrow(popn) > 0,
              msg = "popn_age_on was given a data frame with 0 rows of input")
  assert_that(length(unique(popn[[col_age]])) > 1,
              msg = "popn_age_on was given a data frame containing only one age value")
  assert_that(is.null(col_year) || is.numeric(popn[[col_year]]),
              msg = "popn_age_on needs numeric values in the col_year year column")
  
  if(is.factor(popn[[col_age]])) {
    assert_that(setequal(popn[[col_age]], levels(popn[[col_age]])),
                msg = "popn_age_on's template_age_levels is a factor with the levels mismatching the contents")
  }
  
  assert_that(is.numeric(popn[[col_age]]) || is.ordered(popn[[col_age]]),
              msg = "popn_age_on: data in col_age must be numeric or an ordered factor")
  assert_that(timestep > 0,
              msg = "popn_age_on: timestep must be > 0")
  if(floor(timestep) != timestep) {
    warning("popn_age_on was given a non-integer timestep - it's not been tested for this situation so check the outputs")
  }
  if(!is.ordered(popn[[col_age]])) {
    assert_that(all( diff(sort(unique(popn[[col_age]]))) == timestep), # we know the data are complete so this simplification is valid
                msg = "popn_age_on needs all ages to be equally spaced (by timestep) in its input data (alternately, convert to an ordered factor)")
  }
  
  tryCatch(validate_population(popn,
                               col_aggregation,
                               test_complete = TRUE,
                               test_unique = TRUE,
                               check_negative_values = FALSE),
           error = function(e) stop(paste0("popn_age_on found an error in the input population it was given:\n",e,"\n")),
           warning = function(w) warning(paste0("popn_age_on threw a warning when checking the input population:\n",w,"\n"))
  )
  
  # Check no non-numeric columns depend on age
  col_non_numeric <- names(popn)[ !sapply(popn, is.numeric)]
  col_non_numeric <- union(col_aggregation, col_non_numeric)
  if(!identical(col_aggregation, col_non_numeric)) {
    col_agg_without_age <- setdiff(col_aggregation, col_age)
    test_unique <- as.data.frame(popn) %>%
      group_by_at(col_agg_without_age) %>%
      select(!!!syms(col_non_numeric), -!!sym(col_age))
    
    unique_levels_agg <- nrow(dplyr::summarise(test_unique)) # number of aggregation levels
    
    test_unique <-group_by_all(test_unique)
    unique_levels_all <- nrow(dplyr::summarise(test_unique))       # number of unique rows of data
    
    assert_that(unique_levels_agg == unique_levels_all,
                msg = "popn_age_on: non-numeric, age-dependent data detected in input. The function can't deal with this.")
  }
  
  #Births
  if(!is.null(births)){
    
    assert_that(length(col_data) == 1,
                msg = "in popn_age_on births can only be added if there is 1 data column")
    
    if(is.data.frame(births)) {
      validate_population(births, col_aggregation = col_aggregation,
                          test_complete = TRUE, test_unique = TRUE, check_negative_values = TRUE)
      
      col_geog <- col_aggregation[stringr::str_detect(col_aggregation, "gss_code")]
      for(i in 1:length(col_geog)){
        assert_that(setequal(births[[col_geog[i]]], popn[[col_geog[i]]]))
      }
      
      assert_that(all(col_aggregation %in% names(births)),
                  msg = "in popn_age_on the births dataframe does not contain the columns in specified in col_aggreagtion")
      
    }
  }
  
  
  invisible(TRUE)
}


# -------------------------------------------------------------

validate_popn_age_on_output <- function(popn,
                                        aged,
                                        col_aggregation,
                                        col_age,
                                        col_year,
                                        col_data,
                                        births) {
  
  popn_totals_in  <- sapply(popn[col_data], sum) %>% 
    as.numeric() 
  
  if(!is.null(births)){
    if(!is.null(col_year)){
      common_years <- intersect(births[[col_year]], popn[[col_year]])
      births_totals_df <- births %>% filter(!!sym(col_year) %in% common_years)
      popn_totals_df <- popn %>% filter(!!sym(col_year) %in% common_years)
    }
    births_totals_in <- sum(births_totals_df[[col_data]])
    popn_totals_in <- sum(popn_totals_df[[col_data]]) + births_totals_in
  }
  
  popn_totals_in <- popn_totals_in %>% round(digits=2)
  popn_totals_out <- sapply(aged[col_data], sum) %>% round(digits=2) %>% 
    as.numeric()
  assert_that(identical(popn_totals_in, popn_totals_out),
              msg = "Error in popn_age_on: column sums have changed")
  
  # FIXME does this check take ages on large datasets?
  if(is.null(births)){
    age_min <- min(popn[[col_age]])
    assert_that(setequal( setdiff(popn[[col_age]], age_min), aged[[col_age]]),
                msg = "Output age level mismatch")
  }
  
  validate_population(aged,
                      col_aggregation,
                      col_data,
                      test_complete = TRUE,
                      test_unique = TRUE,
                      check_negative_values = FALSE,
                      comparison_pop = NA)
}
