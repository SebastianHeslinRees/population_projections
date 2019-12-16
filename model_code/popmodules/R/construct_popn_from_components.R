#' Add components to a starting population
#'
#' Shortcut function to combine population components
#'
#' @param start_population Data frame with initial population. All subsequent inputs should be at the same resolution.
#' @param addition_data a List of component dataframes contain data to be added to the start population
#' @param subtraction_data a List of component dataframes contain data to be subtracted from the start population
#' @param col_aggregation Aggregation columns common to all the above to join on.
#'
#' @return A data frame with all components added.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom dtplyr lazy_dt
#'
#' @export
#'
construct_popn_from_components <- function(start_population,
                                           addition_data,
                                           subtraction_data,
                                           col_aggregation = c("year","gss_code","sex","age")){
  
  validate_construct_popn_from_component_input(start_population, addition_data, subtraction_data, col_aggregation)
  
  nm <- last(names(start_population))
  
  start_population <- mutate(start_population, var = "start") %>%
    rename(popn = last(names(start_population))) %>%
    select_at(c(col_aggregation, "var", "popn"))
  
  for(i in seq(addition_data)){
    addition_data[[i]] <- tidyr::pivot_longer(addition_data[[i]],
                                              ncol(addition_data[[i]]),
                                              names_to = "var",
                                              values_to = "popn") %>%
      select(col_aggregation, "var", "popn")
  }
  
  for(i in seq(subtraction_data)){
    subtraction_data[[i]] <- tidyr::pivot_longer(subtraction_data[[i]],
                                                 ncol(subtraction_data[[i]]),
                                                 names_to = "var",
                                                 values_to = "popn") %>%
      mutate(popn = popn*-1) %>%
      select(col_aggregation, "var", "popn")
  }
  
  constructed <- rbind(start_population,
                       data.table::rbindlist(addition_data),
                       data.table::rbindlist(subtraction_data)) %>%
    lazy_dt() %>%
    group_by_at(col_aggregation) %>%
    summarise(popn = sum(popn)) %>%
    as.data.frame() %>%
    select(col_aggregation, "popn")
  
  return(constructed)
  
}

validate_construct_popn_from_component_input <- function(start_population,
                                                         addition_data,
                                                         subtraction_data,
                                                         col_aggregation){
  #validation
  #are addition and subratction lists
  assertthat::assert_that(is.list(addition_data),
                          msg="In construct_popn_from_components addition_data must be a list of dataframes")
  assertthat::assert_that(is.list(subtraction_data),
                          msg="In construct_popn_from_components subtraction_data must be a list of dataframes")
  
  #are the elements of those lists dataframes
  assertthat::assert_that(is.data.frame(start_population),
                          msg = "construct_popn_from_components: start_population must be a dataframe")
  
  assertthat::assert_that(
    all(
      as.logical(
        sapply(addition_data, is.data.frame)
      )
    ),
    msg = "construct_popn_from_components: All elements the addition_data list must be dataframes"
  )
  
  assertthat::assert_that(
    all(
      as.logical(
        lapply(subtraction_data, FUN = function(x) is.data.frame(x))
      )
    ),
    msg = "construct_popn_from_components: All elements the subtraction_data list must be dataframes"
  )
  
  #is the final colmn of each df numeric
  assertthat::assert_that(
    all(
      as.logical(
        lapply(addition_data, FUN = function(x) is.numeric(x[[ncol(x)]]))
      )
    ),
    msg = "construct_popn_from_components: The final column in every addition_data dataframe must be numeric"
  )
  
  assertthat::assert_that(
    all(
      as.logical(
        lapply(subtraction_data, FUN = function(x) is.numeric(x[[ncol(x)]]))
      )
    ),
    msg = "construct_popn_from_components: The final column in every subtraction_data dataframe must be numeric"
    
  )
  
  #every data frame contains col_aggregation fields
  
  assertthat::assert_that(
    all(
      as.logical(
        lapply(addition_data, FUN = function(x) all(col_aggregation %in% names(x)))
      )
    ),
    msg = "construct_popn_from_components: one or more col_aggregation columns not found in one of the addition_data dataframes"
  )
  
  assertthat::assert_that(
    all(
      as.logical(
        lapply(subtraction_data, FUN = function(x) all(col_aggregation %in% names(x)))
      )
    ),
    msg = "construct_popn_from_components: one or more col_aggregation columns not found in one of the subtraction_data dataframes"
  )
  
  #every dataframe contains the same gss codes
  assertthat::assert_that(
    all(
      as.logical(
        lapply(addition_data, FUN = function(x) validate_same_geog(start_population, x)
        ))
    ),
    msg = "construct_popn_from_components: dataframes in addition_data contain different gss_codes to start_population - they must be indentical"
  )
  
  assertthat::assert_that(
    all(
      as.logical(
        lapply(subtraction_data, FUN = function(x) validate_same_geog(start_population, x)
        ))
    ),
    msg = "construct_popn_from_components: dataframes in subtraction_data contain different gss_codes to start_population - they must be indentical"
  )
  
}
