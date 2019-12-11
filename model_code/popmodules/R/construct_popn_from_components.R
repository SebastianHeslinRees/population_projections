#' Age on a population and add components to it
#'
#' Shortcut function to combine population components
#'
#' @param start_population Data frame with initial population. All subsequent inputs should be at the same resolution.
#' @param births Data frame with 0-year-olds. Births in column 'births'.
#' @param deaths Data frame with deaths. Deaths in column 'deaths'.
#' @param int_in Data frame with international inflows. Flows in column 'int_in'.
#' @param int_out Data frame with international outflows. Flows in column 'int_out'.
#' @param dom_in Data frame with domestic inflows. Flows in column 'dom_in'.
#' @param dom_out Data frame with domestic outflows. Flows in column ''dom_out'
#' @param upc Data frame with unattributed population change. Currently unused/
#' @param col_aggregation Aggregation columns common to all the above to join on.
#'
#' @return Aged on data frame with all components added.
#'
#' @import dplyr
#'
#' @export
#'
construct_popn_from_components <- function(start_population,
                                                 births,
                                                 deaths,
                                                 int_in,
                                                 int_out,
                                                 dom_in,
                                                 dom_out,
                                                 upc = NULL,
                                                 col_aggregation = c("year","gss_code","sex","age")){

  #TODO check that required columns are present
  #TODO make flexible so that it can take whatever components dataframes you give it
  #it can then be used in the trend core at lines 158-178
  #TODO include UPC!

  constructed <- start_population %>%
    as.data.frame() %>%
    popn_age_on(births = births) %>%
    dtplyr::lazy_dt() %>%
    left_join(deaths, by = col_aggregation) %>%
    left_join(int_in, by = col_aggregation) %>%
    left_join(int_out, by = col_aggregation) %>%
    left_join(dom_in, by = col_aggregation) %>%
    left_join(dom_out, by = col_aggregation) %>%
    mutate(constructed_popn = popn - deaths + int_in - int_out + dom_in - dom_out) %>%
    select(c(col_aggregation, popn = constructed_popn)) %>%
    as.data.frame()

  return(constructed)

}
