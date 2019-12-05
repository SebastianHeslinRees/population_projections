#TODO Document
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
  #TODO make felxible so that it can take whatever components dataframes you give it
  #it can then be used in the trend core at lines 158-178
  
  constructed <- start_population %>%
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