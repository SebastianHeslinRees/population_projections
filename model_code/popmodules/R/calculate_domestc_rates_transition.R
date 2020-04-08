calculate_domestic_rates_transition <- function(dom_origin_destination_path,
                                                popn_mye_path,
                                                births_mye_path,
                                                years_backseries = 2002:2018,
                                                col_partial_match = c("gss_out","gss_in"),
                                                col_aggregation = c("year","gss_code"="gss_out","gss_in","sex","age"),
                                                col_component = "value",
                                                rate_cap = 0.8,
                                                col_rate = "rate",
                                                dom_mig_last_data_year_initial,
                                                dom_mig_years_to_avg_initial,
                                                dom_mig_last_data_year_longterm=NULL,
                                                dom_mig_years_to_avg_longterm=NULL,
                                                domestic_transition_yr=NULL){
  
  domestic_rate_backseries <- get_rate_backseries(component_mye_path = dom_origin_destination_path,
                                                  popn_mye_path = popn_mye_path,
                                                  births_mye_path = births_mye_path,
                                                  years_backseries = years_backseries,
                                                  col_partial_match = col_partial_match,
                                                  col_aggregation = col_aggregation,
                                                  col_component = col_component,
                                                  rate_cap = NULL)
  
  domestic_rates <- list(initial = calculate_mean_domestic_rates(domestic_rate_backseries,
                                                                 last_data_year = dom_mig_last_data_year_initial,
                                                                 n_years_to_avg = dom_mig_years_to_avg_initial,
                                                                 col_rate = col_rate,
                                                                 rate_cap = rate_cap))
  if(!is.null(domestic_transition_yr)){
    domestic_rates[['longterm']] <- calculate_mean_domestic_rates(domestic_rate_backseries,
                                                                  last_data_year = dom_mig_last_data_year_longterm,
                                                                  n_years_to_avg = dom_mig_years_to_avg_longterm,
                                                                  col_rate = col_rate,
                                                                  rate_cap = rate_cap)
  }
  
  return(domestic_rates)
  
}