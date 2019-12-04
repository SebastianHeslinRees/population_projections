housing_led_core <- function(start_population = curr_yr_popn,
                             fertility_rates = curr_yr_fertility,
                             mortality_rates = curr_yr_mortality,
                             int_out_flows_rates = curr_yr_int_out,
                             int_in_flows = curr_yr_int_in_flows,
                             domestic_rates = domestic_rates,
                             int_out_method = config_list$int_out_method,
                             npp_constraints = NULL, upc = NULL,
                             borough_constraints,
                             communal_establishment_population,
                             average_household_size,
                             development_trajectory,
                             dwelling2household_ratio,
                             projection_year = projection_year){

  #1. Run the trend model for one year####

  trend_population <- trend_core(start_population,
                                 fertility_rates, mortality_rates,
                                 int_out_flows_rates, int_in_flows,
                                 domestic_rates,
                                 int_out_method,
                                 constraints = npp_constraints,
                                 upc = NULL,
                                 projection_year)


  #2. Calculate household population

  #3. Calculate trend AHS

  #4. AHS Decision Tree

  #5. Target population

  #6. Constrain births, deaths & international

  #7. Add components from step 6 to domestic from step 1 & start population

  #8. Compare population from step 7 to target from step 5. Difference = domestic adjustment

  #9. Adjust domestic

  #10. Add components from step 6 to domestic from step 9 & start population

  #11. Constrain total population

  #12. Compare population from step 11 to population from step 10. Difference = domestic adjustment

  #13. Adjust domestic

  return()

}
