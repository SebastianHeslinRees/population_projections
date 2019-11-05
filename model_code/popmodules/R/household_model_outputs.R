
#ons_household_model(population, hh_rep_rates_path, communal_est_pop_path)

# dclg_outputs <- function(households, stage_1, household_population, institutional_population,
#                          households_regional, stage_2, AHS, output_location){
#   ### Output files
#   setwd(output_location)
#
#   fwrite(households, "Households - district Totals.csv", quote = TRUE)
#   fwrite(households_regional, "Households - Regional Totals.csv", quote = TRUE)
#
#   fwrite(household_population, "Household population.csv", quote = TRUE)
#   fwrite(institutional_population, "Institutional population.csv", quote = TRUE)
#
#   fwrite(stage1, "Households - Stage 1.csv", quote = TRUE)
#   fwrite(stage2, "Households - Stage 2.csv", quote = TRUE)
#   fwrite(AHS, "Households - AHS.csv", quote = TRUE)
#
#
# }

ons_hh_model_outputs <- function(model_output, output_dir){
  
  #Deatiled outputs
  detailed_unconstrained <- model_output$unconstrained %>%
    pivot_wider(names_from = year, values_from = households)
  
  detailed_constrained <- model_output$constrained %>%
    pivot_wider(names_from = year, values_from = households)
  
  detailed_household_pop <- model_output$household_population %>%
    pivot_wider(names_from = year, values_from = household_popn)
  
  detailed_ce_pop <- model_output$communal_establishment_population %>%
    pivot_wider(names_from = year, values_from = ce_pop)
  
  
  #Summary outputs
  data_totals <- function(data, in_col, out_col){
    x <- group_by(data, gss_code, year) %>%
      summarise(!!out_col := sum(!!sym(in_col)))
  }
  
  total_ce_pop <- data_totals(model_output$communal_establishment_population,
                              "ce_pop", "communal_establishment_popualtion")
  total_hh_pop <- data_totals(model_output$household_population,
                              "household_popn", "household_population")
  total_constrained <- data_totals(model_output$constrained,
                                   "households", "constrained_households")
  total_unconstrained <- data_totals(model_output$unconstrained,
                                     "households", "unconstrained_households")
  
  household_summary_sheet <- left_join(total_unconstrained, total_constrained, by = c("gss_code", "year")) %>%
    left_join(total_hh_pop, by = c("gss_code", "year")) %>%
    left_join(total_ce_pop, by = c("gss_code", "year")) %>%
    mutate(averge_household_size = household_population / constrained_households)
  
  
  #write outputs
  #TODO Add LA & region names to outputs
  output_dataframes <- list(detailed_unconstrained=detailed_unconstrained,
                            detailed_constrained=detailed_constrained,
                            detailed_household_pop=detailed_household_pop,
                            detailed_ce_pop=detailed_ce_pop,
                            household_summary_sheet=household_summary_sheet)
  
  lapply(seq_along(output_dataframes),
         function(i) data.table::fwrite(output_dataframes[[i]],
                                        paste0(output_dir, names(output_dataframes)[i], ".csv"))) %>%
    invisible()

}
