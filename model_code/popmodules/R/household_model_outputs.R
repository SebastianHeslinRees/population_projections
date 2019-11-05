
ons_household_model(population, hh_rep_rates_path, communal_est_pop_path)

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