get_gss_names <- function(){
  gss_names <- readRDS("Q:/Teams/D&PA/Demography/Projections/R Models/Trend Model/Lookups/district code to name.rds") %>%
    rename(gss_name = district)  %>%
    rbind(data.frame(LAD11CD = "E12000007", gss_name = "London (total)"))
}
