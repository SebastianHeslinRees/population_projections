#' Produce 3 Excel workbooks from a set of projection RDS files
#'
#' If a projection has been run with the \code{write_excel} variable set to
#' \code{FALSE} and the Excel outputs are subsequently required this function
#' will produce those workbooks. Creates a population workbook, an ONS household
#' workbook and a DCLG household workbook.
#'
#' @param trend_dir String. The output directory where the projection RDS files are
#'   saved.
#' @param excel_file_name String. File name for the population workbook output.
#'  With or without "xlsx" suffix.
#' @examples
#' \dontrun{
#' create_trend_model_excels(
#'   trend_dir = "outputs/trend/2018/2018_long_19-11-13_2144",
#'   excel_file_name = "2018_long_trend.xlsx")
#' }
#' 
#' @export

create_trend_model_excels <- function(trend_dir, excel_file_name, household_models = TRUE){
  
  if(substr(excel_file_name, nchar(excel_file_name)-4, nchar(excel_file_name)) != ".xlsx"){
    excel_file_name <- paste0(excel_file_name,".xlsx")
  }
  
  trend_datastore_outputs(population = readRDS(paste0(trend_dir,"population.rds")),
                          births = readRDS(paste0(trend_dir,"births.rds")),
                          deaths = readRDS(paste0(trend_dir,"deaths.rds")),
                          int_in = readRDS(paste0(trend_dir,"int_in.rds")),
                          int_out = readRDS(paste0(trend_dir,"int_out.rds")),
                          dom_in = readRDS(paste0(trend_dir,"dom_in.rds")),
                          dom_out = readRDS(paste0(trend_dir,"dom_out.rds")),
                          popn_adjustment =  readRDS(paste0(trend_dir,"popn_adjustment.rds")),
                          output_dir = trend_dir,
                          excel_file_name = excel_file_name)
  
  if(household_models){
    for(model in c("ons","dclg")){
      household_model_outputs(model_output = readRDS(paste0(trend_dir, "households/", model, "_households.rds")),
                              model = model, output_dir = trend_dir, write_excel = T)
    }
  }
  
}
