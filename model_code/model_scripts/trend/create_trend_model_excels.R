#' Produce 3 Excel workbooks from a set of projection RDS files
#'
#' If a projection has been run with the \code{write_excel} variable set to
#' \code{FALSE} and the Excel outputs are subsequently required this function
#' will produce those workbooks. Creates a population workbook, an ONS household
#' workbook and a DCLG household workbook.
#'
#' @param dir String. The output directory where the projection RDS files are
#'   saved.
#' @param stamp String. The time stamp on the output RDS file names format
#'   \code{"\%y-\%m-\%d_\%H\%M"}.
#' @param file_name String. File name for the population workbook ouput. Must
#'   include \code{.xslx} suffix.
#'
#' @examples
#' \dontrun{
#' create_excels(dir = "outputs/trend/2018/2018_long/",
#'   stamp = "19-11-13_2144",
#'   file_name = "2018_long_trend.xlsx")
#' }
#' @export

create_trend_model_excels <- function(trend_dir, excel_file_name){

  trend_datastore_outputs(population = readRDS(paste0(dir,"population.rds")),
                    births = readRDS(paste0(dir,"births.rds")),
                    deaths = readRDS(paste0(dir,"deaths.rds")),
                    int_in = readRDS(paste0(dir,"int_in.rds")),
                    int_out = readRDS(paste0(dir,"int_out.rds")),
                    dom_in = readRDS(paste0(dir,"dom_in.rds")),
                    dom_out = readRDS(paste0(dir,"dom_out.rds")),
                    output_dir = trend_dir,
                    excel_file_name = excel_file_name,
                    output_date = output_date,
                    write_excel = T)

  for(model in c("ons","dclg")){
    household_model_outputs(model_output = readRDS(paste0(trend_dir, model, "_households.rds")),
                            model = model, output_dir = trend_dir, write_excel = T)
  }

}