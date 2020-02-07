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

create_excels <- function(dir, stamp, file_name){

  datastore_outputs(population = readRDS(paste0(dir,"population_",stamp,".rds")),
                    births = readRDS(paste0(dir,"births_",stamp,".rds")),
                    deaths = readRDS(paste0(dir,"deaths_",stamp,".rds")),
                    int_in = readRDS(paste0(dir,"int_in_",stamp,".rds")),
                    int_out = readRDS(paste0(dir,"int_out_",stamp,".rds")),
                    dom_in = readRDS(paste0(dir,"dom_in_",stamp,".rds")),
                    dom_out = readRDS(paste0(dir,"dom_out_",stamp,".rds")),
                    output_dir = dir,
                    file_name = file_name,
                    output_date = stamp,
                    write_excel = T)

  for(model in c("ons","dclg")){
    household_model_outputs(model_output = readRDS(paste0(dir,model,"_households_",stamp,".rds")),
                            model = model, output_dir = dir, timestamp = stamp, write_excel = T)
  }

}

