#' Write model log file
#'
#' Run loggr::deactivate_log() and then clean up the resulting log file
#'
#' @param log_file_path The path and file name (with extension) of the log file
#' 
#' @import dplyr
#' @importFrom loggr deactivate_log()
#' @importFrom data.table fread fwrite
#' 
#' @export

deactivate_log <- function(log_file_path){
  
  loggr::deactivate_log()
  
  data.table::fread(log_file_path, header = FALSE,
                    sep = "*") %>%
    data.frame() %>% 
    filter(substr(V1,43,80)!="Unable to convert event to a log event") %>% 
    data.table::fwrite(log_file_path, col.names = FALSE, quote=FALSE)
  
}