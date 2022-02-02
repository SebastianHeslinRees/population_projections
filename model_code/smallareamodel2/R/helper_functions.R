#' Check that paths in a config list point to actual rds files
#' 
#' Given a set of input parameters run a housing-led for small areas
#' 
#' @param config_list A List. A model configuration list.
#' 
#' @import stringr
#'  
#' @export

.validate_input_paths <- function(config_list){
  
  paths <- names(config_list)[str_detect(names(config_list), "path")]
  
  paths <- config_list[names(config_list) %in% paths]
  
  lapply(seq(paths),
         function(i) {
           if(!is.null(paths[[i]])){
             assert_that(file.exists(paths[[i]]),
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(paths[[i]], split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile is not .rds format"))
           }
         })
  
  if("out_migration" %in% names(config_list)){
    lapply(config_list$out_migration,
           function(x) {
             assert_that(file.exists(x$path),
                         msg = paste0(x$path, "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(x$path, "\nFile is not .rds format"))
           })
  }
  
  if("in_migration" %in% names(config_list)){
    lapply(config_list$in_migration,
           function(x) {
             assert_that(file.exists(x$path),
                         msg = paste0(x$path, "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(x$path, "\nFile is not .rds format"))
           })
  }
  
  if("constraint_list" %in% names(config_list) & !is.null(constraint_list$constraint_path)){
    assert_that(dir.exists(config_list$constraint_list$constraint_path),
                msg = "constraint folder does not exist at specified path")
  }
  
  
  invisible()
}