#' Run 3 concurrent BPO projections from one config file
#'
#' Use \code{rstudioapi::jobRunScript} to run 3 migration scenario projections
#' at the same time from a single BPO config script.
#'
#' @param config_name The name of the config file saved in the
#' \code{config_scripts/housing_led/bpo_conigs/} folder
#'
#' @importFrom rstudioapi jobRunScript
#' @export

run_bpo_config_Jobscript <- function(config_name){
  
  for(s in 1:3){
    
    wd <- getwd()
    scenario <<- paste0("scenario_",s) #this has to be in the GlobalEnv
    
    config_path <- paste0(wd,"/config_scripts/housing_led/bpo_configs/",
                          config_name, ".R")
    
    rstudioapi::jobRunScript(path = config_path,
                             name = paste(config_name,scenario),
                             encoding = "unknown",
                             workingDir = wd,
                             importEnv = TRUE,
                             exportEnv = "")
  }
  
}