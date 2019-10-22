get_data_for_snpp_rate_chain <- function(popn_mye_path,
                                         births_mye_path,
                                         deaths_mye_path=NULL,
                                         fertility_or_mortality){

  validate_snpp_rate_chain_filetype(popn_mye_path, births_mye_path, deaths_mye_path)

  population <- data.frame(readRDS(popn_mye_path))
  births <- data.frame(readRDS(births_mye_path))
  
  if(fertility_or_mortality == "fertility"){
    rate_chain_data <- list(population = population, component_data = births)
  }

  if(fertility_or_mortality == "mortality"){
    deaths <- data.frame(readRDS(deaths_mye_path))
    rate_chain_data <- list(population = list(population, births), component_data = deaths)
  }

  return(rate_chain_data)

}


#---------------------------------

validate_snpp_rate_chain_filetype <- function(path_1, path_2, path_3) {

  for(i in c(path_1, path_2, path_3)){
    filepath <- i
    file_ext <- tolower(strsplit(basename(filepath), split="\\.")[[1]][[2]])

    assertthat::assert_that(file_ext == "rds",
                            msg = paste(i,": file must be a .rds file"))
  }

  invisible(TRUE)

}

#---------------------------------

