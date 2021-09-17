#' Create the 2020-based housing-led projection excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template. London borough data.
#'
#' @param output_dir The directory from which to read the data and save the Excel file
#' @param projection_name The name of the output Excel file
#' @param file_suffix A string to append to the end of the file name
#' 
#' @import dplyr
#' @import reticulate
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#' 
#' @export

create_housingled_excels <- function(output_dir, projection_name, file_suffix = ".xlsx"){
  
  if(substr(output_dir,nchar(output_dir),nchar(output_dir))!="/"){
    output_dir <- paste0(output_dir,"/")
  }
  
  persons <- fread(paste0(output_dir,"csv/persons.csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  females <- fread(paste0(output_dir,"csv/females.csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050))) 
  males <- fread(paste0(output_dir,"csv/males.csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  
  components <- fread(paste0(output_dir,"csv/components.csv"), header = TRUE) %>% 
    as.data.frame() %>% 
    rename(population = popn) %>%
    filter(year <= 2041)
    #TODO Figure out a way of output these as not blank cells but in a way
    #that doesn't coerce the whole column to character
    # mutate(dom_in = ifelse(is.na(dom_in),"#N/A", dom_in),
    #        dom_out = ifelse(is.na(dom_out),"#N/A", dom_out))
  
  names(components) <- str_replace_all(names(components), "_", " ")
  
  assumed_dev <- fread(paste0(output_dir,"csv/assumed_dev.csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  stock <- fread(paste0(output_dir,"csv/housing_stock.csv"), header = TRUE) %>%
    as.data.frame() %>% 
    select(-c(as.character(2042:2050)))
  

  if(substr(file_suffix, nchar(file_suffix)-4, nchar(file_suffix)) != ".xlsx"){
    file_suffix <- paste0(file_suffix,".xlsx")
  }
  
  output_dir <- paste0(output_dir, "excels/")
  dir.create(output_dir, showWarnings = FALSE)
  wb_filename <- paste0(projection_name, file_suffix)
 
  #python excel
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work around is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also trendmodel::create_household_model_excels, etc
  
  try( reticulate::source_python('model_code/other_scripts/python_to_excel_housingled.py'), silent = TRUE)
  reticulate::source_python('model_code/other_scripts/python_to_excel_housingled.py') 
  python_to_excel_housingled(persons, females, males, components, assumed_dev, stock, output_dir, wb_filename)
  
}