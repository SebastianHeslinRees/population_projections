#' Create the 2020-based housing-led projection excel output
#' 
#' Process population, components and development data and place it into an
#' Excel template. London borough data.
#'
#' @param output_dir string. The directory from which to read the data and save the Excel file
#' @param wb_filename String. The name of the output Excel file. With or without '.xlsx'
#' @param projection_name String. The projection name.
#' 
#' @import dplyr
#' @import reticulate
#' @import popmodules
#' @import stringr
#' @importFrom data.table fread
#' 
#' @export

create_housingled_excels <- function(output_dir, wb_filename, projection_name){

  message("Creating borough Excel workbook")
  
  output_dir <- .add_slash(output_dir)
  excel_dir <- paste0(output_dir, "excels/")
  dir.create(excel_dir, showWarnings = FALSE)
  
  if(str_sub(wb_filename,-5,-1)!=".xlsx"){
    wb_filename <- paste0(wb_filename,".xlsx")
  }
  
  #-----------------------------------------------------------------------------
  
  persons <- filter_max_2041("persons", output_dir)
  females <- filter_max_2041("females", output_dir)
  males <- filter_max_2041("males", output_dir)
  assumed_dev <- filter_max_2041("assumed_dev", output_dir)
  stock <- filter_max_2041("housing_stock", output_dir)
  
  components <- fread(paste0(output_dir,"csv/components.csv"), header = TRUE) %>%
    as.data.frame() %>%
    rename(population = popn) %>%
    filter(year <= 2041)
  
  #TODO London dom in and out are NA. These get output as blank cells.
  #It would look better in the final sheet with something in the cell.
  #If they are changed to 'NA' or '#N/A' it coerces the whole column
  #to character. 
  # mutate(dom_in = ifelse(is.na(dom_in),"#N/A", dom_in),
  #        dom_out = ifelse(is.na(dom_out),"#N/A", dom_out))
  
  names(components) <- str_replace_all(names(components), "_", " ")

  #-----------------------------------------------------------------------------
  
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
  python_to_excel_housingled(persons, females, males, components, assumed_dev, stock, 
                             excel_dir, wb_filename, projection_name)
  
}

#If the projection horizon is >2041 remove extra years, if not use max(year)
filter_max_2041 <- function(x, output_dir){
  
  a <- fread(paste0(output_dir,"csv/",x,".csv"), header = TRUE) %>%
    as.data.frame()
  
  b <- as.numeric(last(names(a)))
  
  if(b>2041){
    a <- select(a, -c(as.character(2042:b)))
  }
  
  return(a)
  
}