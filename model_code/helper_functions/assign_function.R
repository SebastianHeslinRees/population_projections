assign_function <- function(filepath) {
  
  # Add checks here to make sure that the file being sourced contains one function and one function only
  
  function_file <- readLines(filepath, warn = FALSE)
  function_file <- function_file[!grepl("^library\\(",function_file)]
  function_file <- function_file[!grepl("^require\\(",function_file)]
  function_file <- function_file[!grepl("^\\s*$",function_file)]
  function_file <- function_file[!grepl("^\\s*#",function_file)]
  
  function_defs <- function_file[grepl("(-\\s.*|-)function\\(",function_file)]
  if (length(function_defs) > 1) warning("load_function() expects only one function in the file being loaded.  More than one function call has been found in the file - this is only OK if they are nested so that there is only one containing function")
  
  last_line <- tail(function_file,1) # should end in } with only whitespace or #asdfasdf afterwards
  
  if (!grepl("}\\s*(#.*)*$",last_line)) stop("There is a problem with the code file.  There should be no code after the closing '}' on the function")
  
  first_line <- function_file[[1]] # should start with a function def
  if (!grepl("(-\\s.*|-)function\\(",first_line)) stop("There is a problem with the code file.  There should be no code except library calls before the function is defined")
  
  x <- lsf.str(envir = .GlobalEnv)
 
  my_fun <- source(filepath)[[1]]
  
  rm(list = setdiff(lsf.str(envir = .GlobalEnv), x), envir = .GlobalEnv)
  
  return(my_fun)
} 
