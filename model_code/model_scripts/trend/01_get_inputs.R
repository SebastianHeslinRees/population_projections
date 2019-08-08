
get_mye_component <- function(filepath, max_yr) {
  component <- readRDS(filepath) %>%
    filter(year <= max_yr)
  
  # TODO: check that there is data in here
  # TODO: validate MYE component
  # TODO: test that the compontent contains max_yr
  
  return(component)
  
}


evaluate_fns_list <- function(fns_args_list) {
  
  # fns_args_list should contain a list of functions and the arguments each function takes
  # these should be in the order that they should be run and in the format:
  # each function should create or modify the same df, eg. mortality or fertility (called subject here)
  #
  # funs_args_list <- list(
  #   list(fn = function1, args = list(arg1_1, arg1_2, ....)),
  #   list(fn = function2, args = list(arg2_1, arg2_2, ....)),
  #   .....
  # )
  #
  
  
  # run the functions in the chain to get the e.g. mortality rates
  
  ## the first creates an initial mortality dataframe
  subject <- do.call(fns_args_list[[1]]$fn, fns_args_list[[1]]$args)
  n_fns_remaining <- length(fns_args_list) - 1
  
  ## the rest take in mortality as their first argument
  for (i in 1:n_fns_remaining) {
    ind <- i + 1
    
    # add mortality on as the first argument to the function
    # TODO make this less ugly
    n_args <- length(fns_args_list[[ind]]$args)
    all_args <- fns_args_list[[ind]]$args
    all_args[[n_args + 1]] <- subject
    all_args <- all_args[c(n_args + 1, 1:n_args)]
    
    subject <- do.call(fns_args_list[[ind]]$fn, all_args)
    # TODO validate this df
  }
  return(subject)
}
