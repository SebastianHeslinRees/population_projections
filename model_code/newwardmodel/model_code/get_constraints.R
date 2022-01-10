#' Read in constraint data a place in a list for use in the small area model
#'
#' The constraint_list parameter has 3 elements: the location of a LA-level trend
#' projection; a mapping parameter which maps between the constraint data and the
#' projection data; a list of which components to constrain. The function returns
#' a list of data frames containing the constraint data.
#' 
#' @param constraint_list List with 3 elements.
#'  1: constraint_path: String. The path to the output folder of an LA-level
#'  projection run using the GLA trend model
#'  2: mapping: Character. A vector of column names common to both the constraint
#'  and the projection. E.g. c("gss_code","year","sex","age")
#'  3: constraint_list: A list of logical values indicating whether or not each
#'  component should be constrained. The list must include the following components:
#'  births, deaths, in_migration, out_migration, population)
#' @param last_proj_yr Numeric. The final projection year.
#' 
#' @return A list where each element is a data frame containing constraint data.
#'
#' @import popmodules
#' 
#' @export
#'
#' @example 
#' get_constraints(
#'   constraint_path = list(constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259/",
#'   mapping = c("gss_code","year","sex","age"),
#'   components = list(births = TRUE,
#'                     deaths = TRUE,
#'                     in_migration = FALSE,
#'                     out_migration = FALSE,
#'                     population = FALSE))

get_constraints <- function(constraint_list, last_proj_yr){
  
  path <- constraint_list$constraint_path
  
  if(constraint_list$components$births){
    constraint_list$births_constraint <- get_component_from_file(paste0(path,"births.rds"),
                                                                 last_proj_yr) 
  }
  
  if(constraint_list$components$deaths){
    constraint_list$deaths_constraint <- get_component_from_file(paste0(path,"deaths.rds"),
                                                                 last_proj_yr) 
  }
  
  if(constraint_list$components$in_migration){
    constraint_list$in_migration_constraint <- rbind(
      get_component_from_file(paste0(path,"int_in.rds"), last_proj_yr) %>%
        rename(inflow = int_in),
      get_component_from_file(paste0(path,"dom_in.rds"), last_proj_yr) %>%
        rename(inflow = dom_in)) %>% 
      group_by(year, gss_code, sex, age) %>% 
      summarise(inflow = sum(inflow),
                .groups = 'drop_last') %>% 
      data.frame()
  }
  
  if(constraint_list$components$births){
    constraint_list$out_migration_constraint <- rbind(
      get_component_from_file(paste0(path,"int_out.rds"), last_proj_yr) %>%
        rename(outflow = int_out),
      get_component_from_file(paste0(path,"dom_out.rds"), last_proj_yr) %>%
        rename(outflow = dom_out)) %>% 
      group_by(year, gss_code, sex, age) %>% 
      summarise(outflow = sum(outflow),
                .groups = 'drop_last') %>% 
      data.frame()
  }
  
  if(constraint_list$components$population){
    constraint_list$population_constraint <- get_component_from_file(paste0(path,"population.rds"),
                                                                     last_proj_yr) 
  }
  
  return(constraint_list)
}
