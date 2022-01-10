#' Read in constraint data and place in a list for use in the small area model
#'
#' The constraint_list parameter has 3 elements: 1. the location of a LA-level trend
#' projection; 2. a mapping parameter which maps between the constraint data and the
#' projection data; 3. a list indicating components to constrain. The function returns
#' a list of data frames each containing component constraint data.
#' 
#' @param constraint_list List with 3 elements.
#'  1: constraint_path: String. The path to the output folder of an LA-level
#'  projection run using the GLA trend model
#'  2: mapping: Character. A vector of column names common to both the constraint
#'  and the projection. This level at which the constraint is applied.
#'  E.g. c("gss_code","year","sex","age")
#'  3: components: A list of logical values indicating whether or not each
#'  component should be constrained. The list must include the following components:
#'  births, deaths, in_migration, out_migration, population
#' @param last_proj_yr Numeric. The final projection year.
#' 
#' @return A list where each element is a data frame containing constraint data.
#' 
#' @import dplyr
#' @import assertthat
#' 
#' @export
#'
#' @examples 
#' \dontrun{
#' get_constraints(list(
#'   constraint_path = "outputs/trend/2020/2020_CH_central_lower_21-09-21_1259",
#'   mapping = c("gss_code","year","sex","age"),
#'   components = list(births = TRUE,
#'                     deaths = TRUE,
#'                     in_migration = FALSE,
#'                     out_migration = FALSE,
#'                     population = FALSE)))
#' }

get_constraints <- function(constraint_list, last_proj_yr){
  
  path <- .add_slash(constraint_list$constraint_path)
  
  validate_get_constraints(constraint_list, last_proj_yr)
  
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

validate_get_constraints <- function(constraint_list, last_proj_yr){
  
  assert_that(is.list(constraint_list), msg = "In get_constraints: constraint_list parameter must be a list")
  assert_that(is.number(last_proj_yr), msg = "In get_constraints: constraint_list parameter must be a nummber")
  
  assert_that(setequal(names(constraint_list), c("constraint_path", "mapping", "components")),
              msg = "In get_constraints: names(constraint_list) must be: constraint_path, mapping, components")
  
  assert_that(is.string(constraint_list$constraint_path), msg = "In get_constraints: constraint_list$constraint_path parameter must be a string")
  assert_that(is.character(constraint_list$mapping), msg = "In get_constraints: constraint_list parameter must be a character vector")
  assert_that(is.list(constraint_list$components), msg = "In get_constraints: constraint_list$components parameter must be a list")
  
  assert_that(dir.exists(constraint_list$constraint_path), msg = "In get_constraints: constraint_list$constraint_path directory does not exist")
  
  assert_that(setequal(names(constraint_list$components), c("births", "deaths", "in_migration", "out_migration", "population")),
              msg = "In get_constraints: names(constraint_list$components) must be:  births, deaths, in_migration, out_migration, population")
  
  assert_that(is.logical(constraint_list$components$births), msg = "In get_constraints: constraint_list$components$births parameter must be logical")
  assert_that(is.logical(constraint_list$components$deaths), msg = "In get_constraints: constraint_list$components$deaths parameter must be logical")
  assert_that(is.logical(constraint_list$components$in_migration), msg = "In get_constraints: constraint_list$components$in_migration parameter must be logical")
  assert_that(is.logical(constraint_list$components$out_migration), msg = "In get_constraints: constraint_list$components$out_migration parameter must be logical")
  assert_that(is.logical(constraint_list$components$population), msg = "In get_constraints: constraint_list$components$population parameter must be logical")
  
  constraint_list$constraint_path <- .add_slash(constraint_list$constraint_path)
  
  if(constraint_list$components$births){
    assert_that(file.exists(paste0(constraint_list$constraint_path, "births.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "births.rds file does not exist"))
  }
  
  if(constraint_list$components$deaths){
    assert_that(file.exists(paste0(constraint_list$constraint_path, "deaths.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "deaths.rds file does not exist"))
  }
  
  if(constraint_list$components$in_migration){
    assert_that(file.exists(paste0(constraint_list$constraint_path, "int_in.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "int_in.rds file does not exist"))
    
    assert_that(file.exists(paste0(constraint_list$constraint_path, "dom_in.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "dom_in.rds file does not exist"))
    
  }
  
  if(constraint_list$components$out_migration){
    assert_that(file.exists(paste0(constraint_list$constraint_path, "int_out.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "int_out.rds file does not exist"))
    
    assert_that(file.exists(paste0(constraint_list$constraint_path, "dom_out.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "dom_out.rds file does not exist"))
    
  }
  
  if(constraint_list$components$population){
    assert_that(file.exists(paste0(constraint_list$constraint_path, "population.rds")),
                msg = paste0("In get_constraints: ", constraint_list$constraint_path, "npopulation.rds file does not exist"))
  }
  
  invisible()
}
