get_constraints <- function(constraint_list, last_proj_yr){
  
  if(constraint_list$components$births){
    constraint_list$births_constraint <- get_component_from_file(paste0(constraint_list$constraint_path,"births.rds"),
                                                                 last_proj_yr) 
  }
  
  if(constraint_list$components$deaths){
    constraint_list$deaths_constraint <- get_component_from_file(paste0(constraint_list$constraint_path,"deaths.rds"),
                                                                 last_proj_yr) 
  }
  
  if(constraint_list$components$in_migration){
    constraint_list$in_migration_constraint <- rbind(
      get_component_from_file(paste0(constraint_list$constraint_path,"int_in.rds"), last_proj_yr) %>%
        rename(inflow = int_in),
      get_component_from_file(paste0(constraint_list$constraint_path,"dom_in.rds"), last_proj_yr) %>%
        rename(inflow = dom_in)) %>% 
      group_by(year, gss_code, sex, age) %>% 
      summarise(inflow = sum(inflow),
                .groups = 'drop_last') %>% 
      data.frame()
  }
  
  if(constraint_list$components$births){
    constraint_list$out_migration_constraint <- rbind(
      get_component_from_file(paste0(constraint_list$constraint_path,"int_out.rds"), last_proj_yr) %>%
        rename(outflow = int_out),
      get_component_from_file(paste0(constraint_list$constraint_path,"dom_out.rds"), last_proj_yr) %>%
        rename(outflow = dom_out)) %>% 
      group_by(year, gss_code, sex, age) %>% 
      summarise(outflow = sum(outflow),
                .groups = 'drop_last') %>% 
      data.frame()
  }
  
  if(constraint_list$components$population){
    constraint_list$population_constraint <- get_component_from_file(paste0(constraint_list$constraint_path,"population.rds"), last_proj_yr) 
  }
  
  return(constraint_list)
}
