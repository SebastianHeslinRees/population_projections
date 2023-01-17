library(dplyr)
library(ipfp)
library(mipfp)
library(reshape2)
library(data.table)
library(foreach)
library(parallel)
library(doParallel)
library(assertthat)

create_od_data <- function(od_data, tgt_in, tgt_out, parallel = TRUE){
  
  tgt_inflow_df <- tgt_in %>% 
    rename(gss_in = gss_code) %>% 
    select(gss_in, sex, age, value) %>% 
    arrange(sex, age, gss_in) %>% 
    data.frame() %>% 
    mutate(value = ifelse(value == 0, 0.01, value))
  
  tgt_outflow_df <- tgt_out %>% 
    rename(gss_out = gss_code) %>% 
    select(gss_out, sex, age, value) %>% 
    arrange(sex, age, gss_out) %>% 
    data.frame() %>% 
    mutate(value = ifelse(value == 0, 0.01, value))
  
  #-------------------------------------------------------------------------------
  
  if(parallel){
    
    n_cores <- detectCores()
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    n <- floor(182/n_cores)
    a <- list()
    for(i in 1:(n_cores-1)){ a[[i]] <- ((i*n)-(n-1)):(i*n) }
    a[[n_cores]] <- ((n_cores*n)-(n-1)):182
    
    pkgs <- c("dplyr","data.table","reshape2","ipfp","mipfp","tidyr")
    message(paste("running on", n_cores, "cores"))
    
    optimised_flows <- foreach(i = 1:n_cores, .combine=bind_rows, .packages=pkgs, .export = "ipf_function") %dopar% { 
      
      x <- list()
      
      for(j in a[[i]]){
        fit_age <- ifelse(j <= 91, j-1, j-92)
        fit_sex <- ifelse(j <= 91, "female", "male")
        
        fit_data <- filter(od_data, age == fit_age, sex == fit_sex)
        fit_inflows <- filter(tgt_inflow_df, age == fit_age, sex == fit_sex)
        fit_outflows <- filter(tgt_outflow_df, age == fit_age, sex == fit_sex)
        
        x[[j]] <- ipf_function(fit_data, fit_inflows, fit_outflows)
      }
      
      bind_rows(x) %>% 
        mutate(age = as.numeric(age))
      
    }
  } else {
    
    message("running in series")
    
    x <- list()
    
    for(j in 1:182){
      
      fit_age <- ifelse(j <= 91, j-1, j-92)
      fit_sex <- ifelse(j <= 91, "female", "male")
      
      message(paste(fit_sex, fit_age))
      
      fit_data <- filter(od_data, age == fit_age, sex == fit_sex)
      fit_inflows <- filter(tgt_inflow_df, age == fit_age, sex == fit_sex)
      fit_outflows <- filter(tgt_outflow_df, age == fit_age, sex == fit_sex)
      
      x[[j]] <- ipf_function(fit_data, fit_inflows, fit_outflows)
    }
    
    optimised_flows <- bind_rows(x) %>% 
      mutate(age = as.numeric(age))
    
  }
  
  return(optimised_flows)
  
}

#---

#Function to fit with IPF
# 
# .create_target_dimension_list <- function(tgt_vars, seed_dims){
#   
#   tgt.dim.list <- lapply(tgt_vars, function(x) sort(match(x, seed_dims)))
#   
#   return(tgt.dim.list)
# }

ipf_function <- function(od_data, fit_inflows, fit_outflows){
  
  #target inflows
  tgt_inflow_formula <- "gss_in ~ age ~ sex"
  
  target_inflow <- acast(data = fit_inflows,
                         formula = as.formula(tgt_inflow_formula),
                         value.var = "value",
                         fill = 0)
  #browser()
  
  #target outflows
  
  total_inflows <- fit_inflows %>%
    group_by(age, sex) %>%
    summarise(inflow = sum(value), .groups = 'drop_last') %>%
    ungroup()
  
  total_outflows <- fit_outflows %>%
    group_by(age, sex) %>%
    summarise(outflow = sum(value), .groups = 'drop_last') %>%
    ungroup()
  
  #---
  
  #ensure target totals match adjusting the largest values in the outflows
  
  tgt_adjustment <- total_inflows %>%
    left_join(total_outflows, by = c("age", "sex")) %>%
    mutate(value = inflow - outflow) %>%
    select(-inflow, -outflow) %>%
    pull(value)
  
  adjust_la <- fit_outflows %>% 
    arrange(-value) %>% 
    group_by(sex, age) %>% 
    mutate(cumsum = cumsum(value))
  
  min_value <- filter(adjust_la, cumsum > abs(tgt_adjustment)) %>% 
    filter(value == max(value)) %>% 
    pull(value)
  
  adjust_la <- filter(adjust_la, value >= min_value) %>% 
    mutate(value = tgt_adjustment * (value/sum(value))) %>% 
    select(-cumsum) %>% 
    data.frame()
  
  adjusted_outflow <- fit_outflows %>%
    bind_rows(adjust_la) %>%
    group_by(gss_out, age, sex) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>%
    ungroup() %>%
    arrange(sex, age, gss_out)
  
  #if(!all(adjusted_outflow$value >= 0)){browser()}
  
  #---
  
  tgt_outflow_formula <- "gss_out ~ age ~ sex"
  
  target_outflow <- acast(data = adjusted_outflow,
                          formula = as.formula(tgt_outflow_formula),
                          value.var = "value",
                          fill = 0)
  
  
  #---
  
  #create list of target data and also list defining relationships to contingency table dimensions
  
  target_data <- list("outflows" = target_outflow,
                      "inflows" = target_inflow)
  
  target_dimension_list <- list("outflows" = c(1, 3, 4),
                                "inflows" = c(2, 3, 4))
  
  
  #next create seed table
  seed_df <- od_data %>%
    mutate(age = as.character(age), sex = as.character(sex)) %>%
    select(gss_out, gss_in, age, sex, value) %>%
    complete(gss_out=unique(fit_inflows$gss_in), gss_in=unique(fit_inflows$gss_in),
             age, sex, fill = list(value = 0)) %>%
    arrange(gss_out, gss_in, age, sex)
  
  seed_formula <- "gss_out ~ gss_in ~ age ~ sex"
  
  seed_table <- acast(data = seed_df,
                      formula = as.formula(seed_formula),
                      value.var = "value",
                      fill = 0.01)
  
  outputs.ipfp <- Estimate(seed = seed_table, 
                           target.list = target_dimension_list, 
                           target.data = target_data, 
                           method = "ipfp", 
                           iter = 20000) 
  
  results <- reshape2::melt(outputs.ipfp$x.hat, value.name = "value") %>%
    mutate_at(vars(-value), funs(as.character)) %>%
    filter(value > 0) %>% 
    setnames(names(seed_df))
  
  return(results)
  
}
