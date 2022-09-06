#' Regrosser
#' 
#' @param base_in Dataframe
#' @param base_out Dataframe
#' @param target_net Dataframe
#' @param col_inflow String
#' @param col_outflow String
#' @param col_target String
#' 
#' @import dplyr
#' @importFrom data.table rbindlist
#' @import dtplyr
#' 
#' @export

regross <- function(base_in, base_out, target_net,
                    col_inflow = "inflow",
                    col_outflow = "outflow",
                    col_target = "net_target"){
  
  target_cols <- setdiff(names(target_net), col_target)
  base_flows_cols <- intersect(names(base_in), names(base_out))
  common_cols <- intersect(target_cols, base_flows_cols)
  base_in <- rename(base_in, in_base = !!col_inflow)
  base_out <- rename(base_out, out_base = !!col_outflow)
  target_net <- rename(target_net, net_target = !!col_target)
  
  mig_target <- lazy_dt(base_in) %>%
    left_join(base_out, by = base_flows_cols) %>%
    group_by(across(!!common_cols)) %>%
    summarise(in_base = sum(in_base),
              out_base = sum(out_base),
              .groups = 'drop_last') %>%
    left_join(target_net, by = common_cols) %>%
    select(c(target_cols, in_base, out_base, net_target)) 
  
  optimise_gross_flows_vec <- Vectorize(.optimise_gross_flows, SIMPLIFY = TRUE)
  
  optimised_flows <- mig_target %>%
    mutate(model_flows = optimise_gross_flows_vec(in_base, out_base, net_target)) %>%
    data.frame() %>%
    unnest_wider(col = model_flows)
  
  return(optimised_flows)
  
}

#' Regrosser in paralell
#' 
#' @param base_in Dataframe
#' @param base_out Dataframe
#' @param target_net Dataframe
#' @param col_inflow String
#' @param col_outflow String
#' @param col_target String
#' @param n_cores Numeric. Number of cores
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom data.table rbindlist
#' @import foreach
#' @import dtplyr
#' 
#' @export

regross_parallel <- function(base_in, base_out, target_net,
                             col_inflow = "inflow",
                             col_outflow = "outflow",
                             col_target = "net_target",
                             n_cores = 8,
                             fun = 2){
  
  target_cols <- setdiff(names(target_net), col_target)
  base_flows_cols <- intersect(names(base_in), names(base_out))
  common_cols <- intersect(target_cols, base_flows_cols)
  base_in <- rename(base_in, in_base = !!col_inflow)
  base_out <- rename(base_out, out_base = !!col_outflow)
  target_net <- rename(target_net, net_target = !!col_target)
  
  mig_target <- lazy_dt(base_in) %>%
    left_join(base_out, by = base_flows_cols) %>%
    group_by(across(!!common_cols)) %>%
    summarise(in_base = sum(in_base),
              out_base = sum(out_base),
              .groups = 'drop_last') %>%
    left_join(target_net, by = common_cols) %>%
    select(c(target_cols, in_base, out_base, net_target)) %>% 
    data.frame()
  
  if(fun==1){
    optimise_gross_flows_vec <- Vectorize(.optimise_gross_flows, SIMPLIFY = TRUE)
  } else {
    optimise_gross_flows_vec <- Vectorize(.optimise_gross_flows_2, SIMPLIFY = TRUE)
  }
  
  n <- floor(nrow(mig_target)/n_cores)
  a <- list()
  for(i in 1:(n_cores-1)){ a[[i]] <- ((i*n)-(n-1)):(i*n) }
  a[[n_cores]] <- ((n_cores*n)-(n-1)):nrow(mig_target)
  
  optimised_flows <- foreach(i = 1:n_cores, .combine=bind_rows, .packages=c("dplyr")) %dopar% { 
    mig_target[a[[i]],] %>%
      mutate(model_flows = optimise_gross_flows_vec(in_base, out_base, net_target))
  } 
  
  optimised_flows <- unnest_wider(optimised_flows, col = model_flows)
  
  return(optimised_flows)
  
}

#' Optimise gross flows using Poisson distributions
#' 
#' @param base_in Dataframe
#' @param base_out Dataframe
#' @param target_net Dataframe
#' 
#' @import dtplyr
#' @import dplyr

.optimise_gross_flows <- function(base_in, base_out, target_net) {
  
  base_out <- abs(round(base_out, 0))
  base_in <- round(base_in, 0)
  target_net = round(target_net, 0)
  
  chk_max <- base_in + abs(target_net) + base_out
  
  in_tst <- seq(0, chk_max, by = 1)
  
  tst_df <- lazy_dt(in_tst) %>% 
    mutate(out_tst = x - target_net) %>% 
    filter(out_tst >= 0) %>% 
    mutate(p_in = dpois(x, base_in),
           p_out = dpois(out_tst, base_out),
           p_combo = p_in * p_out) %>% 
    slice(which.max(p_combo))  %>%
    as_tibble()
  
  c_out <- list(c("inflow" = tst_df$x[1], "outflow" = tst_df$out_tst[1]))
  
  return(c_out)
}


#' Optimise gross flows using Poisson distributions. Faster implementation.
#' 
#' @param base_in Dataframe
#' @param base_out Dataframe
#' @param target_net Dataframe
#' @param jump_scale Numeric. Lower values of jump scale should be faster, but
#'  potentially less reliable. Always use a value greater than 1. Default = 4.
#' 
#' @import dplyr

.optimise_gross_flows_2 <- function(base_in, base_out, target_net, jump_scale = 4) {
  
  base_out <- abs(round(base_out, 0))
  base_in <- abs(round(base_in, 0))
  base_net <- base_in - base_out
  target_net_rounded = round(target_net, 0)
  change_net = target_net_rounded - base_net
  
  max_iterations <- abs(change_net)
  
  new_in <- base_in
  new_out <- base_out
  
  j <- 1
  while((floor(abs(target_net_rounded - (new_in - new_out))) > 0) & (j < max_iterations)){
    
    distance_from_target <- target_net_rounded - (new_in - new_out)
    direction_to_target <- distance_from_target/abs(distance_from_target)
    int_adjust <- direction_to_target * ceiling(abs(distance_from_target/jump_scale))
    
    p_in_adjust <- dpois(new_in + int_adjust, base_in, log = TRUE) + dpois(new_out, base_out, log = TRUE)
    p_out_adjust <- dpois(new_in, base_in, log = TRUE) + dpois(new_out - int_adjust, base_out, log = TRUE)
    
    if(p_in_adjust > p_out_adjust) {
      new_in <- new_in + int_adjust
    } else {
      new_out <- new_out - int_adjust
    }
    
    j <- j + 1
  }
  
  #Add the residual to the flow that has changed most from the base
  target_net_residual <- new_in - new_out - target_net 
  
  x <- abs(new_in-base_in)/base_in > abs(new_out-base_out)/base_out
  x <- ifelse(is.na(x), FALSE, x)
  
  if(x){
    while(new_in - target_net_residual < 0){
      new_in <- new_in + 0.1
      new_out <- new_out + 0.1
    } 
    new_in <- new_in - target_net_residual
  } else {
    while(new_out + target_net_residual < 0){
      new_in <- new_in + 0.1
      new_out <- new_out + 0.1
    }
    new_out <- new_out + target_net_residual
  }
  
  c_out <- list(c("inflow" = new_in, "outflow" = new_out))
  
  return(c_out)
  
}

