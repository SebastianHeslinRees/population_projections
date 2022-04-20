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
    group_by(across(all_of(common_cols))) %>%
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
                             n_cores = 8){
  
  target_cols <- setdiff(names(target_net), col_target)
  base_flows_cols <- intersect(names(base_in), names(base_out))
  common_cols <- intersect(target_cols, base_flows_cols)
  base_in <- rename(base_in, in_base = !!col_inflow)
  base_out <- rename(base_out, out_base = !!col_outflow)
  target_net <- rename(target_net, net_target = !!col_target)
  
  mig_target <- lazy_dt(base_in) %>%
    left_join(base_out, by = base_flows_cols) %>%
    group_by(across(all_of(common_cols))) %>%
    summarise(in_base = sum(in_base),
              out_base = sum(out_base),
              .groups = 'drop_last') %>%
    left_join(target_net, by = common_cols) %>%
    select(c(target_cols, in_base, out_base, net_target)) %>% 
    data.frame()
  
  optimise_gross_flows_vec <- Vectorize(.optimise_gross_flows, SIMPLIFY = TRUE)
  
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

.optimise_gross_flows <- function(base_in, base_out, target_net) {
  
  base_out <- abs(round(base_out, 0))
  base_in <- round(base_in, 0)
  target_net = round(target_net, 0)
  
  chk_max <- base_in + abs(target_net) + base_out
  
  in_tst <- seq(target_net, chk_max, by = 1)
  
  tst_df <- lazy_dt(in_tst) %>% 
    mutate(out_tst = x - target_net,
           p_in = dpois(x, base_in),
           p_out = dpois(out_tst, base_out),
           p_combo = p_in * p_out) %>% 
    slice(which.max(p_combo))  %>%
    as_tibble()
  
  c_out <- list(c("inflow" = tst_df$x[1], "outflow" = tst_df$out_tst[1]))
  
  return(c_out)
}

