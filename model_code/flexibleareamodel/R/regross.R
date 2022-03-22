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
  
  mig_target <- left_join(base_in, base_out, by = base_flows_cols) %>%
    group_by(across(common_cols)) %>% 
    summarise(in_base = sum(in_base),
              out_base = sum(out_base)) %>% 
    data.frame() %>% 
    left_join(target_net, by = common_cols) %>% 
    select(c(target_cols, in_base, out_base, net_target))
  
  optimise_gross_flows_vec <- Vectorize(.optimise_gross_flows, SIMPLIFY = TRUE)

  optimised_flows <- mig_target %>%
    mutate(model_flows = optimise_gross_flows_vec(in_base, out_base, net_target)) %>%
    unnest_wider(col = model_flows)

  return(optimised_flows)
  
}

.optimise_gross_flows <- function(base_in, base_out, target_net) {
  
  base_out <- abs(round(base_out, 0))
  base_in <- round(base_in, 0)
  target_net = round(target_net, 0)
  
  chk_max <- base_in + abs(target_net) + base_out
  
  in_tst <- seq(0, chk_max, by = 1)
  
  tst_df <- as.data.frame(in_tst) %>%
    mutate(out_tst = in_tst - target_net) %>%
    filter(out_tst >= 0) %>%
    mutate(p_in = 1000 * dpois(in_tst, base_in)) %>%
    mutate(p_out = 1000 * dpois(out_tst, base_out)) %>%
    mutate(p_combo = p_in * p_out) %>%
    slice(which.max(p_combo))
  
  c_out <- list(c("inflow" = tst_df$in_tst[1], "outflow" = tst_df$out_tst[1]))
  
  return(c_out)
}




