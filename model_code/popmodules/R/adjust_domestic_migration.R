#TODO Document

adjust_domestic_migration <- function(popn, target, dom_in, dom_out,
                                      col_aggregation = c("year","gss_code"),
                                      col_popn, col_target){
  
  #find diff between popn and target
  col_popn_new <- paste0(col_popn, ".x")
  col_target_new  <- paste0(col_target,  ".y")
  popn <- rename(popn, !!col_popn_new := !!col_popn)
  target <- rename(target, !!col_target_new := !!col_target)
  
  domestic_adjustment <- left_join(popn, target, by=col_aggregation) %>%
    mutate(net_dom_adjustment = !!sym(col_target_new) - !!sym(col_popn_new)) %>%
    select(c(col_aggregation, net_dom_adjustment))
  
  #if the change is poisitive then its an addition to in migration
  #if negative then add to out migration
  gross_in_adjustment <- filter(domestic_adjustment, net_dom_adjustment >= 0)
  gross_out_adjustment <- filter(domestic_adjustment, net_dom_adjustment < 0)
  
  dom_in_target <- dom_in %>%
    group_by_at(col_aggregation) %>%
    summarise(dom_in_orig = sum(dom_in)) %>%
    ungroup() %>%
    left_join(gross_in_adjustment, by = col_aggregation) %>%
    mutate(dom_in_target = ifelse(is.na(net_dom_adjustment), dom_in_orig,
                              dom_in_orig + net_dom_adjustment))  %>%
    select(c(col_aggregation, dom_in_target))
  
  dom_out_target <- dom_out %>%
    group_by_at(col_aggregation) %>%
    summarise(dom_out_orig = sum(dom_out)) %>%
    ungroup() %>%
    left_join(gross_in_adjustment, by = col_aggregation) %>%
    mutate(dom_out_target = ifelse(is.na(net_dom_adjustment), dom_out_orig,
                                  dom_out_orig + net_dom_adjustment))  %>%
    select(c(col_aggregation, dom_out_target))
  
  #Adust the gross flows
  adjusted_dom_in <- constrain_component(popn = dom_in,
                                         constraint = dom_in_target,
                                         col_aggregation = col_aggregation,
                                         col_popn = "dom_in",
                                         col_constraint = "dom_in_target")
  
  adjusted_dom_out <- constrain_component(popn = dom_out,
                                          constraint = dom_out_target,
                                          col_aggregation = col_aggregation,
                                          col_popn = "dom_out",
                                          col_constraint = "dom_out_target")
  
  return(list(dom_in = adjusted_dom_in,
              dom_out = adjusted_dom_out))
  
}


