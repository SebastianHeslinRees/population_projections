#' Create borough data from ward model outputs
#' 
#' For most components this is a simple sum function. For migration flows
#' scaling is applied to input flows so that the net matches the output from
#' the ward model but the gross flows are not just summed but are closer to
#' something reasonable. This is an attempt to account for intra-borough moves
#' in the ward data.
#' 
#' @param model_output List the output from the arrange_small_area_outputs function
#' @param constraint_path String the location of inflow and outflow borough data
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom assertthat assert_that
#' 
#' @return A list of projection outputs by component for boroughs
#' 

aggregate_borough_data <- function(model_output, constraint_path){
  
  borough_data <- model_output[1:6] %>% 
    lapply(.make_borough)
  
  constraints <- list(dom_in = "dom_in",
                      dom_out = "dom_out",
                      int_in = "int_in",
                      int_out = "int_out") %>% 
    lapply(function(x){readRDS(paste0(constraint_path, x,".rds")) %>% 
        rename(value = !!last(names(.))) %>% 
        mutate(flow = ifelse(x %in% c("dom_in","int_in"),"trend_inflow","trend_outflow"))})
  
  total <- rbindlist(constraints) %>% 
    dtplyr::lazy_dt() %>% 
    filter(year %in% borough_data$net_migration$year,
           gss_code %in% borough_data$net_migration$gss_code) %>% 
    group_by(gss_code, year, sex, age, flow) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame() %>% 
    pivot_wider(names_from = flow, values_from = value) %>% 
    mutate(trend_netflow = trend_inflow - trend_outflow,
           trend_ratio =  trend_inflow/(trend_inflow + trend_outflow),
           trend_ratio = ifelse(is.na(trend_ratio),0.5,0)) %>% 
    left_join(borough_data$in_migration, by = c("gss_code", "year", "sex", "age")) %>% 
    left_join(borough_data$out_migration, by = c("gss_code", "year", "sex", "age", "la_name")) %>% 
    left_join(borough_data$net_migration, by = c("gss_code", "year", "sex", "age", "la_name")) %>% 
    mutate(netdiff = trend_netflow - net_migration,
           ratio_in = netdiff * trend_ratio,
           ratio_out = netdiff - ratio_in,
           final_in = trend_inflow - ratio_in,
           final_out = trend_outflow + ratio_out,
           final_net = final_in - final_out) %>% 
    mutate(final_in = ifelse(final_out < 0, final_in-final_out, final_in),
           final_out = ifelse(final_in < 0, final_out-final_in, final_out)) %>% 
    check_negative_values("final_in") %>% 
    check_negative_values("final_out") %>% 
    mutate(check_net1 = final_in - final_out - final_net,
           check_net2 = final_net - net_migration) 
    
  #validate
  assertthat::assert_that(all(between(total$check_net1, -0.01, 0.01)), msg="in aggreate_borough_data net migration value is not as expected")
  assertthat::assert_that(all(between(total$check_net2, -0.01, 0.01)), msg="in aggreate_borough_data net migration value is not as expected")
  assertthat::assert_that(nrow(filter(total, final_in < 0))==0, msg="in aggreate_borough_data negatives in inflow")
  assertthat::assert_that(nrow(filter(total, final_out < 0))==0, msg="in aggreate_borough_data negatives in outflow")
  
  borough_data$in_migration <- total %>% 
    select(gss_code, la_name, year, sex, age,
           inflow = final_in) %>% 
    data.frame()
  
  borough_data$out_migration <- total %>% 
    select(gss_code, la_name, year, sex, age,
           outflow = final_out)%>% 
    data.frame()
  
  borough_data$net_migration <- total %>% 
    select(gss_code, la_name, year, sex, age,
           netflow = final_net)%>% 
    data.frame()
  
  return(borough_data)
  
}

.make_borough <- function(x){
  nm <- last(names(x))
  x <- x %>% 
    rename(value = !!nm) %>% 
    group_by(gss_code, la_name, year, sex, age) %>% 
    summarise(!!nm := sum(value), .groups = 'drop_last') %>% 
    data.frame()
}
