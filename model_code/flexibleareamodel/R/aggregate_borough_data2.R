#' Create borough data from ward model outputs
#' 
#' For most components this is a simple sum function. For migration flows
#' scaling is applied to input flows so that the net matches the output from
#' the ward model but the gross flows are not just summed but are closer to
#' something reasonable. This is an attempt to account for intra-borough moves
#' in the ward data.
#' 
#' @param model_output List the output from the arrange_outputs function
#' @param constraint_path String the location of inflow and outflow borough data
#' @param first_proj_yr Numeric
#' @param n_cores Numeric. Number of cores
#' @param parallel Logical. Should the regrosser process be run in parallel (TRUE) or serial (FALSE).
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom assertthat assert_that
#' @importFrom data.table rbindlist
#' 
#' @return A list of projection outputs by component for boroughs

aggregate_borough_data2 <- function(model_output, constraint_path, first_proj_yr, n_cores, parallel){
  
  col_agg <- c("gss_code", "la_name", "year", "sex", "age")
  constraint_path <- .add_slash(constraint_path)
  
  # sum population, births, deaths
  
  borough_data <- model_output[1:3] %>% 
    lapply(.make_borough)
  
  #-----------------------------------------------------------------------------
  
  # migration backeries from MYE components
  
  backseries_in <-list(dom_in = "dom_in_ons",
                       int_in = "int_in_gla") %>% 
    lapply(function(x){readRDS(paste0("input_data/mye/2020/", x,".rds")) %>% 
        rename(value = !!last(names(.)))}) %>% 
    rbindlist() %>% 
    mutate(flow = "trend_inflow") %>% 
    group_by(gss_code, year, sex, age, flow) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame()
  
  backseries_out <-list(dom_out = "dom_out_ons",
                        int_out = "int_out_gla") %>% 
    lapply(function(x){readRDS(paste0("input_data/mye/2020/", x,".rds")) %>% 
        rename(value = !!last(names(.)))}) %>%
    rbindlist() %>% 
    mutate(flow = "trend_outflow") %>% 
    group_by(gss_code, year, sex, age, flow) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  
  #Projected migration - net aggregated from wards, gross needs to be calculated
  
  projected_net <- .make_borough(model_output[['net_migration']]) %>% 
    filter(year >= first_proj_yr) %>% 
    group_by(year, gss_code, la_name) %>% 
    summarise(netflow = sum(netflow),
              .groups = 'drop_last') %>% 
    data.frame()
  
  trend_in <-list(dom_in = "dom_in",
                  int_in = "int_in") %>% 
    lapply(function(x){readRDS(paste0(constraint_path, x,".rds")) %>% 
        rename(value = !!last(names(.)))}) %>% 
    rbindlist() %>% 
    mutate(flow = "trend_inflow") %>% 
    group_by(gss_code, year, sex, age, flow) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame()
  
  trend_out <-list(dom_out = "dom_out",
                   int_out = "int_out") %>% 
    lapply(function(x){readRDS(paste0(constraint_path, x,".rds")) %>% 
        rename(value = !!last(names(.)))}) %>% 
    rbindlist() %>% 
    mutate(flow = "trend_outflow") %>% 
    group_by(gss_code, year, sex, age, flow) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    data.frame()
  
  gross_flows <- rbind(trend_in, trend_out) %>% 
    filter(gss_code %in% unique(projected_net$gss_code),
           year %in% unique(projected_net$year)) %>%
    pivot_wider(values_from = value, names_from = flow)
  
  #-----------------------------------------------------------------------------
  
  #Regrosser process
  
  optimised_flows <- regross(base_in = select(gross_flows, -trend_outflow),
                             base_out = select(gross_flows, -trend_inflow),
                             target_net = projected_net,
                             col_inflow = "trend_inflow",
                             col_outflow = "trend_outflow",
                             col_target = "netflow",
                             n_cores = n_cores,
                             fun = 2,
                             parallel = parallel)
  
  final_migration <- gross_flows %>%
    group_by(year, gss_code) %>%
    mutate(dist_in = trend_inflow / sum(trend_inflow),
           dist_out = trend_outflow / sum(trend_outflow)) %>%
    data.frame() %>%
    select(-trend_inflow, -trend_outflow) %>% 
    left_join(optimised_flows, by = c("year", "gss_code")) %>%
    mutate(inflow = inflow * dist_in,
           outflow = outflow * dist_out,
           netflow = inflow - outflow) %>%
    select(col_agg, inflow, outflow, netflow)
  
  final_migration <- list(inflow = select(final_migration, col_agg, inflow),
                          outflow = select(final_migration, col_agg, outflow),
                          netflow = select(final_migration, col_agg, netflow))
  
  #-----------------------------------------------------------------------------
  
  # Join the backseries to the projected
  
  names_lookup <- select(projected_net, gss_code, la_name) %>% unique()
  
  borough_data$in_migration <- backseries_in %>% 
    left_join(names_lookup, by = "gss_code") %>% 
    select(col_agg, inflow = value) %>% 
    filter(year < first_proj_yr,
           gss_code %in% unique(projected_net$gss_code)) %>% 
    rbind(final_migration$inflow) %>% 
    select(col_agg, inflow) %>% 
    data.frame()
  
  borough_data$out_migration <- backseries_out %>% 
    left_join(names_lookup, by = "gss_code") %>% 
    select(col_agg, outflow = value) %>% 
    filter(year < first_proj_yr,
           gss_code %in% unique(projected_net$gss_code)) %>% 
    rbind(final_migration$outflow) %>% 
    select(col_agg, outflow) %>% 
    data.frame()
  
  borough_data$net_migration <- rbind(backseries_in, backseries_out) %>% 
    filter(gss_code %in% unique(projected_net$gss_code),
           year < first_proj_yr) %>%
    pivot_wider(values_from = value, names_from = flow) %>% 
    filter(year < first_proj_yr,
           gss_code %in% unique(projected_net$gss_code)) %>% 
    left_join(names_lookup, by = "gss_code") %>% 
    mutate(netflow = trend_inflow - trend_outflow) %>% 
    select(col_agg, netflow) %>% 
    rbind(final_migration$netflow) %>% 
    select(col_agg, netflow) %>% 
    data.frame()
  
  #-----------------------------------------------------------------------------
  
  # Components summary output dataframe
  
  borough_data$summary <- borough_data$population %>% 
    left_join(borough_data$birth, by = col_agg) %>% 
    left_join(borough_data$deaths, by = col_agg) %>% 
    left_join(borough_data$in_migration, by = col_agg) %>% 
    left_join(borough_data$out_migration, by = col_agg) %>% 
    left_join(borough_data$net_migration, by = col_agg) %>% 
    mutate(births = ifelse(age > 0, 0, births),
           change = births - deaths + netflow) %>% 
    select(gss_code, la_name, year,
           population = popn, births, deaths,
           inflow, outflow, netflow, change) %>% 
    group_by(gss_code, la_name, year) %>% 
    summarise(across(everything(), sum)) %>% 
    data.frame() %>% 
    mutate(across(where(is.numeric) & !year, ~round(.x, digits=3)))
  
  #-----------------------------------------------------------------------------
  
  # development
  borough_data$dwelling_trajectory <- .make_borough(model_output$dwelling_trajectory)
  borough_data$dwelling_stock <- .make_borough(model_output$dwelling_stock)
  
  #-----------------------------------------------------------------------------
  
  return(borough_data)
  
}
