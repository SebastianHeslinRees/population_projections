#' Household model outputs
#'
#' This function generates various outputs from the household model.
#'
#' @param model_output The household model output, as created by
#'   \code{household_model_ons} or \code{household_model_dclg}.
#' @param model String. Either "ons" or "dclg".
#' @param output_dir Location for output.
#' @param write_excel Logical. Whether to output Excel .xlsx files.
#' @param projection_name String. The projection name.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom tidyr pivot_wider
#' @importFrom data.table fwrite
#'
#' @examples
#' household_model_outputs(model_output, "ons", "/output_dir", TRUE, "Projection 2022")

household_model_outputs <- function(model_output, model, output_dir, write_excel, projection_name){
  
  #setup output folder
  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  hh_output_dir <- paste0(output_dir, "households/")
  dir.create(hh_output_dir, showWarnings = FALSE)
  
  #col aggregation
  if(model == "dclg"){ col_aggregation <- c("gss_code", "year", "sex", "age_group", "household_type")}
  if(model == "ons"){ col_aggregation <- c("gss_code", "year", "sex", "age_group")}
  
  #create dataframes
  stage_1_sheet <- model_output$stage_1$detailed_households %>%
    filter(year >= 2011) %>%
    group_by_at(col_aggregation) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = year, values_from = households)
  
  stage_2_sheet <- model_output$stage_2$constrained %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, age_group, household_type) %>%
    summarise(households = sum(households), .groups = 'drop_last') %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = year, values_from = households)
  
  ce_pop <- model_output$stage_1$communal_establishment_population %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(ce_popn = sum(communal_establishment_population), .groups = 'drop_last') %>%
    tidyr::pivot_wider(names_from = year, values_from = ce_popn)
  
  hh_pop <- model_output$stage_1$household_population %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(hh_popn = sum(household_population), .groups = 'drop_last') %>%
    tidyr::pivot_wider(names_from = year, values_from = hh_popn)
  
  household_summary_sheet <- model_output$stage_1$detailed_households %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year) %>%
    summarise(households = sum(households),
              household_population = sum(household_population),
              communal_establishment_population = sum(communal_establishment_population),
              .groups = 'drop_last') %>%
    ungroup() %>%
    mutate(average_household_size = household_population / households)
  
  # Store all dataframes in a list
  output_dataframes <- list(stage1_households = stage_1_sheet,
                            stage2_households = stage_2_sheet,
                            detailed_ce_pop = ce_pop,
                            detailed_hh_pop = hh_pop,
                            household_summary = household_summary_sheet)
  
  #add authority names
  names_lookup <- get_gss_names()
  
  #output csv files
  for(i in seq(output_dataframes)){
    nm <- setdiff(names(output_dataframes[[i]]),"gss_code")
    
    #join dfs to names lookup
    output_dataframes[[i]] <- left_join(output_dataframes[[i]], names_lookup, by="gss_code") %>%
      select_at(c("gss_code", "gss_name", nm)) %>% 
      datastore_csv()
    
    #
    output_dataframes$household_summary <- output_dataframes$household_summary %>%
      mutate(average_household_size = (household_population / households) %>%
               round(digits = 3)) # since the summing in datastore_csv gets this wrong
    
    fwrite(output_dataframes[[i]],
           paste0(hh_output_dir, model, "_", names(output_dataframes)[i], ".csv"))
  }
  
  #RDS
  saveRDS(model_output[['stage_1']][['detailed_households']], paste0(hh_output_dir, model, "_", "stage_1_households.rds"))
  saveRDS(model_output[['stage_1']][['household_population']], paste0(hh_output_dir, model, "_", "household_population.rds"))
  saveRDS(model_output[['stage_1']][['communal_establishment_population']], paste0(hh_output_dir, model, "_", "communal_est_population.rds"))
  saveRDS(model_output[['stage_2']][['constrained']], paste0(hh_output_dir, model, "_", "stage_2_households.rds"))
  saveRDS(select(output_dataframes[['household_summary']],
                 year, gss_code, gss_name, ahs = average_household_size),
          paste0(hh_output_dir, model, "_", "ahs.rds"))
  
  if(write_excel){
    excel_wb_name <- substr(projection_name,1,nchar(projection_name)-14)
    variant_name <- paste("Variant trend projection:",str_replace_all(excel_wb_name, "_", " "))
    create_household_model_excels(output_dir, excel_wb_name, variant_name, model)
  }
  
}

#---------------------------------------

#' Datastore CSV
#'
#' This function prepares the data for output as a CSV file.
#'
#' @param x The input data frame.
#'
#' @return The modified data frame ready for output as a CSV file.
#'
#' @examples
#' datastore_csv(data_frame)
#'
datastore_csv <- function(x){
  
  x <- as.data.frame(x)
  
  sort_order <- intersect(names(x), c("gss_code", "gss_name", "year", "sex", "age_group", "household_type"))
  data_cols <- setdiff(names(x), sort_order)
  
  x <- filter(x, substr(gss_code,1,3) == "E09" | gss_code == "E12000007")
  
  if(!"E12000007" %in% x$gss_code){
    x_summary <- mutate(x, gss_code = "E12000007", gss_name = "London (total)") %>%
      group_by_at(sort_order) %>%
      summarise_at(data_cols, .funs=list(sum)) %>%
      ungroup()
    
    x <- rbind(x_summary, x)
  } else {
    x <- mutate(x, gss_name = recode(gss_name, "London" = "London (total)"))
  }
  
  if("age_group" %in% sort_order){
    x <- mutate(x, age_group = recode(age_group, "0_4" = "00_04", "5_9" = "05_09", "85&" = "85+"))
  }
  
  x
}
  
  x
}
  
  x <- dplyr::arrange_at(x, sort_order) %>%
    reorder_for_output()
  
  #round data for output
  idx <- sapply(x, class)=="numeric"
  x[, idx] <- lapply(x[, idx], round, digits=3)
  
  x <- as.data.frame(x)
  
}