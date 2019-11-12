household_model_outputs <- function(model_output, model, output_dir, timestamp, write_excel=FALSE){

  if(model == "dclg"){ col_aggregation <- c("gss_code", "year", "sex", "age_group", "household_type")}
  if(model == "ons"){ col_aggregation <- c("gss_code", "year", "sex", "age_group")}

  stage_1_sheet <- model_output$stage_1$detailed_households %>%
    group_by_at(col_aggregation) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = year, values_from = households)
  
  stage_2_sheet <- model_output$stage_2$constrained %>%
    group_by(gss_code, year, age_group, household_type) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = year, values_from = households)
  
  ce_pop <- model_output$stage_1$communal_establishment_population %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(ce_popn = sum(communal_establishment_population)) %>%
    tidyr::pivot_wider(names_from = year, values_from = ce_popn)
  
  hh_pop <- model_output$stage_1$household_population %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(hh_popn = sum(household_population)) %>%
    tidyr::pivot_wider(names_from = year, values_from = hh_popn)
  
  household_summary_sheet <- model_output$stage_1$detailed_households %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year) %>%
    summarise(households = sum(households),
              households_population = sum(household_population),
              communal_establishment_population = sum(communal_establishment_population)) %>%
    ungroup() %>%
    mutate(average_households_size = households_population / households)
  
  output_dataframes <- list(stage1_households = stage_1_sheet,
                            stage2_households = stage_2_sheet,
                            detailed_ce_pop = ce_pop,
                            detailed_hh_pop = hh_pop,
                            household_summary = household_summary_sheet)
  
  hh_output_dir <- paste0(output_dir, "/households_",timestamp,"/")
  dir.create(hh_output_dir, showWarnings = FALSE)
  
  lapply(seq_along(output_dataframes),
         function(i) data.table::fwrite(output_dataframes[[i]],
                                        paste0(hh_output_dir, model, "_", names(output_dataframes)[i], ".csv"))) %>%
    invisible()
  
  if(write_excel){
      
      datastore_folder <- rprojroot::find_root_file(output_dir, criterion = rprojroot::is_git_root)
      datastore_folder <- gsub("/", "\\\\", datastore_folder)
      templates_folder <- rprojroot::find_root_file("documentation", "templates", criterion = rprojroot::is_git_root)
      templates_folder <- gsub("/", "\\\\", templates_folder)
      run_excel_vba <- data.frame(a = paste0("start Excel.exe \"", templates_folder, "\\excel_household_template.xlsm"))
      data.table::fwrite(run_excel_vba, "documentation/templates/run_excel_households_vba.bat", col.names=F, quote=F)
      
      bas_file <- "documentation/templates/datastore_households_VBA.bas"
      vba <- create_households_VBA_script(datastore_folder, hh_output_dir, model)
      data.table::fwrite(vba, bas_file, col.names=F, quote=F)
      file.remove("documentation/templates/temp_file.xlsm")
      shell.exec(rprojroot::find_root_file("documentation","templates","run_excel_households_vba.bat",
                                           criterion = rprojroot::is_git_root))
  }
  
  
}

