#' Household model outputs
#'
#' @param model_output The household model output, as created by
#'   \code{household_model_ons} or \code{household_model_dclg}.
#' @param model String. Either "ons" or "dclg".
#' @param output_dir Location for output.
#' @param write_excel Logical. Whether to output Excel .xls files.
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider

household_model_outputs <- function(model_output, model, output_dir, write_excel){

  if(model == "dclg"){ col_aggregation <- c("gss_code", "year", "sex", "age_group", "household_type")}
  if(model == "ons"){ col_aggregation <- c("gss_code", "year", "sex", "age_group")}

  stage_1_sheet <- model_output$stage_1$detailed_households %>%
    filter(year >= 2011) %>%
    group_by_at(col_aggregation) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = households)

  stage_2_sheet <- model_output$stage_2$constrained %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, age_group, household_type) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = households)

  ce_pop <- model_output$stage_1$communal_establishment_population %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(ce_popn = sum(communal_establishment_population)) %>%
    pivot_wider(names_from = year, values_from = ce_popn)

  hh_pop <- model_output$stage_1$household_population %>%
    filter(year >= 2011) %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(hh_popn = sum(household_population)) %>%
    pivot_wider(names_from = year, values_from = hh_popn)

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

  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  hh_output_dir <- paste0(output_dir, "households/")
  dir.create(hh_output_dir, showWarnings = FALSE)

  #add authority names
  names_lookup <- data.table::fread('input_data/lookup/lad18_code_to_name.csv')
  for(i in seq(output_dataframes)){
    nm <- setdiff(names(output_dataframes[[i]]),"gss_code")

    output_dataframes[[i]] <- left_join(output_dataframes[[i]], names_lookup, by="gss_code") %>%
      select(c(gss_code, gss_name, !!nm))
  }


  #CSV
  lapply(seq_along(output_dataframes),
         function(i) data.table::fwrite(output_dataframes[[i]],
                                        paste0(hh_output_dir, model, "_", names(output_dataframes)[i], ".csv"))) %>%
    invisible()

  #RDS
  saveRDS(model_output[['stage_1']][['detailed_households']], paste0(hh_output_dir, model, "_", "stage_1_households.rds"))
  saveRDS(model_output[['stage_1']][['household_population']], paste0(hh_output_dir, model, "_", "household_population.rds"))
  saveRDS(model_output[['stage_1']][['communal_establishment_population']], paste0(hh_output_dir, model, "_", "communal_est_population.rds"))
  saveRDS(model_output[['stage_2']][['constrained']], paste0(hh_output_dir, model, "_", "stage_2_households.rds"))


  ahs <- model_output[['stage_1']][['detailed_households']] %>%
    dtplyr::lazy_dt() %>%
    group_by(year, gss_code) %>%
    summarise(ahs = sum(household_population)/sum(households)) %>%
    as.data.frame()

  saveRDS(ahs, paste0(hh_output_dir, model, "_", "ahs.rds"))

  if(write_excel){

    #Falls over if it tries to write an excel file while the previous excel process is still ongoing
    Sys.sleep(10)

    datastore_csv <- function(x, nm){

      x <- as.data.frame(x)

      col_order <- c("gss_code","gss_name",names(x)[names(x)!="gss_code"])
      sort_order <- intersect(names(x), c("gss_code", "year", "sex", "age_group", "household_type"))

      x <- filter(x, substr(gss_code,1,3) == "E09" | gss_code == "E12000007")

      if(!"E12000007" %in% x$gss_code){

        x <- mutate(x, gss_code = "E12000007") %>%
          group_by_at(sort_order) %>%
          summarise_all(.funs=list(sum)) %>%
          ungroup() %>%
          rbind(x)
      }

      if("age_group" %in% sort_order){
        x <- mutate(x, age_group = recode(age_group, "0_4" = "00_04", "5_9" = "05_09", "85&" = "85+"))
      }

      x <- dplyr::arrange_at(x, sort_order)

      x <- left_join(x, get_gss_names(), by="gss_code") %>%
        select(col_order)

      #round data for output
      idx <- sapply(x, class)=="numeric"
      x[, idx] <- lapply(x[, idx], round, digits=3)

      data.table::fwrite(x, paste0(output_dir,"/datastore_",timestamp,"/",model,"_",nm, ".csv"))

    }

    lapply(seq_along(output_dataframes),
           function(i) datastore_csv(output_dataframes[[i]], names(output_dataframes[i]))) %>%
      invisible()


    datastore_folder <- rprojroot::find_root_file(paste0(output_dir,"/datastore_",timestamp), criterion = rprojroot::is_git_root)
    datastore_folder <- gsub("/", "\\\\", datastore_folder)
    templates_folder <- rprojroot::find_root_file("documentation", "templates", criterion = rprojroot::is_git_root)
    templates_folder <- gsub("/", "\\\\", templates_folder)
    #household_folder <- rprojroot::find_root_file(hh_output_dir, criterion = rprojroot::is_git_root)
    #household_folder <- gsub("/", "\\\\", household_folder)

    run_excel_vba <- data.frame(a = paste0("start Excel.exe \"", templates_folder, "\\excel_household_template.xlsm"))
    data.table::fwrite(run_excel_vba, "documentation/templates/run_excel_households_vba.bat", col.names=F, quote=F)
    bas_file <- "documentation/templates/datastore_households_VBA.bas"

    vba <- create_households_VBA_script(datastore_folder, model)
    data.table::fwrite(vba, bas_file, col.names=F, quote=F)
    file.remove("documentation/templates/temp_file.xlsm")
    shell.exec(rprojroot::find_root_file("documentation","templates","run_excel_households_vba.bat",
                                         criterion = rprojroot::is_git_root))
  }
}

