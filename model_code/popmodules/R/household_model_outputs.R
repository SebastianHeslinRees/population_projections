household_model_outputs <- function(model_output, model, output_dir, timestamp){

  stage_1_sheet <- model_output$stage_1$detailed_households %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = year, values_from = households)

  stage_2_sheet <- model_output$stage_2$constrained %>%
    group_by(gss_code, year, age_group) %>%
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

  output_dir <- paste0(output_dir, "/households_",timestamp,"/")
  dir.create(output_dir, showWarnings = FALSE)

  lapply(seq_along(output_dataframes),
         function(i) data.table::fwrite(output_dataframes[[i]],
                                        paste0(output_dir, model, "_", names(output_dataframes)[i], ".csv"))) %>%
    invisible()

}

