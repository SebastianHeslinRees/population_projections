ons_hh_model_outputs <- function(model_output, output_dir, timestamp){

  #Deatiled outputs
  detailed_unconstrained <- model_output$unconstrained %>%
    pivot_wider(names_from = year, values_from = households)

  detailed_constrained <- model_output$constrained %>%
    pivot_wider(names_from = year, values_from = households)

  detailed_household_pop <- model_output$household_population %>%
    pivot_wider(names_from = year, values_from = household_popn)

  detailed_ce_pop <- model_output$communal_establishment_population %>%
    pivot_wider(names_from = year, values_from = ce_pop)


  #Summary outputs
  data_totals <- function(data, in_col, out_col){
    x <- data.frame(data) %>%
      rename(value = in_col) %>%
      group_by(gss_code, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(!!out_col := value)
  }

  total_ce_pop <- data_totals(model_output$communal_establishment_population,
                              "ce_pop", "communal_establishment_popualtion")
  total_hh_pop <- data_totals(model_output$household_population,
                              "household_popn", "household_population")
  total_constrained <- data_totals(model_output$constrained,
                                   "households", "constrained_households")
  total_unconstrained <- data_totals(model_output$unconstrained,
                                     "households", "unconstrained_households")

  household_summary_sheet <- left_join(total_unconstrained, total_constrained, by = c("gss_code", "year")) %>%
    left_join(total_hh_pop, by = c("gss_code", "year")) %>%
    left_join(total_ce_pop, by = c("gss_code", "year")) %>%
    mutate(averge_household_size = household_population / constrained_households)


  #write outputs
  #TODO Add LA & region names to outputs
  output_dataframes <- list(detailed_unconstrained = detailed_unconstrained,
                            detailed_constrained = detailed_constrained,
                            detailed_household_pop = detailed_household_pop,
                            detailed_ce_pop = detailed_ce_pop,
                            household_summary = household_summary_sheet)

  output_dir <- paste0(output_dir, "/households_",timestamp,"/")
  dir.create(output_dir, showWarnings = FALSE)

  lapply(seq_along(output_dataframes),
         function(i) data.table::fwrite(output_dataframes[[i]],
                                        paste0(output_dir, "ons_",names(output_dataframes)[i], ".csv"))) %>%
    invisible()

}


dclg_hh_model_outputs <- function(model_output, output_dir, timestamp){

  stage_1_sheet <- model_output$stage_1$scaled_households %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = households)

  stage_2_sheet <- model_output$stage_2$constrained %>%
    group_by(gss_code, year, age_group) %>%
    summarise(households = sum(households)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = households)

  ce_pop <- model_output$stage_1$scaled_households %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(scaled_ce_popn = sum(scaled_ce_popn)) %>%
    pivot_wider(names_from = year, values_from = scaled_ce_popn)

  hh_pop <- model_output$stage_1$scaled_households %>%
    group_by(gss_code, year, sex, age_group) %>%
    summarise(scaled_hh_popn = sum(scaled_hh_popn)) %>%
    pivot_wider(names_from = year, values_from = scaled_hh_popn)

  household_summary_sheet <- model_output$stage_1$scaled_households %>%
    group_by(gss_code, year) %>%
    summarise(households = sum(scaled_households),
              households_population = sum(scaled_hh_popn),
              communal_establishment_population = sum(scaled_ce_popn)) %>%
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
                                        paste0(output_dir, "dclg_", names(output_dataframes)[i], ".csv"))) %>%
    invisible()

}

