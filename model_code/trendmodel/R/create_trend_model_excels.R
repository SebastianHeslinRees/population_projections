#' Produce 3 Excel workbooks from a set of projection RDS files
#'
#' If a projection has been run with the \code{write_excel} variable set to
#' \code{FALSE} and the Excel outputs are subsequently required this function
#' will produce those workbooks. Creates a population workbook, an ONS household
#' workbook and a DCLG household workbook.
#'
#' @param output_dir String. The output directory where the projection RDS files are
#'   saved.
#' @param wb_filename String. File name for the population workbook output.
#'  With or without "xlsx" suffix.
#' @param household_models Logical. Should the household model excel files also
#'  be created.
#'  
#' @import dplyr
#' @import reticulate
#' @import popmodules
#' @import stringr
#' @importFrom tidyr pivot_wider
#' @importFrom dtplyr lazy_dt
#' 
#' @export

create_trend_model_excels <- function(output_dir, wb_filename, projection_name,
                                      household_models = TRUE){
  
  #datastore directory
  output_dir <- .add_slash(output_dir)
  datastore_dir <- paste0(output_dir,"datastore")
  dir.create(datastore_dir, recursive = T, showWarnings = F)
  
  #excel file name
  if(str_sub(wb_filename,-5,-1)!=".xlsx"){
    wb_filename <- paste0(wb_filename,".xlsx")
  }
  
  #read data
  data <- list(population = "population.rds",
               births = "births.rds",
               deaths = "deaths.rds",
               int_in = "int_in.rds",
               int_out = "int_out.rds",
               dom_in = "dom_in.rds",
               dom_out = "dom_out.rds",
               popn_adjustment ="popn_adjustment.rds") %>% 
    lapply(function(x){paste0(output_dir,x)}) %>% 
    lapply(readRDS)
  
  #process data
  female <- filter(data$population, sex == "female") %>%
    wrangle_datastore_outputs()
  
  male <- filter(data$population, sex == "male") %>%
    wrangle_datastore_outputs()
  
  persons <- data$population %>%
    mutate(sex = "persons") %>%
    group_by(year, gss_code, sex, age) %>%
    summarise(popn = sum(popn), .groups = 'drop_last') %>%
    ungroup() %>%
    wrangle_datastore_outputs()
  
  #CoC
  births <- get_component_datastore(data$births, "births")
  deaths <- get_component_datastore(data$deaths, "deaths")
  int_in <- get_component_datastore(data$int_in, "int_in")
  int_out <- get_component_datastore(data$int_out, "int_out")
  dom_in <- get_component_datastore(data$dom_in, "dom_in")
  dom_out <- get_component_datastore(data$dom_out, "dom_out")
  popn <- get_component_datastore(data$population, "popn")
  
  components <- left_join(popn, get_gss_names(), by="gss_code") %>%
    rename(borough = gss_name,
           population = popn) %>%
    left_join(births, by = c("gss_code", "year")) %>%
    left_join(deaths, by = c("gss_code", "year")) %>%
    left_join(int_in, by = c("gss_code", "year")) %>%
    left_join(int_out, by = c("gss_code", "year")) %>%
    mutate(int_net = int_in - int_out) %>%
    left_join(dom_in, by = c("gss_code", "year")) %>%
    left_join(dom_out, by = c("gss_code", "year")) %>%
    mutate(dom_net = dom_in - dom_out) %>%
    mutate(borough = recode(borough, "London" = "London (total)"))
  
  popn_adjustment <- get_component_datastore(data$popn_adjustment, "adjustment")
  
  components <- components %>% 
    left_join(popn_adjustment, by = c("gss_code", "year")) %>% 
    mutate(total_change = births - deaths + int_net + dom_net + adjustment) %>%
    select(gss_code, borough, year,
           population, births, deaths,
           int_in, int_out, int_net,
           dom_in, dom_out, dom_net,
           adjustment, total_change) %>%
    reorder_for_output() %>%
    as.data.frame()
  
  #round data for output
  idx <- sapply(components, class)=="numeric"
  components[, idx] <- lapply(components[, idx], round, digits=3)
  
  #python excel
  #TODO
  #FIXME
  #There is a problem with reticulate/python/renv that I can't get to the bottom of
  #The first time a python script is sourced the function throws an error. It can't find
  #rpytools. If you source again it works. The temp work around is to wrap the source in
  #try() so the error is caught and then run it a second time. Its ugly but it works.
  #See also smallareamodel::create_small_area_excels, etc
  
  try( reticulate::source_python('model_code/other_scripts/python_to_excel_trendmodel.py'), silent = TRUE)
  reticulate::source_python('model_code/other_scripts/python_to_excel_trendmodel.py') 
  python_to_excel_trendmodel(persons, female, male, components, datastore_dir, wb_filename, projection_name)
  
  if(household_models){
    wb_filename <- substr(wb_filename, 1, nchar(wb_filename)-5)
    for(model in c("ons","dclg")){
      create_household_model_excels(output_dir, wb_filename, projection_name, model)
    }
  }
}

#--------------------------------------------

wrangle_datastore_outputs <- function(x){
  
  years <- max(min(x$year), 2011):max(x$year) %>%
    as.character()
  
  wrangled <- filter(x, year >= 2011) %>%
    filter_to_london() %>%
    left_join(get_gss_names(), by = "gss_code") %>%
    mutate(gss_name = recode(gss_name, "London" = "London (total)")) %>%
    rename(borough = gss_name) %>%
    select(year, gss_code, borough, sex, age, popn) %>%
    mutate(popn = round(popn, digits=3)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    select_at(c("gss_code", "borough", "age", "sex", years)) %>%
    arrange(gss_code, sex, age) %>%
    reorder_for_output() %>%
    as.data.frame()
  
  return(wrangled)
  
}

filter_to_london <- function(x){
  filter(x, substr(gss_code,1,3) == "E09" | gss_code == "E12000007")
}

get_component_datastore <- function(component, data_col){
  
  component <- component %>% 
    dtplyr::lazy_dt() %>%
    filter(year >= 2011) %>%
    filter_to_london() %>%
    rename(value = !!data_col) %>%
    group_by(gss_code, year) %>%
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    rename(!!data_col := value) 
  
}