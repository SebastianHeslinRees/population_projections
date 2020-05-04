#' Trend Model Datastore outputs
#'
#' @param population,births,deaths,int_in,int_out,dom_in,dom_out Data frames with population and component data.
#' @param output_dir Path to output directory.
#' @param excel_file_name Output file name. With or without xslx suffix.
#'
#' @import dplyr
#' @export

trend_datastore_outputs <- function(population, births, deaths, int_in, int_out, dom_in, dom_out,
                                    output_dir, excel_file_name){
  
  #datastore directory
  if(!grepl("/$", output_dir)){ output_dir <- paste0(output_dir, "/") }
  datastore_dir <- paste0(output_dir,"datastore")
  dir.create(datastore_dir, recursive = T, showWarnings = F)
  
  #excel file name
  if(substr(excel_file_name, nchar(excel_file_name)-4, nchar(excel_file_name))!=".xlsx"){
    excel_file_name <- paste0(excel_file_name,".xlsx")
  }
  
  #process data
  female <- filter(population, sex == "female") %>%
    wrangle_datastore_outputs()
  
  male <- filter(population, sex == "male") %>%
    wrangle_datastore_outputs()
  
  persons <- population %>%
    mutate(sex = "persons") %>%
    group_by(year, gss_code, sex, age) %>%
    summarise(popn = sum(popn)) %>%
    ungroup()%>%
    wrangle_datastore_outputs()
  
  #CoC
  births <- get_component_datastore(births, "births")
  deaths <- get_component_datastore(deaths, "deaths")
  int_in <- get_component_datastore(int_in, "int_in")
  int_out <- get_component_datastore(int_out, "int_out")
  dom_in <- get_component_datastore(dom_in, "dom_in")
  dom_out <- get_component_datastore(dom_out, "dom_out")
  popn <- get_component_datastore(population, "popn")
  
  
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
    mutate(total_change = births - deaths + int_net + dom_net) %>%
    mutate(borough = recode(borough, "London" = "London (total)")) %>%
    select(gss_code, borough, year,
           population, births, deaths,
           int_in, int_out, int_net,
           dom_in, dom_out, dom_net,
           total_change) %>%
    as.data.frame()
  
  #round data for output
  idx <- sapply(components, class)=="numeric"
  components[, idx] <- lapply(components[, idx], round, digits=3)
  
  #excel
  wb <- xlsx::loadWorkbook("input_data/excel_templates/trend_template.xlsx")
  wb_sheets<- xlsx::getSheets(wb)
  
  xlsx::addDataFrame(persons, wb_sheets$persons, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(female, wb_sheets$females, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(male, wb_sheets$males, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  xlsx::addDataFrame(components, wb_sheets$`components of change`, col.names = FALSE, row.names = FALSE, startRow = 2, startColumn = 1)
  
  #Write xlsx file
  wb_filename <- paste(datastore_dir,excel_file_name,sep="/")
  xlsx::saveWorkbook(wb, wb_filename)
  
  
}

#--------------------------------------------


wrangle_datastore_outputs <- function(x){
  
  wrangled <- filter(x, year >= 2011) %>%
    filter_to_london() %>%
    left_join(get_gss_names(), by = "gss_code") %>%
    mutate(gss_name = recode(gss_name, "London" = "London (total)")) %>%
    rename(borough = gss_name) %>%
    select(year, gss_code, borough, sex, age, popn) %>%
    mutate(popn = round(popn, digits=3)) %>%
    tidyr::pivot_wider(names_from = year, values_from = popn) %>%
    as.data.frame()
  
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


