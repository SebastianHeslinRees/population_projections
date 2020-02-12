#' Trend Model Datastore outputs
#'
#' @param population,births,deaths,int_in,int_out,dom_in,dom_out Data frames with population and component data.
#' @param output_dir Path to output directory.
#' @param excel_file_name Output file
#' @param write_excel Logical. Whether to create Excel output files.
#'
#' @import dplyr
#' @export
trend_datastore_outputs <- function(population, births, deaths, int_in, int_out, dom_in, dom_out,
                              output_dir, excel_file_name, write_excel){

  wrangle <- function(x){
    x <- filter(x, year >= 2011) %>%
      tidyr::spread(year, popn)
  }

  group_by_london <- function(x, data_col){
    london <- filter(x, substr(gss_code,1,3)=="E09") %>%
      mutate(gss_code = "E12000007") %>%
      rename(value = data_col) %>%
      group_by(year, gss_code, sex, age) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(!!data_col := value)
  }

  population <- filter(population, substr(gss_code,1,3)=="E09") %>%
    rbind(group_by_london(population, "popn"))

  female <- filter(population, sex == "female") %>%
    wrangle()

  male <- filter(population, sex == "male")%>%
    wrangle()

  persons <- population %>%
    mutate(sex = "persons") %>%
    group_by(year, gss_code, sex, age) %>%
    summarise(popn = sum(popn)) %>%
    ungroup()%>%
    wrangle()

  #CoC

  get_component_datastore <- function(component, data_col){

    component <- filter(component, substr(gss_code,1,3)=="E09") %>%
      rbind(group_by_london(component, data_col)) %>%
      rename(value = data_col) %>%
      group_by(gss_code, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(!!data_col := value) %>%
      filter(year >= 2011)
  }

  births <- get_component_datastore(births, "births")
  deaths <- get_component_datastore(deaths, "deaths")
  int_in <- get_component_datastore(int_in, "int_in")
  int_out <- get_component_datastore(int_out, "int_out")
  dom_in <- get_component_datastore(dom_in, "dom_in")
  dom_out <- get_component_datastore(dom_out, "dom_out")
  popn <- get_component_datastore(population, "popn")

  gss_names <- get_gss_names()

  components <- left_join(popn, gss_names, by="gss_code") %>%
    left_join(births, by = c("gss_code", "year")) %>%
    left_join(deaths, by = c("gss_code", "year")) %>%
    left_join(int_in, by = c("gss_code", "year")) %>%
    left_join(int_out, by = c("gss_code", "year")) %>%
    mutate(int_net = int_in - int_out) %>%
    left_join(dom_in, by = c("gss_code", "year")) %>%
    left_join(dom_out, by = c("gss_code", "year")) %>%
    mutate(dom_net = dom_in - dom_out) %>%
    mutate(total_change = births - deaths + int_net + dom_net) %>%
    select(gss_code, gss_name, year,
           population = popn, births, deaths,
           int_in, int_out, int_net,
           dom_in, dom_out, dom_net,
           total_change)

  #round data for output
  idx <- sapply(components, class)=="numeric"
  components[, idx] <- lapply(components[, idx], round, digits=3)

  #write
  if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")
  datastore_dir <- paste0(output_dir,"datastore")
  dir.create(datastore_dir, recursive = T, showWarnings = F)

  data.table::fwrite(persons, paste0(datastore_dir,"/persons.csv"))
  data.table::fwrite(female, paste0(datastore_dir,"/females.csv"))
  data.table::fwrite(male, paste0(datastore_dir,"/males.csv"))
  data.table::fwrite(components, paste0(datastore_dir,"/components.csv"))

  #excel
  if(write_excel) {
    message("excel process running")

    datastore_folder <- rprojroot::find_root_file(datastore_dir, criterion = rprojroot::is_git_root)
    datastore_folder <- gsub("/", "\\\\", datastore_folder)
    templates_folder <- rprojroot::find_root_file("documentation", "templates", criterion = rprojroot::is_git_root)
    templates_folder <- gsub("/", "\\\\", templates_folder)
    run_excel_vba <- data.frame(a = paste0("start Excel.exe \"", templates_folder, "\\excel_template.xlsm"))
    data.table::fwrite(run_excel_vba, "documentation/templates/run_excel_vba.bat", col.names=F, quote=F)

    bas_file <- "documentation/templates/datastoreVBA.bas"
    vba <- create_VBA_script(datastore_folder, file_name)
    data.table::fwrite(vba, bas_file, col.names=F, quote=F)
    file.remove("documentation/templates/temp_file.xlsm")
    shell.exec(rprojroot::find_root_file("documentation","templates","run_excel_vba.bat", criterion = rprojroot::is_git_root))

  }
}

#--------------------------------------

create_VBA_script <- function(datastore_dir, file_name){

  x <- data.frame(a = c(

    "Attribute VB_Name = \"datastoreVBA\"",

    "Sub run_it()",

    "Set This_wkb = Application.ThisWorkbook",
    "Set wkb = Workbooks.Add",
    "Call run_open",

    "Application.DisplayAlerts = False",
    "Worksheets(\"Sheet1\").Delete",
    "Application.DisplayAlerts = True",

    paste0("wkb.SaveAs fileName:=\"",datastore_dir,"\\",file_name,"\""),
    "wkb.Close SaveChanges:=False",

    "End Sub",

    "Sub run_open()",

    paste0("Call open_copy_csv(\"",datastore_dir,"\\persons.csv\", \"persons\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\females.csv\", \"females\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\males.csv\", \"males\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\components.csv\", \"components of change\")"),

    "End Sub",
    "Sub open_copy_csv(fileName, tabName)",

    "Dim wbkS As Workbook",
    "Dim wshS As Worksheet",
    "Dim wshT As Worksheet",

    "Set wshT = Worksheets.Add(After:=Worksheets(Worksheets.Count))",
    "Set wbkS = Workbooks.Open(fileName:=fileName)",
    "Set wshS = wbkS.Worksheets(1)",

    "wshS.UsedRange.Copy Destination:=wshT.Range(\"A1\")",
    "wshT.Name = tabName",
    "wbkS.Close SaveChanges:=False",

    "End Sub"))

}

#--------------------------------------

create_households_VBA_script <- function(datastore_dir, model){

  x <- data.frame(a = c(

    "Attribute VB_Name = \"datastoreVBA_households\"",

    "Sub run_it()",

    "Set This_wkb = Application.ThisWorkbook",
    "Set wkb = Workbooks.Add",
    "Call run_open",

    "Application.DisplayAlerts = False",
    "Worksheets(\"Sheet1\").Delete",
    "Application.DisplayAlerts = True",

    paste0("wkb.SaveAs fileName:=\"",datastore_dir,"\\",model,"_households\""),
    "wkb.Close SaveChanges:=False",

    "End Sub",

    "Sub run_open()",

    paste0("Call open_copy_csv(\"",datastore_dir,"\\",model,"_stage1_households.csv\", \"stage 1 households\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\",model,"_stage2_households.csv\", \"stage 2 households\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\",model,"_detailed_hh_pop.csv\", \"household popn\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\",model,"_detailed_ce_pop.csv\", \"communal est popn\")"),
    paste0("Call open_copy_csv(\"",datastore_dir,"\\",model,"_household_summary.csv\", \"summary\")"),

    "End Sub",

    "Sub open_copy_csv(fileName, tabName)",

    "Dim wbkS As Workbook",
    "Dim wshS As Worksheet",
    "Dim wshT As Worksheet",

    "Set wshT = Worksheets.Add(After:=Worksheets(Worksheets.Count))",
    "Set wbkS = Workbooks.Open(fileName:=fileName)",
    "Set wshS = wbkS.Worksheets(1)",

    "wshS.UsedRange.Copy Destination:=wshT.Range(\"A1\")",
    "wshT.Name = tabName",
    "wbkS.Close SaveChanges:=False",

    "End Sub"))

}

