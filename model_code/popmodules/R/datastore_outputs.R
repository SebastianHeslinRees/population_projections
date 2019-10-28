#
# population <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/population19-10-25_1457.rds")
# births <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/births19-10-25_1457.rds")
# deaths <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/deaths19-10-25_1457.rds")
# int_in <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/int_in19-10-25_1457.rds")
# int_out <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/int_out19-10-25_1457.rds")
# dom_in <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/dom_in19-10-25_1457.rds")
# dom_out <- readRDS("//homedata/home$/WTonkiss1/Projects/population_projections/outputs/trend/2018/dom_out19-10-25_1457.rds")


datastore_outputs(population, births, deaths, int_in, int_out, dom_in, dom_out,
                  "outputs/trend/2018/datastore", "datastore_file.xlsx")

datastore_outputs <- function(population, births, deaths, int_in, int_out, dom_in, dom_out,
                              folder_location, file_name){

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

  components <- left_join(popn, births, by = c("gss_code", "year")) %>%
    left_join(deaths, by = c("gss_code", "year")) %>%
    left_join(int_in, by = c("gss_code", "year")) %>%
    left_join(int_out, by = c("gss_code", "year")) %>%
    mutate(int_net = int_in - int_out) %>%
    left_join(dom_in, by = c("gss_code", "year")) %>%
    left_join(dom_out, by = c("gss_code", "year")) %>%
    mutate(dom_net = dom_in - dom_out) %>%
    mutate(total_change = births - deaths + int_net + dom_net)

  #write
  dir.create(folder_location, recursive = T, showWarnings = F)

  data.table::fwrite(persons, paste0(folder_location,"/persons.csv"))
  data.table::fwrite(female, paste0(folder_location,"/females.csv"))
  data.table::fwrite(male, paste0(folder_location,"/males.csv"))
  data.table::fwrite(components, paste0(folder_location,"/components.csv"))

  #excel
  #TODO: Do the folder setting better - VBA needs whole path, not relative
  folder_loc <- paste0(getwd(),"/",folder_location)
  vba <- create_VBA_script(folder_loc, file_name)
  data.table::fwrite(vba, "M:/Projects/population_projections/documentation/templates/datastoreVBA.bas", col.names=F, quote=F)
  shell.exec("M:/Projects/population_projections/documentation/templates/run_excel_vba.bat")

  message("complete")

}

#--------------------------------------

create_VBA_script <- function(folder_location, file_name){

  x <- data.frame(a = c(

    "Attribute VB_Name = \"datastoreVBA\"",

    "Sub run_it()",

    "Set This_wkb = Application.ThisWorkbook",
    "Set wkb = Workbooks.Add",
    "Call run_open",

    "Application.DisplayAlerts = False",
    "Worksheets(\"Sheet1\").Delete",
    "Application.DisplayAlerts = True",

    paste0("wkb.SaveAs fileName:=\"",folder_location,"\\",file_name,"\""),
    "wkb.Close SaveChanges:=False",

    "This_wkb.Close SaveChanges:=False",
    "Application.Quit",

    "End Sub",

    "Sub run_open()",

    paste0("Call open_copy_csv(\"",folder_location,"\\persons.csv\", \"persons\")"),
    paste0("Call open_copy_csv(\"",folder_location,"\\females.csv\", \"females\")"),
    paste0("Call open_copy_csv(\"",folder_location,"\\males.csv\", \"males\")"),
    paste0("Call open_copy_csv(\"",folder_location,"\\components.csv\", \"components of change\")"),

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

