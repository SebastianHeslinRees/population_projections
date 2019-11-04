
output_date <- "19-10-25_1457"
population <- readRDS(paste0("outputs/trend/2018/population", output_date, ".rds"))
births <- readRDS(paste0("outputs/trend/2018/births", output_date, ".rds"))
deaths <- readRDS(paste0("outputs/trend/2018/deaths", output_date, ".rds"))
int_in <- readRDS(paste0("outputs/trend/2018/int_in", output_date, ".rds"))
int_out <- readRDS(paste0("outputs/trend/2018/int_out", output_date, ".rds"))
dom_in <- readRDS(paste0("outputs/trend/2018/dom_in", output_date, ".rds"))
dom_out <- readRDS(paste0("outputs/trend/2018/dom_out",output_date,".rds"))


datastore_outputs(population, births, deaths, int_in, int_out, dom_in, dom_out,
                  output_dir = "outputs/trend/2018/datastore", file_name = "datastore_file.xlsx")

datastore_outputs <- function(population, births, deaths, int_in, int_out, dom_in, dom_out,
                              output_dir, file_name){

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

  #TODO: Make this better
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
  dir.create(output_dir, recursive = T, showWarnings = F)

  data.table::fwrite(persons, paste0(output_dir,"/persons.csv"))
  data.table::fwrite(female, paste0(output_dir,"/females.csv"))
  data.table::fwrite(male, paste0(output_dir,"/males.csv"))
  data.table::fwrite(components, paste0(output_dir,"/components.csv"))

  #excel
  #TODO: Do the folder setting better - VBA needs whole path, not relative
  bas_file <- "documentation/templates/datastoreVBA.bas"
  output_separated <- strsplit(output_dir, "/")[[1]]
  rprojroot::find_root_file(output_separated, criterion = rprojroot::is_git_root)
  vba <- create_VBA_script(datastore_folder, file_name)
  data.table::fwrite(vba, bas_file, col.names=F, quote=F)
  file.remove("documentation/templates/temp_file.xlsm")
  shell.exec("M:/Projects/population_projections/documentation/templates/run_excel_vba.bat")

  message("excel process running")

}

#--------------------------------------

create_VBA_script <- function(output_dir, file_name){

  x <- data.frame(a = c(

    "Attribute VB_Name = \"datastoreVBA\"",

    "Sub run_it()",

    "Set This_wkb = Application.ThisWorkbook",
    "Set wkb = Workbooks.Add",
    "Call run_open",

    "Application.DisplayAlerts = False",
    "Worksheets(\"Sheet1\").Delete",
    "Application.DisplayAlerts = True",

    paste0("wkb.SaveAs fileName:=\"",output_dir,"\\",file_name,"\""),
    "wkb.Close SaveChanges:=False",

    "End Sub",

    "Sub run_open()",

    paste0("Call open_copy_csv(\"",output_dir,"\\persons.csv\", \"persons\")"),
    paste0("Call open_copy_csv(\"",output_dir,"\\females.csv\", \"females\")"),
    paste0("Call open_copy_csv(\"",output_dir,"\\males.csv\", \"males\")"),
    paste0("Call open_copy_csv(\"",output_dir,"\\components.csv\", \"components of change\")"),

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

