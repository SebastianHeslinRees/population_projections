#' Output a trend population projection
#'
#' Write csv, rds and xlsx model outputs from the \code{trend_core} cohort component
#' model. Input is taken from the \code{arrange_trend_core_outputs} function.
#'
#' @param projection A list. The model outputs from \code{arrange_trend_core_outputs}
#' @param output_dir String. The output location folder path
#' @param write_excel Logical. Should the Excel outputs be written
#' @param n_csv_elements Numeric. The function will output csv files for elements
#'  1:\code{n_csv_elements} of the \code{projection} list.
#'
#' @import dplyr
#' @import popmodules
#' @importFrom stringr str_detect
#' @importFrom data.table fwrite
#' @importFrom tidyr pivot_wider

output_projection <- function(projection, output_dir, write_excel, n_csv_elements) {

  #RDS
  lapply(seq_along(projection),
         function(i) saveRDS(projection[[i]], paste0(output_dir, names(projection)[[i]], ".rds"), compress = "gzip")) %>%
    invisible()

  #CSV
  csv_dir <- paste0(output_dir,"csv/")
  dir.create(csv_dir)

  make_csvs <- function(data, name_stub){

    data_col <- names(data)[ncol(data)]
    cols <- setdiff(names(data), data_col)
    cols <- c("gss_code", "gss_name", cols[cols!="gss_code"])

    data <- left_join(data, get_gss_names(), by="gss_code") %>%
      filter(year >= 2011) %>%
      select_at(c(cols, data_col))

    if(str_detect(name_stub, "births_by_mothers_age")){

      b_m_a <- filter(data, sex == "female", age %in% 15:49) %>%
        rename(rounded = data_col) %>%
        mutate(rounded = round(rounded, 3)) %>%
        pivot_wider(names_from = year, values_from = rounded)

      fwrite(b_m_a, paste0(name_stub,".csv"))

    } else {

      female <- filter(data, sex == "female") %>%
        rename(rounded = data_col) %>%
        mutate(rounded = round(rounded, 3)) %>%
        pivot_wider(names_from = year, values_from = rounded)

      male <- filter(data, sex == "male")  %>%
        rename(rounded = data_col) %>%
        mutate(rounded = round(rounded, 3))%>%
        pivot_wider(names_from = year, values_from = rounded)

      persons <- data %>%
        mutate(sex = "persons") %>%
        rename(value = !!data_col) %>%
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(value = round(value, 3))%>%
        pivot_wider(names_from = year, values_from = value)

      fwrite(female, paste0(name_stub,"_female.csv"))
      fwrite(male, paste0(name_stub,"_male.csv"))
      fwrite(persons, paste0(name_stub,"_persons.csv"))
    }

  }

  lapply(seq(n_csv_elements),
         function(i) make_csvs(projection[[i]], paste0(csv_dir, names(projection)[[i]]))) %>%
    invisible()

  #Excel
  trend_datastore_outputs(population = projection$population,
                    births = projection$births,
                    deaths = projection$deaths,
                    int_in = projection$int_in,
                    int_out = projection$int_out,
                    dom_in = projection$dom_in,
                    dom_out = projection$dom_out,
                    output_dir = output_dir,
                    excel_file_name = paste0("datastore_",Sys.Date(),".xlsx"),
                    write_excel = write_excel)
}
