#' Check that paths in a config list point to actual rds files
#' 
#' Given a set of input parameters run a housing-led for small areas
#' 
#' @param config_list A List. A model configuration list.
#' 
#' @import stringr
#'  
#' @export

.validate_input_paths <- function(config_list){
  
  paths <- names(config_list)[str_detect(names(config_list), "path")]
  
  paths <- config_list[names(config_list) %in% paths]
  
  lapply(seq(paths),
         function(i) {
           if(!is.null(paths[[i]])){
             assert_that(file.exists(paths[[i]]),
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(paths[[i]], split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(names(paths)[[i]], ": ", paths[[i]], "\nFile is not .rds format"))
           }
         })
  
  if("out_migration" %in% names(config_list)){
    lapply(config_list$out_migration,
           function(x) {
             assert_that(file.exists(x$path),
                         msg = paste0(x$path, "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(x$path, "\nFile is not .rds format"))
           })
  }
  
  if("in_migration" %in% names(config_list)){
    lapply(config_list$in_migration,
           function(x) {
             assert_that(file.exists(x$path),
                         msg = paste0(x$path, "\nFile does not exist at specified path"))
             
             file_ext <- tolower(strsplit(x$path, split="\\.")[[1]][[2]])
             assert_that(file_ext == "rds",
                         msg = paste0(x$path, "\nFile is not .rds format"))
           })
  }
  
  if("constraint_list" %in% names(config_list) & !is.null(config_list$constraint_list$constraint_path)){
    assert_that(dir.exists(config_list$constraint_list$constraint_path),
                msg = "constraint folder does not exist at specified path")
  }
  
  
  invisible()
}

.bind_and_arrange <- function(x, lookup){
  
  join_by <- intersect(names(x), c("gss_code", "area_code"))
  
  suppressMessages(
    z <- rbindlist(x, use.names = TRUE) %>% 
      left_join(lookup, by = join_by) %>% 
      data.frame()
  )
  
  arrange_by <- intersect(c("gss_code", "la_name", "area_code", "area_name", "year", "sex", "age"),
                          names(z))
  
  select_by <- c(arrange_by, setdiff(names(z), arrange_by))
  
  z %>% 
    lazy_dt() %>% 
    arrange_at(arrange_by) %>% 
    select_at(select_by) %>%
    as_tibble() %>% 
    mutate(across(where(is.numeric) & !intersect(names(z), c("year","age")),
                  ~round(.x, digits=8))) %>% 
    data.frame()
}

.make_borough <- function(x){
  nm <- last(names(x))
  cols <- intersect(c("gss_code", "la_name", "year", "sex", "age"),
                    names(x))
  x <- x %>% 
    rename(value = !!nm) %>% 
    group_by(across(cols)) %>% 
    summarise(!!nm := sum(value), .groups = 'drop_last') %>% 
    data.frame()
}

.remove_ce_popn <- function(popn, ce_popn, popn_col = "popn",
                            col_aggregation = c("area_code", "sex", "age")){
  
  # hh_popn <- group_by(ce_popn, across(col_aggregation)) %>% 
  #   summarise(ce_popn = sum(ce_popn)) %>%  
  #   data.frame() %>% 
  #   right_join(popn, by = col_aggregation) %>% 
  #   mutate(household_popn = !!sym(popn_col) - ce_popn) %>% 
  #   check_negative_values("household_popn")

  hh_popn <- popn %>%  
    left_join(ce_popn, by = col_aggregation) %>% 
    mutate(household_popn = !!sym(popn_col) - ce_popn) %>% 
    check_negative_values("household_popn")	 
  
}

.add_ce_popn <- function(hh_popn, ce_popn, popn_col = "popn",
                         col_aggregation = c("are_code", "sex", "age")){
  
  ce_popn <- group_by(ce_popn, across(col_aggregation)) %>% 
    summarise(ce_popn = sum(ce_popn)) %>%  
    data.frame() %>% 
    right_join(hh_popn, by = col_aggregation) %>% 
    mutate(!!popn_col := !!sym(popn_col) + ce_popn) %>% 
    check_negative_values(popn_col) %>% 
    select(-ce_popn)
}

.standardise_df <- function(x, col_geog, data_col=NULL, col_agg = c("year", "gss_code", col_geog, "age", "sex")){
  cols <- intersect(c(col_agg, data_col), names(x))
  select(x, all_of(cols)) %>% 
    rename(area_code = all_of(col_geog)) %>% 
    return()
}

.rename_geog_cols <- function(x, code_col, name_col){
  if("area_code" %in% names(x)){
    x <- rename(x, !!code_col := area_code)
  }
  if("area_name" %in% names(x)){
    x <- rename(x, !!name_col := area_name)
  }
  return(x)
}

.add_gss_code <- function(x, lookup){
  if(!"gss_code" %in% names(x)){
    x <- left_join(x,
                   distinct(select(lookup, gss_code, area_code)),
                   by = "area_code")
  }
  return(x)
}

.camel <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
}


#' DT implementation of tidyr::unnest()
#' https://www.r-bloggers.com/2019/10/much-faster-unnesting-with-data-table/
#' 
#' @param tbl Dataframe
#' @param col String
#' 
#' @import rlang
#' @import data.table
#'  
#' @export

.unnest_dt <- function(tbl, col) {
  
  tbl <- as.data.table(tbl)
  
  col <- ensyms(col)
  
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  
  tbl <- as.data.table(tbl)
  
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  
  data.frame(tbl)
}