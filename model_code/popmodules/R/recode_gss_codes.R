#' Recode LA GSS Codes to 2011 geography
#'
#' Switches any non-2011 gss codes to Census 2011 gss codes Uses a hard-coded
#' list of changes
#'
#' Aggregates data where rows with the same gss code are present
#'
#' User must define the column containing the gss_code and the columns to group
#' by
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_geog A string. The column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param data_cols A string or character vector. The column(s) that contain the
#'   data to be aggregated. Defaults to last column of input dataframe.
#' @param recode_to_year Numeric. Conform to geography in which year. Default
#'   \code{2011}.
#' @param aggregate_data Not currently used
#'
#' @return The input dataframe with gss codes changed and data aggregated
#'
#' @import dplyr
#' @export

recode_gss_codes <- function(df_in,
                             col_geog="gss_code",
                             data_cols = last(names(df_in)),
                             fun = list(sum),
                             recode_to_year = 2012,
                             aggregate_data = TRUE){

  #prepare input dataframe
  df <- df_in %>%
    as.data.frame() %>%
    rename("gss_code" = !!col_geog)
  
  col_aggregation <- setdiff(names(df),data_cols)
  
  #prepare recoding
  recode_years <- c(2009,2012,2013,2018,2019,2020)
  assertthat::assert_that(recode_to_year %in% recode_years,
                          msg=paste("recode_to_year variable must be one of",
                                    2009,2012,2013,2018,2019,2020))
  
  recode_merges <- list()
  recode_name_changes <- list()
  
  code_changes <- readRDS("input_data/lookup/district_changes_clean.rds") %>%
    select(changed_to_code, changed_from_code, year, split, merge)
  
  #splits
  #there have thus far been 2 splits, both in 2013
  #both were minor alterations to boundaries
  #northumberland = 1 bungalow
  #east hertfordshire = part of a row of houses
  #both can effectivley be treated as code changes with no transer of
  #population.
  #Therefore ignore E08000020 to E06000057
  #and ignore E07000097 to E06000243
  
  code_changes <- code_changes %>% 
    filter(!(changed_from_code == "E08000020" & changed_to_code == "E06000057")) %>%
    filter(!(changed_from_code == "E07000097" & changed_to_code == "E07000243"))
  
  #for each year make the changes a named vector inside a list
  #the x=X is necessary becuase the vector must not be empty
  for(yr in recode_years){
    
    recode_merges[[yr]] <- filter(code_changes, year == yr,
                                  merge == TRUE)
    
    recode_merges[[yr]] <- c(setNames(as.character(recode_merges[[yr]]$changed_to_code),
                                      recode_merges[[yr]]$changed_from_code), "x"="X")
    
    
    recode_name_changes[[yr]] <- filter(code_changes, year == yr,
                                        merge == FALSE)
    
    recode_name_changes[[yr]] <- c(setNames(as.character(recode_name_changes[[yr]]$changed_to_code),
                                            recode_name_changes[[yr]]$changed_from_code), "x"="X")
  }
  
  #april 2020 changes - not yet in code history database
  recode_merges[[2020]] <- c("E07000004" = "E06000060", #Aylesbury Vale => Buckinghamshire
                             "E07000005" = "E06000060", #Chiltern => Buckinghamshire
                             "E07000006" = "E06000060", #South Bucks => Buckinghamshire
                             "E07000007" = "E06000060") #Wycombe => Buckinghamshire
  recode_name_changes[[2020]] <- c("x" = "X")
  
  recode_years <- recode_years[recode_years <= recode_to_year]

  #in a loop so that codes that are changed and then changed again are picked-up
  for(yr in recode_years){
    
    #name changes
    df <- df %>% 
      mutate(gss_code = recode(gss_code, !!!recode_name_changes[[yr]])) 
    
    #merges
    df <- df %>% 
      mutate(gss_code = recode(gss_code, !!!recode_merges[[yr]])) 
  }
  
  if(aggregate_data){
    df <- df %>% 
      dtplyr::lazy_dt() %>%
      group_by_at(col_aggregation) %>%
      summarise_all(.funs = fun) %>%
      as.data.frame() %>%
      rename(!!col_geog := "gss_code")
  }
  
  return(df)
  
}



