#' Recode LA GSS Codes to a later geography
#'
#' Switches any gss codes to a later vintage
#'
#' Aggregates data where rows with the same gss code are present
#'
#' User must define the column containing the gss_code and the columns
#' which contain data. Data columns must be numeric or integer.
#'
#' @param df_in A data frame containing gss_codes and data.
#' @param col_geog A string. The column which contains gss codes (defaults to
#'   \code{gss_code}).
#' @param data_cols A string or character vector. The column(s) that contain the
#'   data to be aggregated. Defaults to last column of input dataframe.
#' @param fun List. Function to be applied in aggregating data. Passed to the
#'   .funs argument of \code{dplyr::summarise_all}. Default \code{list(sum)}.
#' @param recode_to_year Numeric. Conform to geography in which year. Default
#'   \code{2013} (i.e. census geography).
#' @param aggregate_data Logical. If set to true multiple instances of the same
#'   gss code will be aggregated using the function specified in \code{fun} parammeter.
#'   Default to \code{TRUE}.
#' @recode_gla_codes Logical. If set to \code{TRUE} recodes to the codes used in
#'  2018-based and earlier projections. This is introduced for backward
#'  compatibility and doesn't correspond to any single vintage year.
#'  Default \code{FALSE}.
#'
#' @return The input dataframe with gss codes changed and data aggregated
#'
#' @import dplyr
#' @importFrom dtply lazy_dt
#' @importFrom assertthat assert_that
#' 
#' @export

recode_gss_codes <- function(df_in,
                             col_geog="gss_code",
                             data_cols = last(names(df_in)),
                             fun = list(sum),
                             recode_to_year = 2013,
                             aggregate_data = TRUE,
                             recode_gla_codes = FALSE){

  #prepare input dataframe
  df <- df_in %>%
    as.data.frame() %>%
    rename("gss_code" = !!col_geog)
  
  col_aggregation <- setdiff(names(df),data_cols)
  
  #Recode to old codes - for backwards compatibility
  if(recode_gla_codes){
    df <- .recode_gss_to_gla(df, col_geog, col_aggregation, fun)
    return(df)
  }
  
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
  } else {
    df <- as.data.frame(df) %>%
      rename(!!col_geog := "gss_code")
  }
  
  return(df)
  
}



.recode_gss_to_gla <- function(df, col_geog, col_aggregation, fun){
  
  #This is the recoding that was used for the 2018 and earlier projections
  #It is not a set of codes that ever existed simultaneously - its a 
  #lowest common denominator approach
  #Its mostly census (ie 2013 codes) with a couple of differences
  
  recoding <- c("E07000001" = "E06000056",
                "E07000002" = "E06000055",
                "E07000003" = "E06000056",
                "E07000013" = "E06000050",
                "E07000014" = "E06000049",
                "E07000015" = "E06000049",
                "E07000016" = "E06000050",
                "E07000017" = "E06000049",
                "E07000018" = "E06000050",
                "E07000019" = "E06000052",
                "E07000020" = "E06000052",
                "E07000021" = "E06000052",
                "E07000022" = "E06000052",
                "E07000023" = "E06000052",
                "E07000024" = "E06000052",
                "E07000025" = "E06000053",
                "E07000054" = "E06000047",
                "E07000055" = "E06000047",
                "E07000056" = "E06000047",
                "E07000057" = "E06000047",
                "E07000058" = "E06000047",
                "E07000059" = "E06000047",
                "E07000060" = "E06000047",
                "E06000057" = "E06000048",
                "E07000242" = "E07000097",
                "E07000243" = "E07000101",
                "E08000037" = "E08000020",
                "E07000157" = "E06000048",
                "E07000158" = "E06000048",
                "E07000159" = "E06000048",
                "E07000160" = "E06000048",
                "E07000161" = "E06000048",
                "E07000162" = "E06000048",
                "E07000182" = "E06000051",
                "E07000183" = "E06000051",
                "E07000184" = "E06000051",
                "E07000185" = "E06000051",
                "E07000186" = "E06000051",
                "E07000230" = "E06000054",
                "E07000231" = "E06000054",
                "E07000232" = "E06000054",
                "E07000233" = "E06000054",
                "E07000240" = "E07000100",
                "E07000241" = "E07000104")

    df <- df %>% 
      mutate(gss_code = recode(gss_code, !!!recoding)) %>% 
      dtplyr::lazy_dt() %>% 
      group_by_at(col_aggregation) %>%
      summarise_all(.funs=fun) %>%
      data.frame() %>%
      rename(!!col_geog := "gss_code")
    
    return(df)
}
