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
#' @param col_geog A character vector. The column which contains gss codes
#'   (defaults to \code{gss_code}).
#' @param col_aggregation A string or character vector giving column names for
#'   levels of data aggregations: e.g. geographic area, age/age band, sex.
#' @param fun A list. A function to summarise the data. Must be provided in a
#'   list. Defaults to \code{list(sum)}.
#' @param aggregate_data Logical. Whether to reaggregate the data after
#'   recoding. If TRUE, all other columns will be combined using the new levels
#'   and the function specified ny \code{fun}. If FALSE nothing will be done and
#'   any newly duplicated levels will remain.
#' @param recode_to_year Numeric. Conform to geography in which year.
#'   Default \code{2011}.
#'
#' @return The input dataframe with gss codes changed and data aggregated
#'
#' @import dplyr
#' @importFrom data.table setDF setDT setkeyv
#' @export


recode_gss_codes <- function(df_in, col_geog="gss_code",
                               col_aggregation, fun=list(sum),
                               aggregate_data = TRUE,
                               recode_to_year = 2011){
  
  df <- df_in %>%
    as.data.frame() %>%
    rename("gss_code" = !!col_geog)
  
  i <- which(col_aggregation == col_geog)
  col_aggregation[i] <- "gss_code"
  
  recoding_2011 <- c("E07000001" = "E06000056",
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
  
  #april 2019 geography changes
  recoding_2019 <- c("E06000028" = "E06000058",
                     "E06000029" = "E06000058",
                     "E07000048" = "E06000058",
                     
                     "E07000049" = "E06000059",         
                     "E07000050" = "E06000059",         
                     "E07000051" = "E06000059",             
                     "E07000052" = "E06000059",          
                     "E07000053" = "E06000059",
                     
                     "E07000205" = "E07000244",
                     "E07000206" = "E07000244",
                     
                     "E07000201" = "E07000245",
                     "E07000204" = "E07000245",
                     
                     "E07000190" = "E07000246",
                     "E07000191" = "E07000246")
  
  #april 2020 changes
  recoding_2020 <- c("E07000004" = "E06000060",
                     "E07000005" = "E06000060",
                     "E07000006" = "E06000060",
                     "E07000007" = "E06000060")
  
  recoding <- recoding_2011
  if(recode_to_year == 2019){recoding <- c(recoding, recoding_2019)}
  if(recode_to_year == 2020){recoding <- c(recoding, recoding_2019, recoding_2020)}
  
  df <- df %>% 
    dtplyr::lazy_dt() %>% 
    mutate(gss_code = recode(gss_code, !!!recoding)) %>% 
    group_by_at(col_aggregation) %>%
    summarise_all(.funs=fun) %>%
    as.data.frame() %>%
    rename(!!col_geog := "gss_code")
  
}

