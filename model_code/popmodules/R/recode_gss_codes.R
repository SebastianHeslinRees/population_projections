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
  
  #Change 2011-2018, not sure exactly when
  recoding_2011 <- c("E07000001" = "E06000056", #Mid Bedfordshire => Central Bedfordshire
                     "E07000002" = "E06000055", #Bedford => Bedford
                     "E07000003" = "E06000056", #South Bedfordshire => Central Bedfordshire
                     "E07000013" = "E06000050", #Chester => Cheshire and West Chester
                     "E07000014" = "E06000049", #Congleton => Chesire East
                     "E07000015" = "E06000049", #Crewe and Nantwich => Chesire East
                     "E07000016" = "E06000050", #Ellesmere Port & Neston => Cheshire and West Chester
                     "E07000017" = "E06000049", #Macclesfield => Chesire East
                     "E07000018" = "E06000050", #Vale Royal  => Cheshire and West Chester
                     "E07000019" = "E06000052", #Caradon => Cornnwall
                     "E07000020" = "E06000052", #Carrick => Cornnwall
                     "E07000021" = "E06000052", #Kerrier => Cornnwall
                     "E07000022" = "E06000052", #North Cornwall => Cornnwall
                     "E07000023" = "E06000052", #Penwith => Cornnwall
                     "E07000024" = "E06000052", #Restormel => Cornnwall
                     "E07000025" = "E06000053", #Isles of Scilly => Isles of Scilly
                     "E07000054" = "E06000047", #Chester-le-Street  => County Durham
                     "E07000055" = "E06000047", #Derwentside => County Durham
                     "E07000056" = "E06000047", #Durham => County Durham
                     "E07000057" = "E06000047", #Easington => County Durham
                     "E07000058" = "E06000047", #Sedgefield => County Durham
                     "E07000059" = "E06000047", #Teesdale => County Durham
                     "E07000060" = "E06000047", #Wear Valley => County Durham
                     "E06000057" = "E06000048", #Northumberland => Christchurch
                     "E07000242" = "E07000097", #FIXME East Hertfordshire => East Hertfordshire (we're using obsolete code)
                     "E07000243" = "E07000101", #FIXME Stevenage => Stevenage (we're using obsolete code)
                     "E08000037" = "E08000020", #FIXME Gateshead => Gateshead (we're using obsolete code)
                     "E07000157" = "E06000048", #Alnwick => Christchurch
                     "E07000158" = "E06000048", #Berwick-upon-Tweed => Christchurch
                     "E07000159" = "E06000048", #Blyth Valley => Christchurch
                     "E07000160" = "E06000048", #Castle Morpeth => Christchurch
                     "E07000161" = "E06000048", #Tynedale => Christchurch
                     "E07000162" = "E06000048", #Wansbeck => Christchurch
                     "E07000182" = "E06000051", #Bridgnorth => Shropshire
                     "E07000183" = "E06000051", #North Shropshire => Shropshire
                     "E07000184" = "E06000051", #Oswestry => Shropshire
                     "E07000185" = "E06000051", #Shrewsbury and Atcham => Shropshire
                     "E07000186" = "E06000051", #South Shropshire => Shropshire
                     "E07000230" = "E06000054", #Kennet  => Wiltshire
                     "E07000231" = "E06000054", #North Wiltshire => Wiltshire
                     "E07000232" = "E06000054", #Salisbury => Wiltshire
                     "E07000233" = "E06000054", #West Wiltshire =>Wiltshire
                     "E07000240" = "E07000100", #FIXME St Albans => St Albans (we're using obsolete code)
                     "E07000241" = "E07000104") #FIXME Welwyn Hatfield => Welwyn Hatfield (we're using obsolete code)
  
  #april 2019 geography changes
  recoding_2019 <- c("E06000028" = "E06000058", #Bournemouth => Bournemouth, Christchurch & Poole
                     "E06000029" = "E06000058", #Poole => Bournemouth, Christchurch & Poole
                     "E07000048" = "E06000058", #Christchurch => Bournemouth, Christchurch & Pool
                     
                     "E07000049" = "E06000059", #East Dorset => Dorset         
                     "E07000050" = "E06000059", #North Dorset => Dorset     
                     "E07000051" = "E06000059", #Purbeck => Dorset             
                     "E07000052" = "E06000059", #West Dorset => Dorset          
                     "E07000053" = "E06000059", #Weymouth and Portland => Dorset
                     
                     "E07000205" = "E07000244", #Suffolk Col => astaEast Suffolk
                     "E07000206" = "E07000244", #Waveney => East Suffolk
                     
                     "E07000201" = "E07000245", #Forest Heath => West Suffolk
                     "E07000204" = "E07000245", #St Edmundsbury => West Suffolk
                     
                     "E07000190" = "E07000246", #Taunton Deane => Somerset West and Taunton
                     "E07000191" = "E07000246") #West Somerset  => Somerset West and Taunton
  
  #april 2020 changes
  recoding_2020 <- c("E07000004" = "E06000060", #Aylesbury Vale => Buckinghamshire
                     "E07000005" = "E06000060", #Chiltern => Buckinghamshire
                     "E07000006" = "E06000060", #South Bucks => Buckinghamshire
                     "E07000007" = "E06000060") #Wycombe => Buckinghamshire
  
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

