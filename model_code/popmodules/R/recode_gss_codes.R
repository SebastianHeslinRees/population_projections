#' Recode LA GSS Codes to 2011 geography
#'
#' Switches any non-2011 gss codes to Census 2011 gss codes
#' Uses a hard-coded list of changes
#'
#' Aggregates data where rows with the same gss code are present
#'
#' User must define the column containing the gss_code and the columns to
#' group by
#'
#' @param df A data frame containing gss_codes and data.
#' @param col_aggregation A string or character vector giving column names for
#'   levels of data aggregations: e.g. geographic area, age/age band, sex.
#' @param col_geog A character vector. The column which contains gss codes
#' (defaults to \code{gss_code})
#' @param fun A list. A function to summarise the data. Must be provided in a list.
#' Defaults to \code{list(sum)}
#'
#' @return The input dataframe with gss codes changed and data aggregated
#'
#' @import dplyr
#' @importFrom data.table setDF setDT setkeyv
#' @export


recode_gss_to_2011 <- function(df, col_geog="gss_code", col_aggregation, fun=list(sum)){


  #TODO Add something that flags or deals with 2019 changes to LA codes

  df <- ungroup(df) %>%
    rename("gss_code" = col_geog)

  i <- which(col_aggregation == col_geog)
  col_aggregation[i] <- "gss_code"

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

  # FIXME: using data.table here means that the input df is changed by
  # reference, i.e. if you set x <- recode_gss_to_2011(z), then both x and z
  # will contain the recoded data frame. Is this ok? What should we do about it? --CF
  data.table::setDT(df)
  df[gss_code %in% names(recoding), gss_code := recode(gss_code, !!!recoding)]
  df <- df[, lapply(.SD, fun[[1]]), by = col_aggregation]
  data.table::setkeyv(df, col_aggregation)
  data.table::setDF(df)
  df <- rename(df, !!col_geog := "gss_code")

  return(df)

  if(FALSE) { # tidyverse equivalent
    df <- mutate(df, gss_code = recode(gss_code, !!!recoding))

    df <- rename(df, !!col_geog := "gss_code")

    df_2 <- group_by_at(df, col_aggregation) %>%

      summarise_all(.funs=fun) %>%
      ungroup()

    return(df_2)
  }


}

