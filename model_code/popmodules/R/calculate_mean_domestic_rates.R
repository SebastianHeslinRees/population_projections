#' Calculate average origin-destination domestic migration rates based on past
#' data
#'
#' Given a dataframe of domestic migration rates calculate the mean rate over a given number of years.
#' If the sum of the outrates for any age/sex/LA combination is greater than a defined cap then scale
#' all rates so that sum is equal to the cap.
#'
#' @param origin_destination_rates dataframe. Origin-destination flows by geography, year, sex and age.
#' @param last_data_year numeric. The last year of data on which to calculate
#'   averages.
#' @param n_years_to_avg numeric. The number of years to use in calculating averages.
#' @param col_rate character. The column in the \code{origin_destinatation_flows} dataframe containing the rates
#' @param rate_cap numeric. A value which calculated rates cannot exceed. Where rates exceed \code{rate_cap}
#'   they are limited to the value of \code{rate_cap}.
#'
#' @return A data frame of origin-destination migration probabilities by LA, year, sex and age.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom data.table setDT setkey
#' @importFrom utils capture.output
#'
#' @export

calculate_mean_domestic_rates <- function(origin_destination_rates,
                                          last_data_year,
                                          n_years_to_avg,
                                          col_rate = "rate",
                                          rate_cap = 0.8) {
  # validate input
  check_average_domestic_rate_input(origin_destination_rates, last_data_year, n_years_to_avg, col_rate)
  if(col_rate != "rate") {
    i <- names(origin_destination_rates) == col_rate
    names(origin_destination_rates)[i] <- "rate"
  }

  backseries_years <- (last_data_year - n_years_to_avg + 1):last_data_year
  assert_that(length(backseries_years) == n_years_to_avg)

  # data.table because it's so big
  data.table::setDT(origin_destination_rates)
  origin_destination_rates <- origin_destination_rates[year %in% backseries_years, ]
  origin_destination_rates <- origin_destination_rates[, .(rate = sum(rate)/n_years_to_avg),
                                                       .(gss_out, gss_in, age, sex)]
  data.table::setkey(origin_destination_rates, gss_out, sex, age)

  # tidyverse equivalent
  if(FALSE) {
    origin_destination_rates <- filter(origin_destination_rates, year %in% backseries_years)

    origin_destination_rates %>%
      group_by(gss_out, gss_in, age, sex) %>%
      summarise(rate = rate/n_years_to_avg)
  }

  outflow_sums <- origin_destination_rates[, .(total_rate = sum(rate)), .(gss_out, age, sex)]
  data.table::setkey(outflow_sums, gss_out, sex, age)

  if(any(outflow_sums$total_rate > rate_cap)) {
    ix <- outflow_sums$total_rate > rate_cap
    n <- sum(ix)

    warning(paste0(capture.output({
      print(paste(c("After averaging the backseries, average_domestic_migration rates created rates with sums exceeding the cap of", rate_cap,
                    "at", n, "aggregation levels - these will be scaled to sum to", rate_cap),
                  collapse = " "))
      if(sum(ix) < 30) {
        print("Values:")
        print(outflow_sums[ix,])
      } else {
        print("First 30 values:")
        print(outflow_sums[ix,][1:30,])
        print("Levels affected:")
        sapply(c("gss_out", "age", "sex"), function(col) {
          print("col:")
          print(unique(outflow_sums[ix, get(col)]))
        })
      }
    }), collapse = "\n"))

    outflow_sums$total_rate[outflow_sums$total_rate > rate_cap] <- rate_cap

    origin_destination_rates <- outflow_sums[origin_destination_rates][
      , rate := rate * total_rate/sum(rate), by = c("gss_out", "age", "sex")][
        , total_rate := NULL]

  }


  if(col_rate != "rate") {
    i <- names(origin_destination_rates) == "rate"
    names(origin_destination_rates)[i] <- col_rate
  }

  #check missing levels
  unmatched_levels <- setdiff(
    expand.grid(gss_out = unique(origin_destination_rates$gss_out),
                age = unique(origin_destination_rates$age),
                sex = unique(origin_destination_rates$sex),
                stringsAsFactors = FALSE),
    origin_destination_rates %>%
      dtplyr::lazy_dt() %>%
      select(gss_out, age, sex) %>%
      unique() %>%
      as.data.frame()
    )


  if(nrow(unmatched_levels) > 0) {
    warning(paste(c("calculate_mean_domestic_migration_rates found ", nrow(unmatched_levels), " aggregation levels with no net outmigration.",
                    " These levels will be absent from the output.")), sep=" ")
  }

  return(data.table::setDF(origin_destination_rates))

}




# ================================================================

# validate inputs for the above
check_average_domestic_rate_input <- function(origin_destination_rates, last_data_year, n_years_to_avg, col_rate) {

  assert_that(is.data.frame(origin_destination_rates),
              msg="calculate_mean_domestic_migration_rates expects that origin_destination_rates is a data frame")
  assert_that(is.count(last_data_year),
              msg="calculate_mean_domestic_migration_rates expects that last_data_year is a numeric")
  assert_that(is.count(n_years_to_avg),
              msg="calculate_mean_domestic_migration_rates expects that n_years_to_avg is a numeric")
  assert_that(is.character(col_rate),
              msg="calculate_mean_domestic_migration_rates expects that col_rate is a character")
  assert_that(col_rate %in% names(origin_destination_rates),
              msg="calculate_mean_domestic_migration_rates expects that col_rate is a column in component_data dataframe")

  backseries_years <- (last_data_year - n_years_to_avg + 1):last_data_year
  assert_that(all(backseries_years %in% origin_destination_rates$year),
              msg = paste(c("calculate_mean_domestic_migration_rates expects these years to be present in the origin-destination data:",
                            backseries_years), collapse=" "))
}
