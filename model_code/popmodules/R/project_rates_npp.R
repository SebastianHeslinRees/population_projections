#' Project mortality/fertility rates forward based on a national trend
#'
#' Applies the national rate of change in mortality/fertility to a set of local
#' authority ASMRs/ASFRs to produce a mortality/fertility probability trajectory.
#' This takes the projection for the first projection year as an input, and
#' extends it to the required number of projection years
#'
#' @param jump_off_rates Dataframe. A set of LA/age/sex rates for the first
#'   projection year
#' Likely calculated from \code{scaled_mortality_curve()} or \code{scaled_fertility_curve()}
#' @param rate_col Character. The column in the jump_off_rates dataframe
#'   containing the rates.
#' @param rate_trajectory_filepath Character. The file path for the national-level
#'   ASMR/ASFR trajectory.
#' @param first_proj_yr Integer. The first projection year.
#' @param n_proj_yr Integer. Number of years to project
#' @param npp_var Character. NPP trend variant. Either \code{2018_principal},
#'   \code{2018_high}, \code{2018_low},  \code{2016_principal}, \code{2016_high}, or
#'   \code{2016_low}. Defaults to \code{2018_principal}.
#'
#' @return A data frame of fertility probabilities for all projection years.
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export

project_rates_npp <- function(jump_off_rates,
                              rate_col,
                              rate_trajectory_filepath,
                              first_proj_yr,
                              n_proj_yr,
                              npp_var="2018_principal"){

  rate_trajectory <- readRDS(rate_trajectory_filepath)

  #Test/validate
  check_validate_proj_rates_npp(jump_off_rates, rate_col, rate_trajectory, first_proj_yr, n_proj_yr, npp_var)
  final_projection_year <- first_proj_yr + n_proj_yr -1
  jump_off_year <- max(jump_off_rates$year)
  assert_that(all((jump_off_year + 1):final_projection_year %in% rate_trajectory$year))

  backseries <- filter(jump_off_rates, year < jump_off_year)
  jump_off_rates <- filter(jump_off_rates, year == jump_off_year)
  # calculate the rate changes relative to the first year, and apply this to the
  # jump off rate to calulate the rest of the projection years

  rates <- rate_trajectory %>%
    filter(variant == npp_var) %>%
    filter(year > jump_off_year) %>%
    filter(year <= final_projection_year) %>%
    arrange(year) %>%
    group_by(sex, age) %>%
    mutate(change = change + 1,
           cumprod = cumprod(change))%>%
    ungroup() %>%
    arrange(age,sex,year) %>%
    left_join(select(jump_off_rates, -year), by=c("sex","age")) %>%
    rename(jump_rate = rate_col) %>%
    mutate(rate = cumprod*jump_rate) %>%
    select(gss_code, sex, age, year, rate) %>%
    rename(!!rate_col := rate) %>%
    rbind(jump_off_rates, backseries) %>%
    arrange(gss_code,sex,age,year) %>%
    as.data.frame() 
    

  validate_population(rates, col_aggregation = c("gss_code", "sex", "age", "year"),
                      col_data = rate_col)

  return(rates)

}


#--------------------------------------------------------------------

# Function to check that the input to project_fertility_rates is all legal

check_validate_proj_rates_npp <- function(jump_off_rates,
                                           rate_col,
                                           rate_trajectory,
                                           first_proj_yr,
                                           n_proj_yr,
                                           npp_var) {

  # test input parameters are of the correct type
  assert_that(is.data.frame(jump_off_rates),
              msg="jump_off_rates expects a data frame as input")
  assert_that(is.character(rate_col),
              msg="rate_col should be character")
  assert_that(is.string(rate_trajectory) | is.data.frame(rate_trajectory),
              msg="rate_trajectory expects a data frame as input")
  assert_that(is.numeric(first_proj_yr),
              msg="first_proj_year expects an integer as input")
  assert_that(is.numeric(n_proj_yr),
              msg="n_proj_yr expects an integer as input")
  assert_that(is.character(npp_var),
              msg="npp_var expects character input")

  #TODO find out why these fall over
  validate_population(jump_off_rates, col_data = rate_col)
  #validate_population(rate_trajectory, col_aggregation = c("sex","age","year","variant"))
  validate_join_population(jump_off_rates, rate_trajectory, cols_common_aggregation = c("sex", "age", "year"))


}


