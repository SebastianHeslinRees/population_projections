#' Project mortality rates forward based on a national trend
#'
#' Applies the national rate of change in mortality to a set of local
#' authority AMSRs to produce a mortality probability trajectory.
#'
#' @param jump_off_rates Dataframe. A set of ASMRs for the first projection year.
#' @param future_mortality Dataframe. A national-level ASMR trajectory.
#' @param first_proj_yr Integer. The first projection year.
#' @param n_proj_year Integer. Number of years to project
#' @param npp_var Character. Mortality trend variant. Either \code{2018_principal},
#'   \code{2018_high}, \code{2018_low},  \code{2016_principal}, \code{2016_high}, or
#'   \code{2016_low}. Defaults to \code{2018_principal}.
#'
#' @return A data frame of mortality probabilities for all projection years.
#'
#' @export

project_mortality_rates <- function(jump_off_rates, future_mortality, first_proj_yr, n_proj_yr, npp_var="2018_principal"){

  #Test/validate
  check_validate_proj_mort_rates(jump_off_rates, future_mortality, first_proj_yr, n_proj_yr, npp_var)

  final_projection_year <- first_proj_yr + n_proj_yr

  future_mortality_probs <- future_mortality %>%
    filter(variant == npp_var) %>%
    filter(year > first_proj_yr) %>%
    filter(year <= final_projection_year) %>%
    arrange(year) %>%
    group_by(sex, age) %>%
    mutate(change = change + 1,
           cumprod = cumprod(change))%>%
    ungroup() %>%
    arrange(age,sex,year)%>%
    left_join(select(jump_off_rates, -year), by=c("sex","age")) %>%
    mutate(rate = cumprod*death_rate) %>%
    select(gss_code, sex, age, year, rate) %>%
    arrange(gss_code,sex,age,year)

  return(future_mortality_probs)

}


#--------------------------------------------------------------------

# Function to check that the input to project_mortality_rates is all legal

check_validate_proj_mort_rates <- function(jump_off_rates,
                                           future_mortality,
                                           first_proj_yr,
                                           n_proj_yr,
                                           npp_var) {

  # test input parameters are of the correct type
  assert_that(is.data.frame(jump_off_rates),
              msg="jump_off_rates expects a data frame as input")
  assert_that(is.data.frame(future_mortality),
              msg="future_mortality expects a data frame as input")
  assert_that(is.numeric(first_proj_yr),
              msg="first_proj_year expects an integer as input")
  assert_that(is.numeric(n_proj_yr),
              msg="n_proj_yr expects an integer as input")
  assert_that(is.character(npp_var),
              msg="npp_var expects character input")

  #TODO find out why these fall over
  #validate_population(jump_off_rates)
  validate_population(future_mortality, col_aggregation = c("sex","age","year","variant"))
  #validate_join_population(jump_off_rates, future_mortality)


}


