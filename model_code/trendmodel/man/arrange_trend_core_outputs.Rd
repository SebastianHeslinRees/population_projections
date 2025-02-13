% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange_trend_core_outputs.R
\name{arrange_trend_core_outputs}
\alias{arrange_trend_core_outputs}
\title{Arrange outputs from the \code{trend_core} function into inputs for the
\code{trend_model_outputs} function}
\usage{
arrange_trend_core_outputs(
  projection,
  population,
  births,
  deaths,
  int_out,
  int_in,
  dom_in,
  dom_out,
  upc_mye,
  popn_adjustment,
  fertility_rates,
  mortality_rates,
  int_out_rates_flows,
  first_proj_yr,
  last_proj_yr
)
}
\arguments{
\item{projection}{A list. The output list from the \code{trend_core} function}

\item{population}{Dataframe. Backseries population data}

\item{births}{Dataframe. Backseries births data}

\item{deaths}{Dataframe. Backseries deaths data}

\item{int_out}{Dataframe. Backseries international out migration data}

\item{int_in}{Dataframe. Backseries international in migration data}

\item{dom_in}{Dataframe. Backseries domestic in migration data}

\item{dom_out}{Dataframe. Backseries domestic out migration data}

\item{upc_mye}{Dataframe or NULL. Backseries UPC data}

\item{popn_adjustment}{Dataframe or NULL. Population adjustment data for
the projection period. Used for UPC or other adjustments}

\item{fertility_rates}{Dataframe. Model fertility rates}

\item{mortality_rates}{Dataframe. Model mortality rates}

\item{int_out_rates_flows}{Dataframe. Model international out migration rates}

\item{first_proj_yr}{Numeric. First projection year}

\item{last_proj_yr}{Numeric. Last projection year}
}
\value{
A list where each element is a data frame containing data for each year
  of the projection and the backseries.
}
\description{
The \code{trend_core} outputs a list for each year of the projection.
Each list is a list of components. This function rearranges those outputs
into a list of dataframes of components.
}
