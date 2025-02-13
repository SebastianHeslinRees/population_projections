% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/household_model_ons.R
\name{household_model_ons}
\alias{household_model_ons}
\title{Run the 2016/2018 ONS household model}
\usage{
household_model_ons(
  population,
  stage1_file_path,
  stage2_file_path,
  communal_est_pop_path,
  first_proj_yr
)
}
\arguments{
\item{population}{A data frame containing population data.}

\item{stage1_file_path}{String. Path to file containing DCLG stage 1
household inputs.}

\item{stage2_file_path}{String. Path to file containing DCLG stage 2
household inputs.}

\item{communal_est_pop_path}{String. Path to file containing communal
establishment population rates for the each year of the projection period.}

\item{first_proj_yr}{Numeric. First year of the model projection. Elderly
communal populations are adjusted after this date.}
}
\value{
A list containing 2 lists: Stage 1 outputs and Stage 2 outputs.
}
\description{
Run stages 1 and 2 of the ONS household model and return outputs.
}
