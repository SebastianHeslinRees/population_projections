% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regross.R
\name{regross}
\alias{regross}
\title{Regross}
\usage{
regross(
  base_in,
  base_out,
  target_net,
  col_inflow = "inflow",
  col_outflow = "outflow",
  col_target = "net_target",
  n_cores = 1,
  fun = 2,
  parallel = FALSE
)
}
\arguments{
\item{base_in}{Dataframe base in flows}

\item{base_out}{Dataframe Base outflows}

\item{target_net}{Dataframe Target net migration value}

\item{col_inflow}{String. Column in base_in containing data. Default "inflow"}

\item{col_outflow}{= String.Column in base_out containing data. Default "outflow",}

\item{col_target}{= String.Column in target_net containing data. Default "net_target",}

\item{n_cores}{Numeric. Number of cores to use if paralell is TRUE}

\item{fun}{Numeric. Which version of the regrosser function to use. Verion 1 is
depricated and shouldn't be used. Deafult 2.}

\item{parallel}{= Logical. Parallelise the process}
}
\description{
Regross
}
