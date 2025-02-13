% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_trend_model_excels.R
\name{create_trend_model_excels}
\alias{create_trend_model_excels}
\title{Produce 3 Excel workbooks from a set of projection RDS files}
\usage{
create_trend_model_excels(
  output_dir,
  wb_filename,
  projection_name,
  household_models = TRUE
)
}
\arguments{
\item{output_dir}{string. The directory from which to read the data and save the Excel file}

\item{wb_filename}{String. The name of the output Excel file. With or without '.xlsx'}

\item{projection_name}{String. The projection name.}

\item{household_models}{Logical. Should the household model excel files also be created.}
}
\description{
If a projection has been run with the \code{write_excel} variable set to
\code{FALSE} and the Excel outputs are subsequently required this function
will produce those workbooks. Creates a population workbook, an ONS household
workbook and a DCLG household workbook.
}
