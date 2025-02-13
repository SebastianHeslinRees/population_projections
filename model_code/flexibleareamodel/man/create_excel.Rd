% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_excel.R
\name{create_excel}
\alias{create_excel}
\title{Source a python function to output Ward and/or MSOA excel files}
\usage{
create_excel(
  output_dir,
  wb_filename,
  projection_name,
  bpo = FALSE,
  smallarea = "ward"
)
}
\arguments{
\item{output_dir}{string. The directory from which to read the data and save the Excel file}

\item{wb_filename}{String. The name of the output Excel file. With or without '.xlsx'}

\item{projection_name}{String. The projection name.}

\item{bpo}{FALSE or a gss code indicating which borough's bpo this is}

\item{smallarea}{String. Projection type: either 'ward' or 'msoa'. Default 'ward'.}
}
\value{
Output Excel workbooks
}
\description{
A wrapper for a python function which efficiently creates model output
Excel files.

There is a problem with reticulate/python/renv that I can't get to the bottom of
The first time a python script is sourced the function throws an error. It can't find
rpytools. If you source again it works. The temp work around is to wrap the source in
try() so the error is caught and then run it a second time. Its ugly but it works.
See also trendmodel::create_household_model_excels, etc
}
