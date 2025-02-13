% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_small_area_excels.R
\name{create_small_area_excels}
\alias{create_small_area_excels}
\title{Source a python function to output Ward and/or MSOA excel files}
\usage{
create_small_area_excels(
  output_dir,
  wb_filename,
  projection_name,
  smallarea = "ward"
)
}
\arguments{
\item{output_dir}{string. The directory from which to read the data and save the Excel file}

\item{wb_filename}{String. The name of the output Excel file. With or without '.xlsx'}

\item{projection_name}{String. The projection name.}

\item{smallarea}{String. Projection type: either 'ward' or 'msoa'. Default 'ward'.}
}
\value{
Output Excel workbooks
}
\description{
A wrapper for a python function which efficiently creates model output
Excel files. Run time is 3 minutes for a ward projection and 5 minutes for
an MSOA projection.
}
