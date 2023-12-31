% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidateFormatOfDataTable.r
\name{ValidateFormatOfDataTable}
\alias{ValidateFormatOfDataTable}
\title{validate format of the experimental data table}
\usage{
ValidateFormatOfDataTable(dataTable)
}
\arguments{
\item{dataTable}{the data table to be tested}
}
\value{
true if the data table is valid
}
\description{
Checks the format of the experimental data. This is expected to be two columns, the first containing the gene name and the second the direction of regulation, -1, 0 or 1. The function checks the number of columns and the values of the second column,
}

