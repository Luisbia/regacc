% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_MDT.R
\name{load_MDT}
\alias{load_MDT}
\title{Load files from MDT}
\usage{
load_MDT(folder, table_sel, country_sel)
}
\arguments{
\item{folder}{Path to the location of the files}

\item{table_sel}{table(s) to load (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr).}

\item{country_sel}{Filter for a particular country/ies. All by default}
}
\value{
a data frame/data.table object.
}
\description{
Loads the historical files that we keep from the internal copy of MDT.
The location of the files in the server is:
}
\examples{

df <- load_MDT(folder = "E:/data/REGACC/MDT/source",
                       table_sel = "nama_10r_2gdp",
                       country_sel = c("ES","PT"))
}
