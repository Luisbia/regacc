% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_NACE.R
\name{check_NACE}
\alias{check_NACE}
\title{Check NACE}
\usage{
check_NACE(df, ths_abs = 1, ths_rel = 0.5)
}
\arguments{
\item{df}{the data frame with the data.}

\item{ths_abs}{absolute threshold. Differences below the threshold are ignored.}

\item{ths_rel}{relative threshold. Differences below the threshold are ignored (1=1\%)}
}
\value{
a dataframe. If all pass are passed it will be empty.
}
\description{
Check NACE
}
\examples{
library(tidyverse)
df <- regacc_load_csv(
folder = "D:/data/REGACC/csv",
country_sel = c("AT"),
time_min = "2021-12-01")

check_NACE(df, ths_abs = 1, ths_rel = 0.5)
}
