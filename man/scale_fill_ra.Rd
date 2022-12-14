% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_fill_ra.R
\name{scale_fill_ra}
\alias{scale_fill_ra}
\title{fill palette for charts}
\usage{
scale_fill_ra(name = "light")
}
\arguments{
\item{name}{name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia")}
}
\value{
a discrete fill palette
}
\description{
fill palette for charts
}
\examples{
a fill palette
library(dataregacc)
library(tidyverse)
ro_d61<- eurobase \%>\%
 filter(sto == "D61" &
        accounting_entry == "D" &
        unit_measure == "MIO_NAC" &
        country =="RO")

ggplot(ro_d61 \%>\% filter(NUTS==2 & time_period==2019),aes(ref_area,obs_value,fill=ref_area))+
 geom_col()+
 scale_fill_ra()
}
