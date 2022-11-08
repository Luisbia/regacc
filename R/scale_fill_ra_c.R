#' fill palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia")
#'
#' @return a continuous fill palette
#' @export scale_fill_ra_c
#'
#' @examples a fill palette
#' library(dataregacc)
#'library(tidyverse)
#'ro_d61<- eurobase %>%
#'  filter(sto == "D61" &
#'         accounting_entry == "D" &
#'         unit_measure == "MIO_NAC" &
#'         country =="RO")
#'
#'ggplot(ro_d61,aes(time_period,ref_area,fill=obs_value))+
#'geom_tile()+
#'scale_fill_ra_c()
scale_fill_ra_c = function(name="monoblue") {


  ggplot2::scale_fill_gradientn(colours=ra_palettes(name=name,type="continuous"))
}


