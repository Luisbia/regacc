#' fill palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia")
#'
#' @return a discrete fill palette
#' @export scale_fill_ra
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
#'ggplot(ro_d61 %>% filter(NUTS==2 & time_period==2019),aes(ref_area,obs_value,fill=ref_area))+
#'  geom_col()+
#'  scale_fill_ra()
scale_fill_ra = function(name="light") {
  ggplot2::scale_fill_manual(values = ra_palettes(name, type="discrete"))
}


