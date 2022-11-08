#' colour palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia") as defined in regacc_palettes()
#'
#' @return a colour palette
#' @export scale_colour_ra
#'
#' @examples
#' library(dataregacc)
#'library(tidyverse)
#'ro_d61<- eurobase %>%
#'  filter(sto == "D61" &
#'         accounting_entry == "D" &
#'         unit_measure == "MIO_NAC" &
#'         country =="RO")
#'
#'ggplot(ro_d61 %>% filter(NUTS==2),aes(time_period,obs_value,colour=ref_area))+
#'  geom_line(size=1)+
#'  scale_colour_ra()
scale_colour_ra = function(name="light") {

  ggplot2::scale_colour_manual(values = ra_palettes(name, type="discrete"))
}


