#' adding new Eurostat logo to charts
#'
#' @param chart ggplot chart in which the logo has to be inserted
#' @param colour "blue" by default or "white"
#' @param left left alignment , default 0.9
#' @param bottom bottom alignment , default 0.0
#' @param right right alignment , default 0.99
#' @param top top alignment , default 0.1
#'
#' @return a ggplot
#' @export ra_logo
#'
#' @examples
#' p<-ggplot(dataregacc::regacc_eurobase %>%
#'          filter(country=="EL" &
#'                 NUTS==1 &
#'                 unit_measure=="EUR_HAB" &
#'                 table=="gdp2"),
#'   aes(time_period,obs_value,colour=ref_area))+
#'   geom_line()+
#'   theme_ra()+
#'   scale_colour_ra()+
#'   labs(title="Greece NUTS 1 regions GDP",
#'        subtitle="EUR per inhabitant")
#'
#'q<-ra_logo(p)
#'q
ra_logo<- function (chart,colour = "blue", left=0.87, bottom=0.0,right=1,top=0.1){
  library(patchwork)

  if(colour=="white"){
    logo<-  png::readPNG(system.file("logo","eurostat_new_white.PNG",package ="regacc"),native=TRUE)
  }
  if(colour=="blue"){
  logo<-  png::readPNG(system.file("logo","eurostat_new.PNG",package ="regacc"),native=TRUE)
                  }

  chart_logo<- chart+
    theme(plot.margin= margin(0.5, 0.5, 2, 0.5,unit="cm"))+
    inset_element(logo,
                  left=left,
                  bottom=bottom,
                  right=right,
                  top=top,
                  align_to="full",
                  clip=TRUE,
                  on_top=FALSE)
  return(chart_logo)
}

