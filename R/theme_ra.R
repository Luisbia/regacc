#' Default theme options for regacc charts
#'
#' @param font Font to be used by default MS Sans Sheriff
#' @param fontsize 12 by default
#' @return a set of theme paremeters
#' @export theme_ra
#'
#' @examples
#' ggplot(dataregacc::eurobase %>%
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
theme_ra <- function(font= "MS Sans Serif",fontsize=12) {
  library(ragg)
  font <- font
  dark_text <- "#262B38"
  mid_text <- "#51555F"
  light_text<- "#7C7F87"
  
  ggplot2::theme_minimal(base_size = fontsize)+
    ggplot2::theme(
      text = element_text(colour = mid_text, family = font, lineheight = 1.1),
      plot.title = ggplot2::element_text(
        family = font,
        size = rel(1.6),
        face = "bold",
        color = dark_text,
        margin = margin(12, 0, 8, 0)),
      plot.subtitle = ggplot2::element_text(
        family = font,
        color = dark_text,
        size = rel(1.1),
        margin = ggplot2::margin(5, 0, 5, 0)
      ),
      plot.caption = ggplot2::element_blank(),
      
      legend.position = "top",
      legend.text.align = 0,
      legend.justification = 1,
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = font,
        size = rel(1.0),
        color = light_text
      ),
      
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        family = font,
        size = rel(1.0),
        color = mid_text
      ),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10),family=font,colour = light_text),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour=light_text),
      
      
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major= ggplot2::element_line(color = "#D4D5D7"),
      
      panel.background = ggplot2::element_rect(fill = "#F3F6FC"),
      
      plot.background = element_rect(fill= "#FFFFFF",
                                     colour = light_text),
      line = element_line(colour = light_text),
      rect = element_rect(fill = "#FAFAFA",
                          linetype = 0,
                          colour = NA),
      
      strip.background = ggplot2::element_rect(fill = "#F3F6FC"),
      strip.text = ggplot2::element_text(size = rel(1.4), hjust = 0.1, color=mid_text,face="bold", family=font),
      panel.spacing = unit(2, "lines"),
      strip.placement = "outside"
    )
}
