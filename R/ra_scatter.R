#' Create standard scatter charts
#'
#'This functions makes easy to create standard scatter charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param clr the variable mapped to the colour aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#' @param psz point size
#' @param hbr parameter for the number of horizontal breaks, input for scales::break_extended
#' @param vbr parameter for the number of vertical breaks, input for scales::break_extended
#' @param accv number of decimals for Y axis (0.1 means one decimal)
#' @param acch number of decimals for Y axis (0.1 means one decimal)
#'
#' @return a ggplot
#' @export ra_scatter
#'
#' @examples
#'df<- regacc::NQR %>%
#'  filter(country =="SK" & vintage =="2021" & NUTS =="2") %>%
#'  pivot_wider(names_from=sto,
#'              values_from=obs_value)
#'ra_scatter(dat=df,
#'           hor=B1G,
#'           ver=B6N,
#'           clr=ref_area)
ra_scatter <- function(dat,
                       hor,
                       ver,
                       clr,
                       leg = TRUE,
                       fac,
                       psz = 3,
                       hbr = 4,
                       vbr = 4,
                       acch = 1,
                       accv =1) 
{

  regacc::check_packages()

  plot <- ggplot(dat) +
    geom_point(aes({{hor}}, {{ver}}, colour = {{clr}}),size = psz)+
    theme_ra()+
    scale_colour_ra()+
    scale_x_continuous(labels = scales::label_number(accuracy = acch),
                       breaks = scales::breaks_extended(n=hbr))+
    scale_y_continuous(labels = scales::label_number(accuracy = accv),
                       breaks = scales::breaks_extended(n=vbr))


  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "right")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}),scales="free")
  }

  return(plot)
}


