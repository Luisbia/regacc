#' Create standard dot charts
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
#' @param acc number of decimals for Y axis (0.1 means one decimal)
#'
#' @return a ggplot
#' @export ra_dot
#'
#' @examples
#'df<- dataregacc::NQR %>%
#'  filter(country =="SK" & vintage =="2021" & NUTS =="2") %>%
#'  pivot_wider(names_from=sto,
#'              values_from=obs_value)
#'
#'ra_dot(dat=df,
#'       hor=B1G,
#'       ver=ref_area,
#'       clr="B1G",
#'       psz = 4)
ra_dot <- function(dat,
                   hor,
                   ver,
                   clr,
                   leg = TRUE,
                   fac,
                   psz = 3,
                   hbr = 4,
                   vbr = 4,
                   acc=1)
{
  regacc::check_packages()

  plot <- ggplot(dat) +
    geom_point(aes({{hor}}, fct_reorder({{ver}},{{hor}}), colour = {{clr}}),size = psz)+
    theme_ra()+
    scale_colour_ra()+
    scale_x_continuous(labels = scales::label_number(accuracy = acc),
                       breaks = scales::breaks_extended(n=hbr))+

    theme(panel.grid.major.x = element_blank())


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


