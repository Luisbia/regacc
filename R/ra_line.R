#' Create standard line charts
#'
#'This functions makes easy to create standard line charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param grp the variable to use for grouping the data
#' @param clr the variable mapped to the colour aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#' @param lsz line size
#' @param hbr parameter for the number of horizontal breaks, input for scales::break_extended
#' @param vbr parameter for the number of vertical breaks, input for scales::break_extended
#' @param acc number of decimals for Y axis (0.1 means one decimal)
#' @param exm multiplicative parameter for the scale limits
#' @param exa additive parameter for the scale limits
#'
#' @return a ggplot
#' @export ra_line
#'
#' @examples
#' df<- dataregacc::NQR %>%
#'  filter(country =="SK" & vintage =="2021" & NUTS =="2")
#' ra_line(dat=df,
#'                  hor=time_period,
#'                  ver=obs_value,
#'                  grp=ref_area,
#'                  hbr=3,
#'                  vbr=4,
#'                  clr=ref_area,
#'                  leg=FALSE,
#'                  fac=sto)
ra_line <- function(dat,
                    hor,
                    ver,
                    grp,
                    clr,
                    leg = TRUE,
                    fac,
                    lsz = 0.8,
                    hbr = 3,
                    vbr = 4,
                    acc = 1,
                    exm = 0,#default in ggplot 0.05
                    exa = 0.2) # default in ggplot 0
{
regacc::check_packages()

  plot <- ggplot(dat) +
    geom_line(aes({{hor}}, {{ver}}, group = {{grp}}, colour = {{clr}}),size = lsz)+
    theme_ra()+
    scale_colour_ra()+
    scale_x_continuous(labels = scales::label_number(accuracy = 1),
                       breaks = scales::breaks_extended(n=hbr),
                       expand = c(exm,exa))+
    scale_y_continuous(labels = scales::label_number(accuracy = acc),
                       breaks = scales::breaks_extended(n=vbr))+
    theme(panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line())


  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "right")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}),scales="free_y")
  }

  return(plot)
}


