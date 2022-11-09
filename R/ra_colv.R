#' Create standard vertical bar charts
#'
#'This functions makes easy to create standard scatter charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param clr the variable mapped to the fill aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#' @param bar type of bar, "stack" by default, other options "dodge" and "fill"
#' @param wdt width of the bar from 0 to 1, default 0.98
#' @param vbr parameter for the number of vertical breaks, input for scales::break_extended
#' @param acc number of decimals for Y axis (0.1 means one decimal)
#'
#' @return a ggplot
#' @export ra_colv
#'
#' @examples
#'df<- dataregacc::NQR %>%
#'  filter(country =="SK" & vintage =="2021" & NUTS =="2") %>%
#'  pivot_wider(names_from=sto,
#'              values_from=obs_value)
#'
#'ra_colv(dat=df,
#'        hor=time_period,
#'        ver= EMP,
#'        clr=ref_area)+
#'labs(title="Employment in Slovakia NUTS 2 regions",
#'     subtitle = "Thousands of persons")
ra_colv <- function(dat,
                    hor,
                    ver,
                    clr,
                    leg = TRUE,
                    fac,
                    wdt = 0.98,
                    vbr = 5,
                    acc=1,
                    bar="stack")
{
  regacc::check_packages()

  plot <- ggplot(dat) +
    geom_col(aes({{hor}}, {{ver}}, fill = {{clr}}), position=bar,width = wdt,colour = "#262B38")+
    theme_ra()+
    scale_fill_ra()+
    scale_y_continuous(labels = scales::label_number(accuracy = acc),
                       breaks = scales::breaks_extended(n=vbr))+

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


