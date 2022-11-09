#' Create standard barcharts
#'
#'This functions makes easy to create standard scatter charts for different purposes
#' @param dat the data frame with the data
#' @param hor the name of the variable in the horizontal axis
#' @param ver the name of the variable in the vertical axis
#' @param clr the variable mapped to the fill aesthetic
#' @param leg set to FALSE if you do not want to show the legend
#' @param fac the (unique) variable to use if small multiples are wanted
#' @param lwt linewdth for the squares
#' @param lcl line colour for the squares
#' @param pal fill palette to be chosen from scale_fill_regacc_c
#'
#' @return a ggplot
#' @export ra_heatmap
#'
#' @examples
#'df<- dataregacc::NQR %>%
#'  filter(country =="SK" & vintage =="2021" & NUTS =="2") %>%
#'  pivot_wider(names_from=sto,
#'              values_from=obs_value)
#'
#'ra_heatmap(dat=df %>% drop_na(D1),
#'               hor=time_period,
#'               ver= ref_area,
#'               clr=D1)
ra_heatmap <- function(dat,
                       hor,
                       ver,
                       clr,
                       leg = TRUE,
                       fac,
                       lwt = 0.5,
                       lcl = "white",
                       pal= "monoblue")
{

  regacc::check_packages()

  plot <- ggplot(dat) +
    geom_tile(aes({{hor}}, fct_rev({{ver}}), fill = {{clr}}), colour=lcl,lwd = lwt)+
    theme_ra()+
    scale_fill_ra_c(name = pal)+


    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())+
    coord_fixed(expand=FALSE)


  if (leg == FALSE) {
    plot <- plot + theme(legend.position = "none")
  }
  if (leg == TRUE)  {
    plot <- plot + theme(legend.position = "right")
  }
  # facet
  if(!missing(fac)){
    plot <- plot + facet_wrap(vars({{fac}}))
  }

  return(plot)
}


