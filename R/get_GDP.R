#' Get a dataframe with the latest Eurobase annual NAMA
#'
#' @description
#'This function downloads from Eurobase the latest NAMA data  and other variables in the table nama_10_gdp for the unit declared by the user.
#' @param na_item_sel which na_item to download, B1GQ by default
#' @param unit_sel which unit, CP_MEUR by and CP_MPPS_EU27_2020 by default
#' @param min_time minimum period to ge the data from, by default 2000-01-01
#' @return a data.frame/data.table
#' @export get_annual_NAMA
#'
#' @examples
#' dt<- get_annual_NAMA(na_item_sel=c("B1GQ", "D1"), unit_sel= c("CP_MEUR","CP_MNAC"))
get_annual_NAMA <- function(na_item_sel = "B1GQ",
                           unit_sel =c("CP_MEUR","CP_MPPS_EU27_2020"),
                           min_time ="2000-01-01"){
 
  regacc::check_packages()
   dt<-eurostat::get_eurostat("nama_10_gdp",
                                   filters=list(na_item=na_item_sel,
                                                unit = unit_sel,
                                   label=FALSE))
  dt<- na.omit(dt) %>%
    filter(time>=min_time) %>% 
    mutate(time=year(time)) %>% 
    regacc::convert_eurobase_codes()
  return(dt)}

#' Get a dataframe with the latest Eurobase annual NAMA per capita
#'
#' @description
#'This function downloads from Eurobase the latest NAMA data  and other variables in the table nama_10_gdp for the unit declared by the user.
#' @param na_item_sel which na_item to download, B1GQ by default
#' @param unit_sel which unit, CEUR and PPS are chosen by default
#' @param min_time minimum period to ge the data from, by default 2000-01-01
#' @return a data.frame/data.table
#' @export get_annual_NAMApc
#'
#' @examples
#' dt<- get_annual_NAMApc(na_item_sel=c("B1GQ","P31_S14"))
get_annual_NAMApc <- function(na_item_sel = "B1GQ",
                            unit_sel =c("PC_EU27_2020_HAB_MEUR_CP","PC_EU27_2020_HAB_MPPS_CP","CP_EUR_HAB","CP_PPS_EU27_2020_HAB"),
                            min_time ="2000-01-01"){
  
  regacc::check_packages()
  dt<-eurostat::get_eurostat("nama_10_pc",
                             filters=list(na_item=na_item_sel,
                                          unit = unit_sel,
                                          label=FALSE))
  dt<- na.omit(dt) %>%
    filter(time>=min_time) %>% 
    mutate(time=year(time)) %>% 
    regacc::convert_eurobase_codes()
  return(dt)}


#' Get a dataframe with the latest Eurobase regional GDP
#'
#' @description
#'This function downloads from Eurobase the latest regional GDP data in the table nama_10r_3gdp for the unit declared by the user.
#'
#' @param unit_sel which unit, all by default. Options are "MIO_EUR","EUR_HAB","	EUR_HAB_EU27_2020", "MIO_NAC", "MIO_PPS_EU27_2020", "PPS_EU27_2020_HAB","	PPS_HAB_EU27_2020".
#'
#'
#' @return a data.frame/data.table
#' @export
#'
#' @examples
#' dt<- get_regional_GDP(unit_sel= c("MIO_EUR","EUR_HAB"))
get_regional_GDP <- function(unit_sel =c("MIO_EUR",
                                         "EUR_HAB",
                                         "EUR_HAB_EU27_2020",
                                         "MIO_NAC",
                                         "MIO_PPS_EU27_2020",
                                         "PPS_EU27_2020_HAB",
                                         "PPS_HAB_EU27_2020"))
  {
 regacc::check_packages()
  dt<-eurostat::get_eurostat("nama_10r_3gdp",
                             time_format = "num",
                                   filters=list(unit = unit_sel),
                                   label=FALSE)
    dt<- na.omit(dt)%>% 
     regacc::convert_eurobase_codes() %>% 
      left_join(.,dataregacc::NUTS_2021)
  return(dt)}


