#' Report transmissions
#'
#' The function looks at a specific folder with the transmitted data from countries and provides a table with the dates
#'
#' @param folder folder where files area.
#' @param country_sel optionally specify a country.
#' @param table_sel optionally specify a table.
#' @param time_min optionally specify a minimum date.
#' @param time_max optionally specify a maximum date
#'
#' @return a data frame
#' @export report_transmission
#'
#' @examples
#' report_transmission("E:/regacc/data/input/xml")
#' report_transmission("E:/regacc/data/input/xml", country_sel="SI", table_sel="T1300")
report_transmission<- function (folder,
                                country_sel,
                                table_sel,
                                time_min = "2021-11-01",
                                time_max = "2099-10-01"){

  if(missing(country_sel)) {
    country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                    "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                    "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")}
  if (missing(table_sel)) {
    table_sel<- c("T1001","T1002","T1200","T1300")
  }
  trans <- list.files (path = folder,
                       pattern = "xml$",
                       full.names=TRUE,
                       recursive = TRUE) %>%
    as_tibble() %>%
    mutate(country =str_sub(value,-22,-21),
           table = str_sub(value,-30,-26)) %>%
    filter(table %in% table_sel &
           country %in% country_sel) %>%
    mutate(time=map(value,file.mtime)) %>%
    unnest(cols=c(time)) %>%
    filter(time>=time_min & time <= time_max) %>%
    mutate(time=format(time,"%d/%m/%Y"))

  return(trans)
}



