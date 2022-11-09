#' Find Eurostat data sets by code, description or date of update
#'
#' @description
#'This function looks at Eurostat data sets and finds those that match the full or
#'partial code or description provided. The user can filter by date of last update.
#'Arguments should be unique (only one string per argument).
#'
#' @param sel_code a string ("nama_10r")
#' @param sel_desc a string ("GDP")
#' @param sel_update a date("2022-07-15")

#'
#' @return a data frame
#' @export eurostat_find
#'
#' @examples
#' df<-eurostat_find(sel_code="nama_10r")
#' df<-eurostat_find(sel_code="nama_10r",sel_desc = "GDP")
#' df<-eurostat_find(sel_code="nama_10",sel_desc = "GDP", sel_update ="2022-07-15")
#'
eurostat_find <- function(sel_code, sel_desc, sel_update) {
  # Function to Install and Load R Packages
  regacc::check_packages()

  options(warn = - 1)                # Disable warning messages globally
  df <- readr::read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt",
                          show_col_types = FALSE) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "dataset") %>%
    select(-type,-values)

    if(!missing(sel_code)) {
      df<- df %>%
        filter(stringr::str_detect(code, glob2rx(paste0("*",sel_code,"*"))))
    }
  if(!missing(sel_desc)) {
    df<- df %>%
      filter(stringr::str_detect(title, glob2rx(paste0("*",sel_desc,"*"))))
  }
  if(!missing(sel_update)) {
    df<- df %>%
      dplyr::mutate(last_update_of_data = lubridate::dmy(last_update_of_data)) %>%
      dplyr::filter(last_update_of_data >= sel_update)
  }

  df<- df %>%
    dplyr::distinct()
  options(warn = 0)                # Disable warning messages globally
  return(df)
      }
