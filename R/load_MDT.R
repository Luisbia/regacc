#' Load files from MDT
#'
#'Loads the historical files that we keep from the internal copy of MDT.
#'The location of the files in the server is:
#' @param folder Path to the location of the files
#' @param table_sel table(s) to load (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr).
#' @param country_sel Filter for a particular country/ies. All by default
#' @return a data frame/data.table object.
#' @export load_MDT
#'
#' @examples
#'
#'df <- load_MDT(folder = "E:/data/REGACC/MDT/source",
#'                        table_sel = "nama_10r_2gdp",
#'                        country_sel = c("ES","PT"))
load_MDT <- function(folder, table_sel, country_sel){
regacc::check_packages()
  options(warn = - 1)
    if(missing(country_sel)) {
    country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                    "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                    "SK","NO", "ME", "MK","TR","AL","RS","UK","CH")
                             }

      import_MDT<- function(file) {
       fread(file,fill=TRUE)%>%
        select(-obs_decimals) %>%
        mutate(country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        mutate(NUTS = as.factor(str_length(geo)-2),
               obs_value = as.numeric(obs_value),
               time = as.integer(time),
               obs_status = as.factor(obs_status))  }


    df <- list.files(
      path = folder,
      pattern = glob2rx(paste0("*",table_sel,".dat")),
      recursive = TRUE,
      full.names = TRUE) %>%
      as_tibble() %>%
      mutate(data=map(value,import_MDT)) %>%
      unnest(cols=c(data)) #%>%
      #mutate(vintage= as.integer(str_sub(value,-28,-25))) %>%
      #select(-value)

    return(df)
    options(warn = 0)
  }


