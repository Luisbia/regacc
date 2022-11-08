#' Load files sent to Eurobase
#'
#' This function looks at the folder where the files sent to Eurobase are stored, loads them and puts them together in a data frame.
#' @param folder specifies the folder where the files are (zip files)
#' @param table_sel the Eurobase table to look for. It only accepts a single argument (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr)
#' @param country_sel country or countries to look for
#' @param time_min date of publication from where to start looking (yyyy-mm-dd)
#' @param time_max date of publication from where to stop looking (yyyy-mm-dd)
#' @param consolidate TRUE to remove duplicated values, FALSE (default) to keep them all
#' @export load_sent_eurobase
#'
#' @return a data frame
#'
#' @examples
#' df<- load_sent_eurobase(folder = "E:/data/REGACC/sent/source",
#' table_sel="nama_10r_2gvagr",
#' country_sel = c("ES","PT"),
#' time_min = "2020-01-01",
#' time_max = "2021-03-16",
#' consolidate = TRUE)



load_sent_eurobase<- function(folder,table_sel, country_sel,time_min= "2019-01-01",time_max="2023-07-11", consolidate = FALSE){
  regacc::check_packages()
  options(warn = - 1)
  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvagr")

  if (table_sel %in% tables){

    if(missing(country_sel)) {
      country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                      "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                      "SK","NO", "ME", "MK","TR","AL","RS","UK","CH","EU")}

    # import gdp, pop
    import_file_gdp <- function(file) {
             vroom(file,
             delim = " ",
             col_names = c("time", "geo", "unit", "values"),
             col_types ="cccc",
             skip = 4
      ) %>%
        mutate(time = as.integer(stringr::str_sub(time, 1, 4)),
               country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(values,c("values","flag"), sep="~") %>%
        mutate(NUTS = stringr::str_length(geo)-2,
               values = as.numeric(values))

    }
    #import gva
    import_file_gva <- function(file) {
      vroom(file,
             delim = " ",
             col_names = c("time", "geo", "nace_r2", "unit", "values"),
             col_types = "ccccc",
             skip = 4
      ) %>%
        mutate(time = as.integer(stringr::str_sub(time, 1, 4)),
               country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(values,c("values","flag"), sep="~") %>%
        mutate(NUTS = stringr::str_length(geo)-2,
               values = as.numeric(values))
    }

    # import file for emp
    import_file_emp <- function(file) {
      vroom(file,
             delim = " ",
             col_names = c("time", "geo", "nace_r2", "na_item", "unit", "values"),
             col_types = "cccccc",
             skip = 4
      ) %>%
        mutate(time = as.integer(stringr::str_sub(time, 1, 4)),
               country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(values,c("values","flag"), sep="~") %>%
        mutate(NUTS = stringr::str_length(geo)-2,
               values = as.numeric(values))
    }

    # import file for hh
    import_file_hh <- function(file) {
      vroom(file,
             delim = " ",
             col_names = c("time", "geo", "na_item", "direct", "unit", "values"),
             col_types = "cccccc",
             skip = 4
      ) %>%
        mutate(time = as.integer(stringr::str_sub(time, 1, 4)),
               country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(values,c("values","flag"), sep="~") %>%
        mutate(NUTS = stringr::str_length(geo)-2,
               values = as.numeric(values))
            }

    # gvagr
    import_file_gvagr <- function(file) {
      vroom(file,
             delim = " ",
             col_names = c("time", "geo", "unit", "values"),
             col_types = "cccc",
             skip = 4
      ) %>%
        mutate(time = as.integer(stringr::str_sub(time, 1, 4)),
               country = substr(geo, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(values,c("values","flag"), sep="~") %>%
        mutate(NUTS = stringr::str_length(geo)-2,
               values = as.numeric(values))
    }

    # look for files
    df_list<- list.files(path = folder,
                         full.names = TRUE,
                         pattern = glob2rx(paste0(table_sel,"*")),
                         recursive = TRUE
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date=file.mtime(value)) %>%
      dplyr::filter(date>= time_min & date<=time_max)

    ##sort files
    if(table_sel == "nama_10r_2gdp"){
      df_list<-df_list %>%
      mutate(data= map(value,import_file_gdp)) %>%
      unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3gdp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gdp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3popgdp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gdp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3gva") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gva)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3empers") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_emp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2coe") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gva)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2gfcf") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gva)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2emhrw") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_emp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2hhinc") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_hh)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2gvagr") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gvagr)) %>%
        unnest(cols=c(data))
    } else{
      print("no such table")
    }

  }else {
    print("Wrong table: tables should be one of these:nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvagr" )
  }
  if(consolidate == TRUE){
    df_list <- df_list %>%
      select(-value) %>%
      arrange(date) %>%
      group_by(across(-c(values,date))) %>%
      slice_tail(n=1) %>%
      ungroup}

  return(df_list)
  options(warn = 0)
}




