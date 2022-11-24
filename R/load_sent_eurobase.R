#' Load files sent to Eurobase
#'
#' This function looks at the folder where the files sent to Eurobase are stored, loads them and puts them together in a data frame.
#' @param folder specifies the folder where the files are (zip files)
#' @param table_sel the Eurobase table to look for. It only accepts a single argument (nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr,nama_10r_2lp10, nama_10r_2nlp, nama_10r_3nlp)
#' @param country_sel country or countries to look for
#' @param time_min date of publication from where to start looking (yyyy-mm-dd)
#' @param time_max date of publication from where to stop looking (yyyy-mm-dd)
#' @param consolidate TRUE to remove duplicated obs_value, FALSE (default) to keep them all
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
  
  tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva", "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", "nama_10r_2hhinc", "nama_10r_2gvagr", "nama_10r_2lp10", "nama_10r_2nlp", "nama_10r_3nlp")
  
  if (table_sel %in% tables){
    
    if(missing(country_sel)) {
      country_sel<- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
                      "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI",
                      "SK","NO", "ME", "MK","TR","AL","RS","UK","CH","EU")}
    
    # import gdp
    import_file_gdp2 <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "unit_measure", "obs_value"),
            col_types ="cccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
			   accounting_entry = "B",
			   activity = "_Z",
			   sto= "B1GQ",
			   table="gdp2")
      
    }
    import_file_gdp3 <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "unit_measure", "obs_value"),
            col_types ="cccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               accounting_entry = "B",
               activity = "_Z",
               sto= "B1GQ",
               table="gdp3")
      
    }
    # import pop
    import_file_pop <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "unit_measure", "obs_value"),
            col_types ="cccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               accounting_entry = "_Z",
               activity = "_Z",
			         sto="POP",
			         table="pop3",
			         unit_measure="PS")
      
    }
    #import gva
    import_file_gva <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "activity", "unit_measure", "obs_value"),
            col_types = "ccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
			   accounting_entry = "B",
			   sto="B1G",
			   table="gva3") %>% 
        regacc::convert_eurobase_codes()
    }
    #import hw
    import_file_hw <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "activity", "na_item", "unit_measure", "obs_value"),
            col_types = "cccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               accounting_entry = "_Z",
               table="emphw2",
               unit_measure="HW")%>% 
        regacc::convert_eurobase_codes()
    }
    
    import_file_d1 <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "activity", "unit_measure", "obs_value"),
            col_types = "ccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               accounting_entry = "D",
			          sto="D1",
			          table="coe2")%>% 
        regacc::convert_eurobase_codes()
    }
    import_file_p51g <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "activity", "unit_measure", "obs_value"),
            col_types = "ccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               accounting_entry = "D",
               sto="P51G",
               table="gfcf2")%>% 
        regacc::convert_eurobase_codes()
    }
    
    # import file for emp
    import_file_emp <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "activity", "na_item", "unit_measure", "obs_value"),
            col_types = "cccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
			   accounting_entry = "_Z",
			   table="emp3",
			   unit_measure="PS")%>% 
        regacc::convert_eurobase_codes()
    }
    
    # import file for hh
    import_file_hh <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "na_item", "direct", "unit_measure", "obs_value"),
            col_types = "cccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
               activity ="_Z",
               table="hh2")%>% 
        regacc::convert_eurobase_codes()
    }
    
    # gvagr
    import_file_gvagr <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "unit_measure", "obs_value"),
            col_types = "cccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value),
			   accounting_entry = "_Z",
			   activity = "TOTAL",
			   sto="B1G",
			   table="gvagr2")
    }
    
    # nlp
    import_file_nlp <- function(file) {
      vroom(file,
            delim = " ",
            col_names = c("time_period", "ref_area", "na_item", "unit_measure", "obs_value"),
            col_types = "ccccc",
            skip = 4
      ) %>%
        mutate(time_period = as.integer(stringr::str_sub(time_period, 1, 4)),
               country = substr(ref_area, start = 1, stop = 2)) %>%
        filter(country %in% country_sel) %>%
        separate(obs_value,c("obs_value","obs_status"), sep="~") %>%
        mutate(NUTS = stringr::str_length(ref_area)-2,
               obs_value = as.numeric(obs_value))
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
        mutate(data= map(value,import_file_gdp2)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3gdp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gdp3)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3popgdp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_pop)) %>%
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
        mutate(data= map(value,import_file_d1)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2gfcf") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_p51g)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2emhrw") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_hw)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2hhinc") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_hh)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2gvagr") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_gvagr)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2lp10") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_emp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_2nlp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_nlp)) %>%
        unnest(cols=c(data))
    } else if (table_sel == "nama_10r_3nlp") {
      df_list<-df_list %>%
        mutate(data= map(value,import_file_nlp)) %>%
        unnest(cols=c(data))

  }else {
    cli::cli_alert_danger("Wrong table: tables should be one of these: nama_10r_2gdp, nama_10r_3gdp, nama_10r_3popgdp, nama_10r_3gva, nama_10r_3empers, nama_10r_2coe, nama_10r_2gfcf, nama_10r_2emhrw, nama_10r_2hhinc, nama_10r_2gvavr,nama_10r_2lp10, nama_10r_2nlp, nama_10r_3nlp" )

  }
  if(consolidate == TRUE){
    df_list <- df_list %>%
      select(-value) %>%
      arrange(date,.by_group=TRUE) %>%
      group_by(across(-c(date))) %>%
      slice_head(n=1)%>%
      ungroup()%>% 
      arrange(date) 
    }
  
  return(df_list)
  }
}
