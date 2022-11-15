#' Get a cleaned denodo extraction
#'
#'The function connects to denodo and extracts either primary series (by default) or the complete A tables.
#' @param onlyV FALSE (default) extracts primary T and V series from tables T1001, T1002, T1200 and T1300. This is used for creating country csv files for the analysis. TRUE extracts V series for several units. The file is used for other purposes
#'
#' @return a dataframe
#' @export get_denodo
#'
#' @examples
#' partial<- get_denodo()
#' full<- get_denodo(full=TRUE)
get_denodo<- function(onlyV=FALSE){
  library(odbc) # This library is not normally installed and will need to be installed
  library(tidyverse)
  library(data.table)
  library(splitstackshape)
  
  # Connect to denodo
  vdp_con <- dbConnect(odbc(), "DenodoODBC")
  
  #### REGACC
  if (full==onlyV){
  # Creating the queries. This should take 2 minutes
  sql_regacc_t1001 <- "  SELECT * FROM hv_fame_regacc4regacc_t1001 "
  sql_regacc_t1002 <- "  SELECT * FROM hv_fame_regacc4regacc_t1002 "
  sql_regacc_t1200 <- "  SELECT * FROM hv_fame_regacc4regacc_t1200 "
  sql_regacc_t1300 <- "  SELECT * FROM hv_fame_regacc4regacc_t1300 "
  df_regacc_t1001<- dbGetQuery(vdp_con, sql_regacc_t1001)
  df_regacc_t1002<- dbGetQuery(vdp_con, sql_regacc_t1002)
  df_regacc_t1200<- dbGetQuery(vdp_con, sql_regacc_t1200)
  df_regacc_t1300<- dbGetQuery(vdp_con, sql_regacc_t1300)
  
  # Bind the extractions
  df_regacc<- bind_rows(df_regacc_t1001,df_regacc_t1002, df_regacc_t1200,df_regacc_t1300)
  
  df_regacc <- df_regacc %>% 
    select(name,date,value) %>% 
    na.omit() %>% 
    cSplit("name", sep = ".") %>% 
    rename("type"=name_01,
           "table_identifier"=name_02,
           "freq"=name_03,
           "ref_area"=name_04,
           "counterpart_area"=name_05,
           "ref_sector"=name_06,
           "counterpart_sector"=name_07,
           "accounting_entry"=name_08,
           "sto"=name_09,
           "activity"=name_10,
           "valuation"=name_11,
           "prices"=name_12,
           "transformation"=name_13,
           "unit_measure"=name_14
    ) %>% 
    rename(time_period=date,
           obs_value=value)%>% 
    filter(table_identifier!="TNAMA" & unit_measure %in% c("PS","XDC","PC") & transformation %in% c("N","G1")) %>% 
    mutate(time_period=as.integer(time_period),
           obs_value=as.numeric(obs_value),
           NUTS=as.factor(str_length(ref_area)-2),
           country= as.character(str_sub(ref_area,1,2))) %>% 
    pivot_wider(names_from=type,
                values_from=obs_value) %>% 
    mutate(T=coalesce(T,V)) %>% 
    pivot_longer(cols=c(T,V),
                 names_to="type",
                 values_to="obs_value")
  
  # Reconstruct series
  ##T
  gvagr<- df_regacc %>% 
    filter(type=="T" & unit_measure=="PC")
  
  lasty<- df_regacc %>% 
    filter(type=="T" & table_identifier=="T1001" & time_period== 2021 & 
             prices !="Y")# remove GVA in PYP
  
  prevy<- df_regacc %>% 
    filter(type=="T" & table_identifier=="T1200" & time_period< 2021 & 
             prices !="Y" & NUTS!="3" & activity %in% c("_T","_Z") & 
             sto %in% c("POP","EMP","B1G") ) %>% # remove GVA in PYP
    mutate(table_identifier="T1001")
  
  no_t1001<- df_regacc %>% 
    filter(type=="T" & table_identifier!="T1001" & prices!="Y")
  
  T_series <- bind_rows(gvagr, lasty, prevy,no_t1001)
  
  ##V
  gvagr<- df_regacc %>% 
    filter(type=="V" & unit_measure=="PC")
  
  lasty<- df_regacc %>% 
    filter(type=="V" & table_identifier=="T1001" & time_period>= 2020 & 
             prices !="Y")# remove GVA in PYP
  
  prevy<- df_regacc %>% 
    filter(type=="V" & table_identifier=="T1200" & time_period< 2020 & 
             prices !="Y" & NUTS!="3" & activity %in% c("_T","_Z")) %>% # remove GVA in PYP
    mutate(table_identifier="T1001")
  
  no_t1001<- df_regacc %>% 
    filter(type=="V" & table_identifier!="T1001" & prices!="Y")
  
  V_series <- bind_rows(gvagr, lasty, prevy,no_t1001)
  
  
  df_regacc<- bind_rows(T_series,V_series) %>% 
    select(type,table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value)
  
  
  }
  
  if (onlyV==TRUE){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_all_dbs "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)

    df_regacc <- df_regacc %>% 
      select(name,date,value) %>% 
      na.omit() %>% 
      cSplit("name", sep = ".") %>% 
      rename("type"=name_01,
             "table_identifier"=name_02,
             "freq"=name_03,
             "ref_area"=name_04,
             "counterpart_area"=name_05,
             "ref_sector"=name_06,
             "counterpart_sector"=name_07,
             "accounting_entry"=name_08,
             "sto"=name_09,
             "activity"=name_10,
             "valuation"=name_11,
             "prices"=name_12,
             "transformation"=name_13,
             "unit_measure"=name_14
      ) %>%
      filter(type=="V") %>% 
      select(-type) %>% 
      rename(time_period=date,
             obs_value=value)%>% 
      mutate(time_period=as.integer(time_period),
             obs_value=as.numeric(obs_value),
             NUTS=as.factor(str_length(ref_area)-2),
             country= as.character(str_sub(ref_area,1,2))) 
       }
  
  dbDisconnect(vdp_con)

    #Clean
 
  return(df_regacc)
  }
 



