#' Get a cleaned denodo extraction
#'
#'The function connects to denodo and extracts either primary series (by default) or the complete A tables.
#' @param extraction view to select from Denodo ("hv_fame_regacc4regacc_all_dbs","hv_fame_regacc4regacc_t1001","hv_fame_regacc4regacc_t1002","hv_fame_regacc4regacc_t1200", "hv_fame_regacc4regacc_t1300","hv_fame_regacc4regacc_t1001_1200", "hv_fame_regacc4regacc_tnama" )
#'
#' @return a dataframe
#' @export get_denodo
#'
#' @examples
#' tnama<-get_denodo_new(extraction = "hv_fame_regacc4regacc_tnama")
#' 
get_denodo<- function(extraction){
  library(odbc) # This library is not normally installed and will need to be installed
  library(tidyverse)
  library(data.table)
  library(splitstackshape)
  
  # Connect to denodo
  vdp_con <- dbConnect(odbc(), "DenodoODBC")
  
  #### REGACC
  if (extraction=="hv_fame_regacc4regacc_tnama"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_tnama "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_t1001"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_t1001 "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_t1002"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_t1002 "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_t1200"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_t1200 "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_t1300"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_t1300 "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_t1001_1200"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_t1001_1200 "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
  
  if (extraction=="hv_fame_regacc4regacc_all_dbs"){
    sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_all_dbs "
    df_regacc<- dbGetQuery(vdp_con, sql_regacc)}
    
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
      filter(!is.na(obs_value)) %>% 
      mutate(time_period=as.integer(time_period),
             obs_value=as.numeric(obs_value),
             NUTS=as.factor(str_length(ref_area)-2),
             country= as.character(str_sub(ref_area,1,2))) 
  
  dbDisconnect(vdp_con)
  
  #Clean
  
  return(df_regacc)
}





