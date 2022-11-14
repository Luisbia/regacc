#' Get basic information from xml files
#'
#'This functions provides summary information (reporting periods, regions, flags and comments) from xml files. The output are dynamic HTML tables
#' @param ... same parameters as for load_xml
#'
#' @return four interactive html tables
#' @export get_xml_basic_info
#'
#' @examples
#' get_xml_basic_info(folder = "D:/data/REGACC/xml",
#' country_sel="AT",
#' time_min = "2021-10-01")
#' 
get_xml_basic_info<- function(...){
  
  regacc::check_packages()
  
  data<-regacc::load_xml(...) 
  
  temp<- data %>% 
    filter(!obs_status %in% c("L","M") & !is.na(obs_value) ) %>% 
    select(country,table_identifier,country,sto,accounting_entry,unit_measure, time_period) %>% 
    group_by(across(-c(time_period))) %>% 
    mutate(first=min(time_period),
           last = max(time_period)) %>% 
    select(-time_period) %>% 
    unique() %>% 
    mutate(across(everything(as.factor)))

  
  temp1<-show_DT(temp, caption ="Reported periods")
  
  
  if ("embargo_date" %in% colnames(data)){
    temp<- data %>% 
      select(country,table_identifier,country,embargo_date) %>% 
      na.omit() %>% 
      unique() %>% 
      mutate(across(everything(as.factor))) %>% 
      droplevels()

    temp2<-show_DT(temp, caption ="Embargo dates")
  } else {
    temp2<- "No embargo dates"
  }

  if ("comment_ts" %in% names(df)) {
    temp<- data %>% 
      select(country,table_identifier,comment_ts) %>% 
      filter(comment_ts!="xxx" & comment_ts!="") %>% 
      na.omit() %>% 
      unique() %>% 
      mutate(across(everything(as.factor))) %>% 
      droplevels()
    
    temp3<-show_DT(temp, caption ="Comments")
  } else {
    temp2<- "No comments"
  }
  temp<- data %>% 
    select(country,table_identifier,sto,activity,unit_measure,conf_status) %>% 
    filter(conf_status =="N") %>% 
    na.omit() %>% 
    unique() %>% 
    mutate(across(everything(as.factor))) %>% 
    droplevels() 
  
  temp3<-show_DT(temp, caption ="Data not publishable")
  
  temp<- data %>% 
    select(country,table_identifier,sto,unit_measure,obs_status,time_period) %>% 
    filter(obs_status %in% c("E","P","B","D","U")) %>% 
    na.omit() %>% 
    unique() %>% 
    mutate(across(everything(as.factor))) %>% 
    droplevels()
  
  temp4<-show_DT(temp, caption ="Flags")
  
return(list(temp1,temp2,temp3,temp4))
}


