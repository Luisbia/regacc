#' Get basic information from xml files
#'
#'This functions provides summary information (reporting periods, regions, flags and comments) from xml files. The output are dynamic HTML tables
#' @param ... same parameters as for load_xml
#'
#' @return four intervactive html tables
#' @export get_xml_basic_info
#'
#' @examples
#' get_xml_basic_info(folder = "D:/data/REGACC/xml",
#' country_sel="AT",
#' time_min = "2021-10-01")
#' 
get_xml_basic_info<- function(...){
  regacc::check_packages()
  
  data<-regacc::load_xml(...) %>% 
    as.data.table()
  
  temp<- unique(data[!obs_status %in% c("L","M") & !is.na(obs_value) ,.(first = min(time_period), last = max(time_period)),by =.(table_identifier,country,sto,accounting_entry,unit_measure)])%>% 
    .[,lapply(.SD, as.factor),] %>% # convert to factors for better filtering
    droplevels()

  
  temp1<-show_DT(temp, caption ="Reported periods")
  
  
  if ("embargo_date" %in% colnames(data)){
    temp<- na.omit(unique(files[,.(table_identifier,country,embargo_date)])) %>% 
      .[,lapply(.SD, as.factor),] %>% 
      droplevels()

    temp2<-show_DT(temp, caption ="Embargo dates")
  } else {
    temp2<- "No embargo dates"
  }

  if ("comment_ts" %in% names(df)) {
    temp<-na.omit(unique(data[comment_ts!="xxx" & comment_ts!="",.(country,table_identifier,comment_ts)])) %>%   
      .[,lapply(.SD, as.factor),] %>% 
      droplevels()
    temp3<-show_DT(temp, caption ="Comments")
  } else {
    temp2<- "No comments"
  }
    
  temp<-na.omit(unique(data[conf_status=="N",.(country,table_identifier,sto,activity,unit_measure,conf_status)]))%>% 
    .[,lapply(.SD, as.factor),] %>% 
    droplevels()
  temp3<-show_DT(temp, caption ="Data not publishable")
  
  temp<-na.omit(unique(data[obs_status %in% c("E","P","B","D","U"),.(country,table_identifier,sto,unit_measure,obs_status,time_period)])) %>% 
    .[,lapply(.SD, as.factor),] %>% 
    droplevels()  
  
  temp4<-show_DT(temp, caption ="Flags")
  
return(list(temp1,temp2,temp3,temp4))
}


