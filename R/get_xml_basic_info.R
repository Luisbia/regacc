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
  
  files<-regacc::load_xml(...)
  
  temp<- files %>% 
    group_by(table,sto,activity,unit_measure,accounting_entry) %>% 
    select(value,country,time_period,sto,activity,unit_measure,accounting_entry,obs_value,obs_status) %>% 
    drop_na(obs_value) %>% 
    filter(!obs_status %in% c("M", "L")) %>% 
    mutate(first=min(time_period),
           last=max(time_period)) %>% 
    ungroup() %>% 
    select(value,country,table,sto,activity,unit_measure,accounting_entry,first,last) %>% 
    unique()
  
  temp1<-show_DT(temp, caption ="Reported periods")
  
  
temp<- files %>% 
  select(value,country,table,ref_area,time_period,sto,activity,unit_measure,accounting_entry,obs_value) %>% 
  na.omit() 

NUTS <- dataregacc::NUTS_2021 %>% 
  select(-label)

temp<- left_join(temp,NUTS) %>%
  select(value,table,country,NUTS,time_period,sto,activity,unit_measure,accounting_entry) %>% 
  group_by(value,table,country,NUTS,sto,unit_measure) %>% 
  count()

temp2<-show_DT(temp, caption = "Reported ref_area")

temp<- files

if ("embargo_date" %in% colnames(temp)){
  temp<- temp %>% 
    select(value,country,table,ref_area,time_period,sto,activity,unit_measure,accounting_entry,obs_value, obs_status,conf_status,embargo_date) %>% 
    filter(conf_status !="F") %>% 
    na.omit() %>%  
    group_by(value,table,country,time_period,obs_status, conf_status,embargo_date) %>% 
    count()
  
} else {
  temp<- temp %>% 
    select(value,table,country,time_period,sto,activity,unit_measure,accounting_entry,obs_status,conf_status) %>% 
    group_by(value,table,country,time_period,obs_status, conf_status) %>% 
    count()
  
}
temp3<-show_DT(temp, caption = "Reported flags")

temp<- files %>%
  ungroup() 

if ("comment_ts" %in% colnames(files)){
  temp<- temp %>% 
    select(value,country,table,sto,unit_measure,comment_ts) %>%
    na.omit() %>% 
    unique()
  
  temp4<-show_DT(temp, caption = "Comments")
  
} else {
  print("No comments")
}

return(list(temp1,temp2,temp3,temp4))
}


