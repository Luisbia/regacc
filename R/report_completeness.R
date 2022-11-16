#' Report completeness as in NQR
#'
#'The function provides the same ratios as reported in the NQR and an interactive table with the missing values.
#' @param data a data frame with the reported data
#' @param table_sel individual table to check
#' @param verbose FALSE(default) shows the result as a donut chart. TRUE to get a table with the details.
#'
#' @return completeness ratios and missing series
#' @export report_completeness
#'
#' @examples
#' df<- load_xml("D:/regacc/data/input/xml",
#' country_sel = "ES",
#' table_sel = c("T1001","T1002","T1200","T1300"),
#' time_min = "2021-10-01")
#'
#' report_completeness (df,"T1001")
#'
report_completeness<- function(df, table_sel, verbose=FALSE){
  options(warn = - 1)
  country_sel<-unique(str_sub(df$ref_area,1,2))

  if("T1001" %in% table_sel){

    # T1001 ----
  table_identifier<- "T1001"
  ref_area<- NUTS_2021 %>%
    filter(NUTS !="3" & country %in% country_sel) %>%
    filter(label!="Extra-regio") %>%
    select(ref_area) %>%
  pull()

  accounting_entry <- c("B","_Z")
  sto <- c ("B1G", "EMP", "POP")
  activity<- c("_T", "_Z")
  unit_measure <-c ("XDC", "PC", "PS")
  country <- country_sel
  time_period<- seq(2000L,2021L,by=1L)

  temp <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

  temp1<- temp %>%
    filter(  sto =="B1G" &
               activity == "_T" &
               accounting_entry == "B" &
               unit_measure== "XDC" &
               time_period == 2021)

  temp2<- temp %>%
    filter(  sto =="B1G" &
               activity == "_T" &
               accounting_entry == "_Z" &
               unit_measure == "PC"&
               time_period >=2001 & time_period <=2020)

  temp3 <- temp %>%
    filter(  sto =="EMP" &
               activity == "_T" &
               accounting_entry == "_Z" &
               unit_measure== "PS" &
               time_period == 2021 )

  temp4 <- temp %>%
    filter(   sto =="POP" &
                activity == "_Z" &
                accounting_entry == "_Z" &
                unit_measure== "PS" &
                time_period == 2021)

  t1001<- bind_rows(temp1, temp2, temp3, temp4) %>%
    mutate(NUTS=as.factor(str_length(ref_area)-2))
  rm(temp1,temp2,temp3,temp4)


  t1001<- left_join(t1001,df)
  notrep_1001<- t1001 %>%
    filter(is.na(obs_value))

  notrep_1001_n<-nrow(notrep_1001)

  tot_1001<- nrow(t1001) # count all

  ratio_1001<-round((tot_1001-notrep_1001_n)/tot_1001,2) #calculate the ratio
  p<-donut_plot(ratio_1001,title="Table 1001")
  if(verbose==FALSE){
  return(p)}

  if (verbose==TRUE){
  show_missing_t1001 <- notrep_1001 %>%
    select(table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value) %>%
    show_DT()
  return(show_missing_t1001)}
  }

  if("T1002" %in% table_sel){

    table_identifier<- "T1002"
    ref_area<- NUTS_2021 %>%
      filter(NUTS !="3"  & country== country_sel) %>%
      filter(label!="Extra-regio") %>%
      select(ref_area) %>%
      pull()
    accounting_entry <- c("D","_Z")
    sto <- c ("D1", "P51G", "EMP", "SAL")
    activity<- c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU")
    unit_measure <-c ("XDC","HW")
    country <- country_sel
    time_period<- seq(2000L,2020L,by=1L)

    temp <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

    temp1 <- temp %>%
      filter(accounting_entry == "D" &
               sto %in% c("D1", "P51G") &
               unit_measure == "XDC")

    temp2 <- temp %>%
      filter(accounting_entry == "_Z" &
               sto %in% c("EMP", "SAL") &
               unit_measure == "HW")

    t1002<- bind_rows(temp1, temp2)%>%
      mutate(NUTS=as.factor(str_length(ref_area)-2))

    t1002<- left_join(t1002,df)
    notrep_1002<- t1002 %>%
      filter(is.na(obs_value))

    notrep_1002_n<-nrow(notrep_1002)

    tot_1002<- nrow(t1002) # count all

    ratio_1002<-round((tot_1002-notrep_1002_n)/tot_1002,2) #calculate the ratio
    p<-donut_plot(ratio_1002,title="Table 1002")
    if(verbose==FALSE){
      return(p)}
    if (verbose==TRUE){
    show_missing_t1002 <- notrep_1002 %>%
      select(table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value) %>%
    show_DT()
    return(show_missing_t1002)}
  }

  if("T1200" %in% table_sel){
    table_identifier<- "T1200"
    ref_area<- NUTS_2021 %>%
      filter(label!="Extra-regio" & country ==country_sel) %>%
      select(ref_area) %>%
      pull()
    accounting_entry <- c("B","_Z")
    sto <- c ("B1G", "POP", "EMP", "SAL")
    activity<- c("_Z","_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU")
    unit_measure <-c ("PS","XDC")
    country <- country_sel
    time_period<- seq(2000L,2020L,by=1L)


    temp <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

    temp1 <- temp %>%
      filter(accounting_entry == "_Z" &
               sto =="POP" &
               activity == "_Z" &
               unit_measure == "PS")

    temp2 <- temp %>%
      filter(  sto =="B1G" &
                 accounting_entry == "B" &
                 activity %in% c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU") &
                 unit_measure == "XDC")

    temp2a<- left_join(temp2,NUTS_2021) %>%
      filter (NUTS !="3")

    temp2b<- left_join(temp2,NUTS_2021) %>%
      filter (NUTS =="3" & activity %in% c("_T", "A", "BTE", "C", "F", "GTJ", "KTN",  "OTU"))

    temp2 <- bind_rows(temp2a,temp2b)%>%
      select(-NUTS, -label)

    temp3 <- temp %>%
      filter(  sto %in% c("EMP", "SAL") &
                 accounting_entry == "_Z" &
                 activity %in% c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU") &
                 unit_measure == "PS")

    temp3a<- left_join(temp3,NUTS_2021) %>%
      filter (NUTS !="3")

    temp3b<- left_join(temp3,NUTS_2021) %>%
      filter (NUTS =="3" & activity %in% c("_T", "A", "BTE", "C", "F", "GTJ", "KTN",  "OTU"))

    temp3 <- bind_rows(temp3a,temp3b) %>%
      select(-NUTS, -label)


    t1200<- bind_rows(temp1, temp2, temp3)%>%
      mutate(NUTS=as.factor(str_length(ref_area)-2))

    t1200<- left_join(t1200,df)
    notrep_1200<- t1200 %>%
      filter(is.na(obs_value))

    notrep_1200_n<-nrow(notrep_1200)

    tot_1200<- nrow(t1200) # count all

    ratio_1200<-round((tot_1200-notrep_1200_n)/tot_1200,2) #calculate the ratio
    p<-donut_plot(ratio_1200,title="Table 1200")
    if(verbose==FALSE){
      return(p)}
    if (verbose==TRUE){
    show_missing_t1200 <- notrep_1200 %>%
      select(table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value) %>%
    show_DT()
      return(show_missing_t1200)}
  }

  if("T1300" %in% table_sel){
    table_identifier<- "T1300"
    ref_area<- NUTS_2021 %>%
      filter(NUTS !="3" & country == country_sel) %>%
      filter(label!="Extra-regio") %>%
      select(ref_area) %>%
      pull()
    accounting_entry <- c("B","C","D")
    sto <- c ("B5N", "B6N", "B2A3N", "D1", "D4", "D61", "D62", "D7", "D5")
    activity<- c("_Z")
    unit_measure <-c ("XDC")
    country <- country_sel
    time_period<- seq(2000L,2020L,by=1L)


    temp <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

    temp1<- temp %>%
      filter(accounting_entry == "B" &
               sto %in% c("B5N", "B6N", "B2A3N"))

    temp2<- temp %>%
      filter(accounting_entry == "C" &
               sto %in% c("D1", "D4", "D62", "D7"))

    temp3<- temp %>%
      filter(accounting_entry == "D" &
               sto %in% c("D4", "D5", "D61",  "D7"))

    t1300<- bind_rows(temp1, temp2, temp3)%>%
      mutate(NUTS=as.factor(str_length(ref_area)-2))

    t1300<- left_join(t1300,df)
    notrep_1300<- t1300 %>%
      filter(is.na(obs_value))

    notrep_1300_n<-nrow(notrep_1300)

    tot_1300<- nrow(t1300) # count all

    ratio_1300<-round((tot_1300-notrep_1300_n)/tot_1300,2) #calculate the ratio
    p<-donut_plot(ratio_1300,title="Table 1300")
    if(verbose==FALSE){
      return(p)}
    if (verbose==TRUE){
    show_missing_t1300 <- notrep_1300 %>%
      select(table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value) %>%
    show_DT()
    return(show_missing_t1300)}
  }
}




