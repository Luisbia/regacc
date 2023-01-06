
#' Calculate B1G in previous year prices for Matis
#' 
#' The function creates a csv that can be later on loaded in Matis.
#'
#' @param country_sel Country
#' @param nat_dat A dataframe with the national data on GVA at previous year prices. It should have the following columns: country, sto, prices, time_period, obs_value.
#' @param reg_dat A dataframe with regional data. It should have the following columns: country, ref_area, NUTS, sto, prices, time_period, obs_value.
#' @param output_dir the folder where t create the file
#'
#' @return a csv file
#' @export calculate_gva_pyp
#'
#' @examples
#' calculate_gva_pyp("LU",nat_nama,regacc,"vol_GDP/output")
calculate_gva_pyp<- function(country_sel,nat_dat,reg_dat,output_dir){
  
  gva_pyp<- reg_dat %>%  # to change 
    filter(sto %in% c("B1G")) %>%
    select(-prices) %>% 
    pivot_wider(names_from = unit_measure,
                values_from = obs_value) %>% 
    group_by(country,ref_area) %>% 
    arrange(time_period,.by_group = TRUE) %>% 
    mutate(Y = (1+PC/100) * lag(XDC)) %>% 
    ungroup() %>% 
    select(-PC,-XDC) %>% 
    pivot_longer(cols = c(Y),
                 names_to = "prices",
                 values_to = "obs_value") %>% 
    na.omit() %>% 
    mutate(unit_measure ="XDC")
  
  #### Create a total that is the sum NUTS 1
  gva_pyp_total1 <- gva_pyp %>% 
    filter(NUTS =="1") %>% 
    group_by(country,time_period) %>% 
    mutate(nat=sum(obs_value)) %>% 
    ungroup() %>% 
    select(country,time_period,prices,nat) %>% 
    unique()
  
  #### Create a total that is the sum NUTS 2
  gva_pyp_total2 <- gva_pyp %>% 
    filter(NUTS =="2") %>% 
    group_by(country,time_period) %>% 
    mutate(nat=sum(obs_value)) %>% 
    ungroup() %>% 
    select(country,time_period,prices,nat) %>% 
    unique()
  
  
  # calculate shares for NUTS 1  
  gva_shares1 <- left_join(gva_pyp_total1,gva_pyp)%>%
    filter(NUTS ==1 & unit_measure =="XDC") %>% 
    mutate(share=obs_value/nat) %>% 
    select(-unit_measure,-obs_value, -sto,-nat)
  
  # calculate shares for NUTS 2  
  gva_shares2 <- left_join(gva_pyp_total2,gva_pyp)%>%
    filter(NUTS ==2 & unit_measure =="XDC") %>% 
    mutate(share=obs_value/nat) %>% 
    select(-unit_measure,-obs_value, -sto,-nat)
  
  #multiply shares by national GVA
  gva_pyp1<- nat_dat %>% 
    filter(sto=="B1G" & prices =="Y") %>% 
    left_join(gva_shares1,.) %>% 
    na.omit() %>% 
    mutate(obs_value=share*obs_value) %>% 
    select(-share) %>% 
    mutate(unit_measure="XDC")
  
  gva_pyp2<- nat_dat %>% 
    filter(sto=="B1G" & prices =="Y") %>% 
    left_join(gva_shares2,.) %>% 
    na.omit() %>% 
    mutate(obs_value=share*obs_value) %>% 
    select(-share) %>% 
    mutate(unit_measure="XDC")
  
  gva_pyp0<- nat_dat %>% 
    filter(sto =="B1G" & prices =="Y") %>% 
    mutate(ref_area=country,
           NUTS="0",
           NUTS=as.factor(NUTS),
           unit_measure="XDC")
  
  gva_pyp<- bind_rows(gva_pyp0,gva_pyp1,gva_pyp2)
  
  check<- gva_pyp %>% 
    mutate(table_identifier="gva_pyp",
           accounting_entry="Z",
           activity="Z") %>% 
    check_NUTS(ths_abs =0, ths_rel = 0.0)
  
  if(nrow(check)==0){
    cli::cli_alert_success("Everything adds up!")
  } else {
    regacc::show_DT(check)
  }
  
  final_t1001<- gva_pyp %>% 
    filter(time_period>2000) %>% 
    mutate(series=paste0("VAL.T.T1001.A.",ref_area,".W2.S1.S1.B.B1G._T.B.Y.N.XDC")) %>%
    select(series,time_period,obs_value) %>% 
    mutate(obs_value=paste0(obs_value,"#E0")) %>% 
    pivot_wider(names_from=time_period,
                values_from=obs_value) %>% 
    arrange(series) %>% 
    rename(ANNUAL=series)
  
  final_t1200<- final_t1001 %>% 
    mutate(ANNUAL=str_replace(ANNUAL,"T1001","T1200"))
  
  data.table::fwrite(final_t1001,paste0(output_dir,"/", country_sel,"_T1001_B1G_PYP.csv"),sep=";")
  data.table::fwrite(final_t1200,paste0(output_dir,"/", country_sel,"_T1200_B1G_PYP.csv"),sep=";")
  
  cli::cli_alert_success(paste0("Files created at: ",output_dir))
}
