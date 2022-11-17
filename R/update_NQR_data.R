#' Update NQR data set with 2022 transmissions
#'
#' This function puts together the existing NQR vintages (up to 2021) and the newest one
#' from the validated series from a denodo extraction and creates a csv file
#' with the results in the chosen folder
#'
#' @param country_sel country to update
#' @param input_file parquet file with an extraction of primary data
#' @param output_dir output folder for the file
#'
#' @return a csv file
#' @export update_NQR_data
#'
#' @examples
#' update_NQR_data(country_sel = "DK",
#' input_file = "D:/check_input/data/denodo/all_primary.parquet",
#' output_dir= "D:/")
update_NQR_data<- function(country_sel, input_file="data/denodo/all_primary.parquet", output_dir="NQR/data"){
regacc::check_packages()

data<- read_parquet(input_file)

t1001<- data %>% 
  filter(country == country_sel &
           table_identifier=="T1001" & 
           time_period==2021 & 
           activity=="_T" & 
           sto %in% c("B1G", "EMP") &
           prices!="Y"  &
           unit_measure %in% c("PS","XDC") &
           NUTS %in% c(0,2))

t1200 <- data %>%
  filter(
    country == country_sel &
      type == "V" &
      table_identifier == "T1200" &
      time_period < 2021 & time_period >= 2012 &
      activity == "_T" &
      sto %in% c("B1G", "EMP") &
      unit_measure %in% c("PS", "XDC") &
      prices != "Y"  &
      NUTS %in% c(0, 2)
  )


t1002 <- data %>%
  filter(
    country == country_sel &
      type == "V" &
      table_identifier == "T1002" &
      time_period >= 2012 & time_period <= 2020 &
      activity == "_T" &
      sto %in% c("D1") &
      unit_measure %in% c("XDC") &
      NUTS %in% c(0, 2)
  )


t1300<- data %>% 
  filter(country == country_sel &
           type =="V" &
           table_identifier=="T1300" & 
           time_period>= 2012 & time_period<= 2020 &
           sto %in% c("B6N") &
           unit_measure %in% c("XDC") &
           NUTS %in% c(0,2))

new<- bind_rows(t1001,t1002,t1200,t1300) %>% 
  mutate(vintage = 2022) %>% 
  select(country,NUTS,ref_area,sto,vintage,time_period,obs_value)


prev<-dataregacc::NQR %>% 
  filter(country==country_sel) %>% 
  select(-label)


data<- bind_rows(prev,new) %>% 
  right_join(dataregacc::NUTS_2021,.)

data.table::fwrite(data,paste0(output_dir,"/",country_sel,".csv"))

cli::cli_alert_success(paste0("file created at: ",output_dir,"/",country_sel,".csv"))
}


