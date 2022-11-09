#' Update NQR data set with 2022 transmissions
#'
#' This function puts together the existing NQR vintages (up to 2021) and the newest one
#' from the validated series from a denodo extraction and creates a csv file
#' with the results in the chosen folder
#'
#' @param country_sel country to update
#' @param input_dir folder where the country denodo extraction is stored, data
#' should have been validated (V)
#' @param output_dir output folder for the file
#'
#' @return a csv file
#' @export update_NQR_data
#'
#' @examples
#' update_NQR_data(country_sel = "DK",
#' input_dir = "D:/check_input/data/denodo",
#' output_dir= "D:/")
update_NQR_data<- function(country_sel, input_dir, output_dir){

  prev<-dataregacc::NQR %>%
    filter(country == country_sel) %>%
    select(-label)


new<- list.files(path = input_dir,# modify as neccessary
                 pattern= glob2rx(paste0("regacc_", country_sel,"*.csv")),
                 full.names=TRUE) |>
  as_tibble() %>%
  mutate(date=map(value,file.mtime)) %>%
  unnest(date) %>%
  arrange(desc(date)) %>%
  head(1) %>%
  select(value) %>%
  pull() %>%
  data.table::fread() |>
  filter(type =="V" &
           activity %in% c("_T", "_Z") &
           sto %in% c("B1G", "EMP", "D1","B6N") &
           unit_measure %in% c("XDC", "PS") &
           NUTS %in% c("0","2")) |>
  mutate(vintage= 2022,
         NUTS = as.factor(NUTS))  %>%
  select(table_identifier,country, NUTS, ref_area, sto, vintage, time_period, obs_value)

t1001<- new |>
  filter(table_identifier =="T1001" & time_period == 2021)

t1002<- new |>
  filter(table_identifier =="T1002" & time_period < 2021)

t1200<- new |>
  filter(table_identifier =="T1200" & time_period < 2021)

t1300<- new |>
  filter(table_identifier =="T1300" & time_period < 2021 & sto =="B6N")

new<- bind_rows(t1001, t1002, t1200, t1300) |>
  select(-table_identifier) %>%
  mutate(NUTS=as.factor(NUTS))


rev_df_long <- bind_rows(prev,new)  %>%
  left_join(.,luispack::NUTS_2021)

data.table::fwrite(rev_df_long,paste0(output_dir,"/",country_sel,"_new.csv"))
cat(paste0("file created at: ",output_dir,"/",country_sel,"_new.csv"))
}


