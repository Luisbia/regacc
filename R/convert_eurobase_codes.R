#' Convert Eurobase codes
#'
#' This function convert Eurobase column names and other codes (NACE) to the
#' codes used in transmissions. This allows an easier comparison between datasets.
#'
#'
#' @param df the dataframe with the eurobase data
#'
#' @return a dataframe with new column names and codes
#' @export convert_eurobase_codes
#'
#' @examples
#' df<- load_sent_eurobase(folder="E:/data/REGACC/sent",
#' table_sel = "nama_10r_2coe",
#' country_sel = "ES",
#' time_min = "2022-01-01")
#'
#' df1<-convert_eurobase_codes(df)
 convert_eurobase_codes <- function(df){

      regacc::check_packages()

     if("geo" %in% names(df)){
    df<- rename(df, ref_area = geo)
  }
  if("na_item" %in% names(df)){
    df<- rename(df, sto = na_item)
  }
  if("direct" %in% names(df)){
    df<- rename(df, accounting_entry = direct)
  }
  if("nace_r2" %in% names(df)){
    df<- rename(df, activity = nace_r2)
  }
  if("unit" %in% names(df)){
    df<- rename(df, unit_measure = unit)
  }
  if("time" %in% names(df)){
    df<- rename(df, time_period = time)
  }
  if("values" %in% names(df)){
    df<- rename(df, obs_value = values)
  }
  if("accounting_entry" %in% names(df)){
    df<- df %>%
      mutate(accounting_entry=case_when(accounting_entry == "BAL" ~ "B",
                                        accounting_entry == "RECV" ~ "C",
                                        accounting_entry == "PAID" ~ "D"))
  }
  if("activity" %in% names(df)){
    df<- df %>%
      mutate(activity = str_replace_all(activity,"B-E","BTE"),
             activity = str_replace_all(activity,"G-J","GTJ"),
             activity = str_replace_all(activity,"G-I","GTI"),
             activity = str_replace_all(activity,"K-N","KTN"),
             activity = str_replace_all(activity,"O-U","OTU"),
             activity = str_replace_all(activity,"O-Q","OTQ"),
             activity = str_replace_all(activity,"R-U","RTU"))
  }
 
  return(df)
}

