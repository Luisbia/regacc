#' Load Matis big extractions
#' @description
#' Load txtextractions from REGACC for A_ tables (VAL.V.T1001.A.AT.W2.S1.S1.B.B1G._T.B.V.N.XDC) extracted horizontally and select the columns to keep.
#'
#' By default it keeps the most useful columns. To keep all columns use: keep_cols= c("VAL","type", "table_identifier", "freq", "ref_area",
#' "counterpart_area","ref_sector","counterpart_sector","accounting_entry", "sto", "activity", "valuation", "prices",
#' "transformation","unit_measure", "prices", "time","values","flag) or any subset.
#' @param file The file to load.
#' @param keep_cols The columns to keep.
#'
#' @return a data frame/ data.table
#' @export load_matis_txt
#'
#' @examples
#' df<-load_matis_txt("E:/regacc/data/matis/extract_data_manager_39_COUNTRIES_230106_120132.txt")
#'
#'
load_matis_txt <- function(file,
                       keep_cols) {
  options(warn = - 1)

  regacc::check_packages()
  df <- vroom::vroom(file, show_col_types = FALSE) %>% 
    mutate(across(everything(), 
                 as.character)) %>% 
    pivot_longer(cols = !starts_with("series"), 
                 names_to = "time_period", 
                 values_to = "obs_value") %>% 
    cSplit("series", sep = ".") %>% 
    rename(VAL = series_01, 
           type = series_02, table_identifier = series_03, freq = series_04, 
           ref_area = series_05, counterpart_area = series_06, 
           ref_sector = series_07, counterpart_sector = series_08, 
           accounting_entry = series_09, sto = series_10, activity = series_11, 
           valuation = series_12, prices = series_13, transformation = series_14, 
           unit_measure = series_15, ) %>% 
    select(-VAL) %>% 
    separate(obs_value, 
             into = c("obs_value", "flag"), 
             sep = "#") %>% 
    mutate(obs_value = as.numeric(obs_value), 
                                                                                                                                                          time_period = as.integer(time_period))
  if (missing(keep_cols)) {
    keep_cols = c("type", "table_identifier", "ref_area", 
                  "accounting_entry", "sto", "activity", "prices","unit_measure", 
                  "time_period", "obs_value", "flag")
  }
  df <- df %>% select(all_of(keep_cols))
  return(df)
  options(warn = 0)
}


