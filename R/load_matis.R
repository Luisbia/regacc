#' Load Matis
#' @description
#' Load excel extractions from REGACC for A_ tables (VAL.T.T1001.A.AT.W2.S1.S1.B.B1G._T.B.V.N.XDC) extracted horizontally and select the columns to keep.
#'
#' By default it keeps the most useful columns. To keep all columns use: keep_cols= c("VAL","type", "table_identifier", "freq", "ref_area",
#' "counterpart_area","ref_sector","counterpart_sector","accounting_entry", "sto", "activity", "valuation", "prices",
#' "transformation","unit_measure", "time","values","flag) or any subset.
#' @param file The file to load.
#' @param keep_cols The columns to keep.
#'
#' @return a data frame/ data.table
#' @export load_matis
#'
#' @examples
#' df<-load_matis("D:/03_Regional Accounts/03D_Data Production/2021/BE/matis/t1200_BE_01.xlsx",keep_cols= c("type","ref_area","sto","activity","time_period","obs_value"))
#'
#'
load_matis <- function(file,
                       keep_cols) {
  options(warn = - 1)

  regacc::check_packages()


  df <-rio::import(file) %>%
    mutate(across(everything(),as.character)) %>%
    pivot_longer(cols= !starts_with("ANNUAL"),
                 names_to = "time_period",
                 values_to = "obs_value") %>%
    cSplit("ANNUAL", sep = ".") %>%
    rename("VAL"= ANNUAL_01,
           "type"=ANNUAL_02,
           "table_identifier"=ANNUAL_03,
           "freq"=ANNUAL_04,
           "ref_area"=ANNUAL_05,
           "counterpart_area"=ANNUAL_06,
           "ref_sector"=ANNUAL_07,
           "counterpart_sector"=ANNUAL_08,
           "accounting_entry"=ANNUAL_09,
           "sto"=ANNUAL_10,
           "activity"=ANNUAL_11,
           "valuation"=ANNUAL_12,
           "prices"=ANNUAL_13,
           "transformation"=ANNUAL_14,
           "unit_measure"=ANNUAL_15,
    ) %>%
    select(-VAL) %>%
    separate(obs_value,into=c("obs_value","flag"),sep="#") %>%
    mutate(obs_value=as.numeric(obs_value),
           time_period=as.integer(time_period))

  if(missing(keep_cols)){
    keep_cols= c("type","table_identifier", "ref_area","accounting_entry","sto","activity","unit_measure","time_period","obs_value","flag")
  }

  df<- df %>%
    select(all_of(keep_cols))

  return(df)

  options(warn = 0)
}


