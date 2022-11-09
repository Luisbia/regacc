#' Identify outliers based on the Z-score of annual change
#'
#' @param data a data frame with the original data.
#' @param z the factor to use, normally 2.5 or 3.
#' @param sto_sel possibly filter some transactions.
#' @param time_min possibly filter unrevised data.
#'
#' @return a data frame with the identified outliers
#' @export report_outliers
#'
#' @examples
#' df <- load_csv(
#' folder = "D:/data/REGACC/csv",
#' country_sel = "AT",
#' time_min = "2021-12-01")
#'
#' report_outliers(df)
#'
report_outliers<- function(data,z=3,sto_sel,time_min){
  check_packages()
  if(missing(sto_sel)){
    sto_sel<-unique(data$sto)
  }
  if(missing(time_min)){
    time_min<-min(data$time_period)
  }

  df1_outliers<- data %>%
    filter(sto %in% sto_sel) %>%
    select(country,ref_area,table_identifier,time_period, sto, accounting_entry, activity, unit_measure,obs_value) %>%
    filter(unit_measure %in% c("XDC","PS","HW")) %>%
    group_by(country,ref_area,table_identifier, sto, accounting_entry, activity, unit_measure) %>%
    arrange(time_period,by_group=TRUE) %>%
    mutate(change= obs_value-lag(obs_value)) %>%
    mutate(outlier_limit=round(mean(change,na.rm=TRUE)+z*sd(change,na.rm=TRUE),2)) %>%
    filter(abs(change)>outlier_limit) %>%
    filter(time_period>= time_min) %>%
    ungroup()

  return(df1_outliers)
}


