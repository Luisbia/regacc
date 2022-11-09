#' Identify outliers based on the Z-score based on several units (levels, diff(t-(t-1)) or  pc (t/(t-1))
#'
#' @param data a data frame with the original data.
#' @param unit level, diff (t-(t-1)) or  pc (t/(t-1))
#' @param z the factor to use, normally 2.5 or 3.
#' @param sto_sel possibly filter some transactions.
#' @param time_min possibly filter unrevised data.
#' @param abs_ths minimum threshold to do the test, 10 by default
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




report_outliers <- function(data,
                            unit = "level",
                            z = 3,
                            sto_sel,
                            time_min,
                            abs_ths=10) {
  regacc::check_packages()
  
  if (missing(sto_sel)) {
    sto_sel <- unique(data$sto)
  }
  
  if (missing(time_min)) {
    time_min <- min(data$time_period)
  }
  
  df1_outliers <- data %>%
    filter(sto %in% sto_sel) %>%
    select(
      country,
      ref_area,
      table_identifier,
      time_period,
      sto,
      accounting_entry,
      activity,
      unit_measure,
      obs_value
    ) %>%
    filter(unit_measure %in% c("XDC", "PS", "HW") & obs_value>abs_ths) %>%
    group_by(country,
             ref_area,
             table_identifier,
             sto,
             accounting_entry,
             activity,
             unit_measure) %>%
    arrange(time_period, by_group = TRUE)
  
  if (unit == "level") {
    df1_outliers <- df1_outliers %>%
      mutate(outlier_limit = round(
        mean(obs_value, na.rm = TRUE) + z * sd(obs_value, na.rm = TRUE),
        2
      )) %>%
      filter(abs(obs_value) > outlier_limit) %>%
      filter(time_period >= time_min) %>%
      ungroup()
    
    return(df1_outliers)
  }
  if (unit == "diff") {
    df1_outliers <- df1_outliers %>%
      mutate(diff= obs_value-lag(obs_value)) %>% 
      mutate(outlier_limit = round(
        mean(diff, na.rm = TRUE) + z * sd(diff, na.rm = TRUE),
        2
      )) %>%
      filter(abs(diff) > outlier_limit) %>%
      filter(time_period >= time_min) %>%
      ungroup()
    
    return(df1_outliers)
  } 
  if (unit == "pc") {
    df1_outliers <- df1_outliers %>%
      mutate(pc= round(obs_value/lag(obs_value)*100-100,2)) %>% 
      mutate(outlier_limit = round(
        mean(pc, na.rm = TRUE) + z * sd(pc, na.rm = TRUE),
        2
      )) %>%
      filter(abs(pc) > outlier_limit) %>%
      filter(time_period >= time_min) %>%
      ungroup()
    
    return(df1_outliers)
  } 
}

  





