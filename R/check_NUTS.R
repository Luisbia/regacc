#' Check NUTS additivity
#'
#' Function for checking NUTS additivity. It requires a data frame with at least the following columns
#' country,ref_area,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, obs_value
#'
#' @param data a data frame
#' @param ths_abs absolute threshold
#' @param ths_rel relative threshold in % (1=1%)
#'
#' @return a data frame with the data not fulfilling the consistency requirements
#' @export check_NUTS
#'
#' @examples
#' df <- load_csv(
#' folder = "E:/data/REGACC/csv",
#' country_sel = c("AT"),
#' table_sel = "T1002",
#' time_min = "2021-12-01")
#'
#' check_NUTS(df)
#'
#'
check_NUTS <- function(data,ths_abs=2,ths_rel=0.05){

  check_packages()

if ("T1200" %in% unique(data$table_identifier)){
  ### NUTS ----

  # prepare dataset
  df<- data %>%
    select(country,ref_area,table_identifier,time_period, sto, accounting_entry, activity, unit_measure,obs_value) %>%
    filter(unit_measure %in% c("XDC","PS","HW")) %>%
    mutate(NUTS=str_length(ref_area)-2)

  temp<- df %>%
    filter(NUTS=="1") %>%
    mutate(geo1 = substr(ref_area, start = 1, stop = 2)) %>%
    group_by(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo1) %>%
    mutate(sum = sum(obs_value)) %>%
    select(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo1, sum) %>%
    distinct() %>%
    rename(ref_area ="geo1") %>%
    ungroup()


  NUTS1 <- left_join(temp, df) %>%
    select(country,table_identifier,NUTS,time_period, sto, accounting_entry, activity, unit_measure, ref_area, sum, obs_value) %>%
    mutate(diff = round(sum - obs_value, digits = 0),
           diffp = round (diff * 100/ obs_value, digits = 1))

  ## NUTS 1----
  temp<-df %>%
    filter(NUTS=="2") %>%
    mutate(geo2 = substr(ref_area, start = 1, stop = 3)) %>%
    group_by(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo2) %>%
    mutate(sum = sum(obs_value)) %>%
    select(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo2, sum) %>%
    distinct() %>%
    rename(ref_area ="geo2") %>%
    ungroup()


  NUTS2 <- left_join(temp, df) %>%
    select(country,table_identifier,NUTS,time_period, sto, accounting_entry, activity, unit_measure, ref_area, sum, obs_value) %>%
    mutate(diff = round(sum - obs_value, digits = 0),
           diffp = round (diff * 100/ obs_value, digits = 1))

  ## NUTS 2----
  temp<-df %>%
    filter(NUTS=="3") %>%
    mutate(geo3 = substr(ref_area, start = 1, stop = 4)) %>%
    group_by(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo3) %>%
    mutate(sum = sum(obs_value)) %>%
    select(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo3, sum) %>%
    distinct() %>%
    rename(ref_area ="geo3") %>%
    ungroup()


  NUTS3 <- left_join(temp, df) %>%
    select(country,table_identifier,NUTS,time_period, sto, accounting_entry, activity, unit_measure, ref_area, sum, obs_value) %>%
    mutate(diff = round(sum - obs_value, digits = 0),
           diffp = round (diff * 100/ obs_value, digits = 1))


  NUTS <- bind_rows(NUTS1, NUTS2, NUTS3) %>%
    filter(abs(diff) > ths_abs ) %>%
    filter(abs(diffp) > ths_rel)
  } else {
    # prepare dataset
    df<- data %>%
      select(country,ref_area,table_identifier,time_period, sto, accounting_entry, activity, unit_measure,obs_value) %>%
      filter(unit_measure %in% c("XDC","PS","HW")) %>%
      mutate(NUTS=str_length(ref_area)-2)

    temp<- df %>%
      filter(NUTS=="1") %>%
      mutate(geo1 = substr(ref_area, start = 1, stop = 2)) %>%
      group_by(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo1) %>%
      mutate(sum = sum(obs_value)) %>%
      select(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo1, sum) %>%
      distinct() %>%
      rename(ref_area ="geo1") %>%
      ungroup()


    NUTS1 <- left_join(temp, df) %>%
      select(country,table_identifier,NUTS,time_period, sto, accounting_entry, activity, unit_measure, ref_area, sum, obs_value) %>%
      mutate(diff = round(sum - obs_value, digits = 0),
             diffp = round (diff * 100/ obs_value, digits = 2))

    ## NUTS 1----
    temp<-df %>%
      filter(NUTS=="2") %>%
      mutate(geo2 = substr(ref_area, start = 1, stop = 3)) %>%
      group_by(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo2) %>%
      mutate(sum = sum(obs_value)) %>%
      select(country,table_identifier,time_period, sto, accounting_entry, activity, unit_measure, geo2, sum) %>%
      distinct() %>%
      rename(ref_area ="geo2") %>%
      ungroup()


    NUTS2 <- left_join(temp, df) %>%
      select(country,table_identifier,NUTS,time_period, sto, accounting_entry, activity, unit_measure, ref_area, sum, obs_value) %>%
      mutate(diff = round(sum - obs_value, digits = 0),
             diffp = round (diff * 100/ obs_value, digits = 2))


    NUTS <- bind_rows(NUTS1, NUTS2) %>%
      na.omit() %>%
      filter(abs(diff) > ths_abs ) %>%
      filter(abs(diffp) > ths_rel)
    }

  }


