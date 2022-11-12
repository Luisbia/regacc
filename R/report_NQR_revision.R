#' NQR revision file
#'Function to create the NRQ revision report that we sent to countries
#'Needs to be updated every year to change some parameters
#' @param dat external csv file with the data
#' @param country_sel country
#' @param output_dir folder to store the generated excel file
#'
#' @return an excel report
#' @export report_NQR_revision
#'
#' @examples
#' report_NQR_revision(dat="D:/DK_new.csv",
#'                     country_sel = "DK",
#'                     output_dir="D:/")
#'
report_NQR_revision <- function(dat,country_sel,output_dir){
regacc::check_packages()
  options(dplyr.summarise.inform = FALSE)
    rev_df_long<- data.table::fread(dat) %>%
  filter(time_period >= 2015) %>%
  filter(vintage >= 2017 & vintage<= 2022) %>%
  mutate(
    time_period = as.integer(time_period),
    vintage = as.integer(vintage)
  ) |>
  arrange(sto,ref_area,vintage)

### check is complete

limit<- NUTS_2021 %>%
  filter(country %in% country_sel & NUTS %in% c("0","2")) %>%
  count() %>%
  pull()

comp_B1G_EMP<- rev_df_long %>%
  filter(!is.na(obs_value)  & sto %in% c("B1G", "EMP") &
           time_period>=2016) %>%
  group_by(sto,vintage,time_period) %>%
  count() %>%
  filter(n!= limit) %>%
  print()

comp_D1_B6N<- rev_df_long %>%
  filter(!is.na(obs_value)  & sto %in% c("D1", "B6N")) %>%
  group_by(sto,vintage,time_period) %>%
  count() %>%
  filter(n!=limit) %>%
  print()

##### Shares ----
shares_long<- rev_df_long %>%
  group_by(country, sto, time_period, vintage) %>%
  mutate(shares= obs_value/obs_value[NUTS =="0"]) %>%
  ungroup() %>%
  na.omit()

#### Revisions ------
rev_long <- shares_long %>%
  group_by(country, ref_area, sto, time_period) %>%
  arrange(vintage, .by_group = TRUE) %>%
  mutate(rev = abs(shares/lag(shares)*100-100)) %>%
  ungroup()

####Mean revision----
mean_rev_long<- rev_long %>%
  group_by(country, ref_area, sto, time_period) %>%
  arrange(vintage, .by_group = TRUE) %>%
  mutate( mean_rev= mean(rev,na.rm=TRUE)) %>%
  ungroup()

#### Mean_weights------
mean_weights_long <- shares_long %>%
  group_by(country, ref_area, sto, time_period) %>%
  mutate(mean_weights = mean(shares, na.rm = TRUE)) %>%
  unique() %>%
  ungroup()

#### Weighted revision------

weighted_rev_long <- full_join(mean_rev_long,mean_weights_long) %>%
  arrange(ref_area, sto, vintage, time_period) %>%
  select(-obs_value,-shares,-rev,-vintage) %>%
  unique() %>%
  group_by(country, ref_area, sto, time_period) %>%
  mutate(weighted_rev = mean_rev*mean_weights) %>%
  select(country, NUTS, ref_area, sto, time_period, weighted_rev) %>%
  ungroup() %>%
  filter(time_period!=2021 & weighted_rev !="NaN") ######################CAREFUL-----

##### Final calculation
final<- weighted_rev_long %>%
  filter(NUTS =="2") %>%
  group_by(country, sto, time_period) %>%
  summarise(indic = sum(weighted_rev,na.rm=TRUE))

#### By indicator
final_b1g_emp<- final %>%
  filter(sto %in% c("B1G","EMP") &
           time_period %in% c("2016","2017","2018","2019","2020")) ######################CAREFUL-----

final_d1_b6n<- final %>%
  filter(sto%in% c("D1","B6N") &
           time_period %in% c("2015","2016","2017","2018","2019")) ######################CAREFUL-----

final_long<- bind_rows(final_b1g_emp, final_d1_b6n) %>%
  arrange(time_period)

final_wide<- final_long %>%
  mutate(indic=janitor::round_half_up(indic,5)) %>%
  pivot_wider(names_from = time_period,
              values_from= indic)

#### Order indicator
x<-final_wide$sto
final_wide$sto<- factor(x, levels=c("B1G","EMP","D1","B6N"))

final_wide<- final_wide %>%
  arrange(sto)

#### Last minus first -----

first_last<- function(df,stos,time_periods,vintages){
  rev_df_long %>%
    filter(sto %in% stos & time_period == time_periods & vintage %in% vintages) %>%
    na.omit() %>%
    group_by(country,sto,time_period,vintage) %>%
    mutate(shares= obs_value/obs_value[NUTS =="0"]) %>%
    ungroup() %>%
    group_by(country, ref_area, sto, time_period) %>%
    arrange(vintage, .by_group = TRUE) %>%
    mutate(rev = abs(shares/shares[vintage== min(vintages)]*100-100)) %>%
    ungroup() %>%
    group_by(country, ref_area, sto, time_period) %>%
    mutate(meanshares = mean(shares),
           wrev = rev*meanshares) %>%
    select(country, NUTS, ref_area, sto, time_period, rev, meanshares, wrev) %>%
    na.omit() %>%
    group_by(country, NUTS, sto, time_period) %>%
    filter(NUTS=="2") %>%
    summarise(regwrev=sum(wrev,na.rm=TRUE)) %>%
    ungroup() %>%
    select(-NUTS) %>%
    distinct()
}

# can be made longer for other purposes

b1g_emp_2016<- first_last(rev_df,c("B1G","EMP"),2016,c("2017","2021"))
b1g_emp_2017<- first_last(rev_df,c("B1G","EMP"),2017,c("2018","2021"))
b1g_emp_2018<- first_last(rev_df,c("B1G","EMP"),2018,c("2019","2021"))
b1g_emp_2019<- first_last(rev_df,c("B1G","EMP"),2019,c("2020","2021"))
b1g_emp_2020<- first_last(rev_df,c("B1G","EMP"),2020,c("2021","2022"))

d1_b6n_2015<- first_last(rev_df,c("D1","B6N"),2015,c("2017","2021"))
d1_b6n_2016<- first_last(rev_df,c("D1","B6N"),2016,c("2018","2021"))
d1_b6n_2017<- first_last(rev_df,c("D1","B6N"),2017,c("2019","2021"))
d1_b6n_2018<- first_last(rev_df,c("D1","B6N"),2018,c("2020","2021"))
d1_b6n_2019<- first_last(rev_df,c("D1","B6N"),2019,c("2021","2022"))

first_last<- bind_rows(b1g_emp_2016, b1g_emp_2017,b1g_emp_2018,
                       b1g_emp_2019,b1g_emp_2020, d1_b6n_2015,d1_b6n_2016,
                       d1_b6n_2017,d1_b6n_2018,d1_b6n_2019) %>%
  arrange(time_period) %>%
  mutate(regwrev=janitor::round_half_up(regwrev,5)) %>%
  pivot_wider(names_from=time_period,
              values_from=regwrev)

x<-first_last$sto
first_last$sto<- factor(x, levels=c("B1G","EMP","D1","B6N"))

first_last<- first_last %>%
  arrange(sto)

##### Things for excel file

##### Chart

#### For chart (change defaults if needed)
nat_dot<- 4
reg_dot<- 2.5
line_chart<-1
height_chart <-6
width_chart<-9


temp<- weighted_rev_long %>%
  filter(NUTS=="2") %>%
  group_by(ref_area,sto) %>%
  mutate(mean=mean(weighted_rev,na.rm=TRUE)) %>%
  ungroup() %>%
  select(ref_area, sto, mean) %>%
  distinct() %>%
  group_by(sto) %>%
  mutate(contr=sum(mean,na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(sto) %>%
  mutate(share_rev=mean*100/contr) %>%
  select(ref_area, sto, share_rev)


temp1<- mean_weights_long %>%
  filter(NUTS=="2") %>%
  group_by(ref_area, sto) %>%
  mutate(mean=100*mean(mean_weights,na.rm=TRUE)) %>%
  select(ref_area, sto, mean) %>%
  distinct()

temp<- full_join(temp,temp1) %>%
  mutate(diff=mean-share_rev)

mycaption <- '<span style="color:#14509E;">Share in variable</span> /
                <span style="color:#F51A0E;">Share in revision indicator</span> /
                <span style="color:#59D122;">Share in revision indicator</span>'


reg_plot<-ggplot()+
  geom_point(data=temp,aes(mean,reorder(ref_area,mean)),size=nat_dot,colour="#14509E")+
  geom_point(data=temp %>% filter(diff>0),aes(share_rev,reorder(ref_area,mean)),size=reg_dot,colour="#59D122")+
  geom_point(data=temp %>% filter(diff<0),aes(share_rev,reorder(ref_area,mean)),size=reg_dot,colour="#F51A0E")+
  geom_segment(data=temp%>% filter(diff>0),aes(x=mean,xend=share_rev,y=ref_area,yend=ref_area),size=line_chart,colour="#59D122")+
  geom_segment(data=temp%>% filter(diff<0),aes(x=mean,xend=share_rev,y=ref_area,yend=ref_area),size=line_chart,colour="#F51A0E")+
  facet_grid(~sto)+
  #coord_cartesian(expand=TRUE, clip = "off" )+
  theme(plot.background = element_rect(fill= "#FFFFFF", #panel background
                                       colour = "black"),# border lines
        line = element_line(colour = "black"),
        rect = element_rect(fill = "#FAFAFA",
                            linetype = 0,
                            colour = NA),
        panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
        axis.title = element_blank(), #axis titles
        axis.line.x = element_line(size = rel(1.1),colour = "#14509E" ),#line of axis
        axis.line.y = element_line(size = rel(1.1), colour = "#14509E"),
        axis.text.y = element_text(colour = "#14509E", size = rel(1.1)),
        axis.text.x = element_text(colour = "#14509E", size = rel(1.1)),
        axis.ticks = element_line(size =rel(1.1),colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(fill="white"),
        legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
        plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
        strip.background = element_rect(fill = "#14509E", colour = NA),
        strip.text.x = element_text(face = "bold",  size = rel (1.2), colour = "white" ), #margin = margin()
        strip.text.y = element_text(face = "bold", size = rel (1.2),  colour = "white"),
        strip.placement = "outside",
        panel.spacing = unit(2, "lines"))+
  labs(title= paste0(unique(str_sub(temp$ref_area,1,2)),": Regional breakdown of the indicator, average for the last 5 years"),
       subtitle = mycaption) +
  theme(plot.subtitle = ggtext::element_markdown(hjust = 0, size = rel(1.3)))

### Table for excel

rev_df_wide<- rev_df_long %>%
  pivot_wider(names_from=time_period,
              values_from=obs_value) %>%
  select(-country)

shares_wide <- shares_long %>%
  select(-obs_value) %>%
  mutate(shares=round(shares*100,5)) %>%
  pivot_wider(names_from=time_period,
              values_from=shares) %>%
  arrange(sto, ref_area, vintage) %>%
  filter(NUTS=="2") %>%
  select(-country,-NUTS)

rev_wide <- rev_long %>%
  na.omit() %>%
  select(-obs_value,-shares) %>%
  mutate(rev=round(rev,5)) %>%
  pivot_wider(names_from=time_period,
              values_from=rev) %>%
  arrange(sto,ref_area,vintage)%>%
  filter(NUTS=="2")%>%
  select(-country,-NUTS)

mean_rev_wide <- mean_rev_long %>%
  select(-obs_value,-shares,-rev,-vintage) %>%
  na.omit() %>%
  unique() %>%
  mutate(mean_rev=round(mean_rev,5)) %>%
  pivot_wider(names_from=time_period,
              values_from=mean_rev) %>%
  arrange(sto,ref_area)%>%
  filter(NUTS=="2")%>%
  select(-country,-NUTS)

mean_weights_wide <- mean_weights_long %>%
  select(-obs_value,-shares,-vintage) %>%
  na.omit() %>%
  unique() %>%
  mutate(mean_weights=round(mean_weights*100,5)) %>%
  filter(NUTS=="2")%>%
  pivot_wider(names_from=time_period,
              values_from=mean_weights)%>%
  select(-country,-NUTS)

weighted_rev_wide <- weighted_rev_long %>%
  filter(NUTS=="2")%>%
  mutate(weighted_rev=round(weighted_rev,5)) %>%
  pivot_wider(names_from=time_period,
              values_from=weighted_rev) %>%
  arrange(sto,ref_area)%>%
  filter(NUTS=="2")%>%
  select(-country,-NUTS)


wb <- loadWorkbook(system.file("extdata", "template_NQR.xlsx", package = "regacc"))
# wb <- loadWorkbook(template)
options(openxlsx.numFmt = "#,##0.00")
writeDataTable(wb, sheet="0_Results", final_wide, startCol = 1, startRow = 3,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="0_Results", first_last, startCol = 1, startRow = 10,tableStyle = "TableStyleLight13")
#print(evol_plot)
# wb %>% insertPlot(sheet="1_evol_plot",startCol =1, startRow=1,width=9,height = 6, dpi=600)
print(reg_plot)
wb %>% insertPlot(sheet="1_Reg_plot",startCol =1, startRow=1,width=width_chart,height = height_chart, dpi=600)
writeDataTable(wb, sheet="2_Source", rev_df_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="3_Shares", shares_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="4_Abs_rev", rev_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="5_Mean_rev", mean_rev_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="6_Mean_shares", mean_weights_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")
writeDataTable(wb, sheet="7_Weighted_rev",weighted_rev_wide, startCol = 1, startRow = 1,tableStyle = "TableStyleLight13")

saveWorkbook(wb, paste0(output_dir,"/",country_sel,"_NQR_revision.xlsx"), overwrite = TRUE)
options(warn = 0)
}


