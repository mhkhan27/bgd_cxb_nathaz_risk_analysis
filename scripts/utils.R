round_any = function(x, accuracy=100, f=ceiling){f(x/ accuracy) * accuracy}

roll_sum_to_max<- function(df, grouper, time_interval){
  tq_col<-paste0("sum_", time_interval)
  df %>%
    group_by(!!sym(grouper)) %>%
    filter(!!sym(tq_col)==max(!!sym(tq_col),na.rm = T)) %>%
    mutate(start_date=date-(parse_number(time_interval)-1),
           start_month=month(start_date,abbr=T, label=T),
           start_day=day(start_date),
           end_day= day(date),
           interval= time_interval,
           label=paste0(start_month," ", start_day,"-",end_day)
    ) %>%
    rename(sum_val=tq_col)

}
