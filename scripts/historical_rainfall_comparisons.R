library(tidyverse)
library(lubridate)
library(tidyquant)

chirps_merged<-map_dfr(list.files("inputs/precip_data/historical/",full.names = T), read_csv)

chirps1<-chirps_merged %>%
  mutate(date=mdy(`system:time_start`),
         year= year(date))


maximum_single_day_precip<- chirps1 %>%
  group_by(year) %>%
  filter(precipitation==max(precipitation))


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


precip_1_day_max<- chirps1 %>%
  # group_by(year) %>%
  tq_mutate(
    # tq_mutate args
    select     = precipitation,
    mutate_fun = rollapply,
    # rollapply args
    width      = 1,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_1_day"
  )
precip_2_day_max<- chirps1 %>%
  # group_by(year) %>%
  tq_mutate(
    # tq_mutate args
    select     = precipitation,
    mutate_fun = rollapply,
    # rollapply args
    width      = 2,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_2_day"
  )

precip_3_day_max<- chirps1 %>%
  # group_by(Device.name) %>%
  tq_mutate(
    # tq_mutate args
    select     = precipitation,
    mutate_fun = rollapply,
    # rollapply args
    width      = 3,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_3_day"
  )


precip_5_day_max<- chirps1 %>%
  # group_by(Device.name) %>%
  tq_mutate(
    # tq_mutate args
    select     = precipitation,
    mutate_fun = rollapply,
    # rollapply args
    width      = 5,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_5_day"
  )



all_roll_max<-bind_rows(
  roll_sum_to_max(df = precip_1_day_max,grouper = "year",time_interval = "1_day"),
roll_sum_to_max(df = precip_2_day_max,grouper = "year",time_interval = "2_day"),
roll_sum_to_max(df = precip_3_day_max,grouper = "year",time_interval = "3_day"),
roll_sum_to_max(df = precip_5_day_max,grouper = "year",time_interval = "5_day")
)

all_roll_max<- all_roll_max %>% mutate(label=ifelse(interval=="1_day",paste0(start_month, " ", start_day), label))


# 2 days tied
all_roll_max %>%
  group_by(year, interval) %>%
  mutate(n=n()) %>%
  filter(n>1)
  slice(1)

  #filter duplicates
  all_roll_max<- all_roll_max %>%
    group_by(year, interval) %>%
    slice(1)


all_roll_max %>% filter(year==1982) %>% print(n=nrow(.))
time_period_labs <- data.frame(
  xstart = c(1981,2017),
  xend =c(2017,2020),
  col= forcats::as_factor( c("Pre-Influx","Post-Influx"))
)

windows();all_roll_max %>%
  # filter(year==1982) %>%
  ggplot(aes(x=year,y=sum_val, fill=as.character(year)))+
  geom_bar(stat = "identity")+
  # geom_rect(data=time_period_labs,
  #           mapping=aes(ymin=0,
  #                       ymax=950,
  #                       xmin=xstart,
  #                       xmax=xend, fill=col), alpha =0.5)+
  scale_x_continuous(breaks=seq(1981,2020,1))+
  scale_y_continuous(limits = c(0,950))+
  geom_text(aes(x=year, y=0, label=label, angle=90, hjust="left"), size=3)+
  geom_text(aes(x=year,
                y=sum_val+10,
                label=paste0(round(sum_val,0)," mm"),
                angle=90, hjust="left"), size=3)+
  annotate(
    geom = "curve", x = 2017, y = 800 ,
    xend = 2015, yend = all_roll_max %>% filter(year==2015) %>% pull(sum_val) ,
    curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "black", size=1) +
  # structures at 0.5m
  annotate(geom = "text", x = 2017, y = 700,
           label = " ~ koomen", hjust = "center")+

  labs(y= "precip (mm)")+
  facet_wrap(~interval)+

  theme_bw()+
  theme(
    axis.text.x = element_text(angle=90),
    legend.position= "none"
  )


# try a stacked bar
windows();all_roll_max %>%
  # filter(year==1982) %>%
  ggplot(aes(x=year,y=sum_val, fill=as.character(interval), group=factor(interval)))+
  geom_bar(stat="identity",position  = "stack")+
  # geom_rect(data=time_period_labs,
  #           mapping=aes(ymin=0,
  #                       ymax=950,
  #                       xmin=xstart,
  #                       xmax=xend, fill=col), alpha =0.5)+
  scale_x_continuous(breaks=seq(1981,2020,1))+
  scale_y_continuous(limits = c(0,950))+
  geom_text(aes(x=year, y=0, label=label, angle=90, hjust="left"), size=3)+
  geom_text(aes(x=year,
                y=sum_val+10,
                label=paste0(round(sum_val,0)," mm"),
                angle=90, hjust="left"), size=3)+
  annotate(
    geom = "curve", x = 2017, y = 800 ,
    xend = 2015, yend = all_roll_max %>% filter(year==2015) %>% pull(sum_val) ,
    curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "black", size=1) +
  # structures at 0.5m
  annotate(geom = "text", x = 2017, y = 700,
           label = " ~ koomen", hjust = "center")+

  labs(y= "precip (mm)")+
  # facet_wrap(~interval)+

  theme_bw()+
  theme(
    axis.text.x = element_text(angle=90),
    # legend.position= "none"
  )

# precip_5_day_max %>% select(date,everything()) %>%
#   group_by(year) %>%
#   filter(max_5_day==max(max_5_day)) %>%
#   mutate(start_date=date-4)
