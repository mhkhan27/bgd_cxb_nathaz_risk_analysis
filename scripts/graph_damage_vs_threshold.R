library(tidyverse)
library(sf)

unhcr_pop<-readxl::read_xlsx("inputs/UNHCR Population Factsheet - Block Level data_20200831_v1.xlsx","Final",skip = 2)
camp_boundary<- st_read("inputs", "Camp_boundary")

unhcr_pop<-unhcr_pop %>% janitor::clean_names()

unhcr_pop<-unhcr_pop %>%
  janitor::clean_names() %>%
  filter(!is.na(camp)& is.na(block)) %>%
  mutate(
    camp=str_replace(camp, "Total","") %>% trimws(),
    total_families=as.numeric(total_families)
  ) %>%
  filter(camp!="Grand") %>%
    select(camp, everything(), -starts_with("block")) %>% print(n=nrow(.))



# all hazards
# buff_1m<-data.table::fread("../../ShinyDashboardTemplates/nathaz_dashboard/nathaz_dashboard/data/shelter_1m_buffer_centroids_max_hazard.csv")
buff_1m<-read_rds("inputs/shelters_by_hazard.rds")
buff_1m<-buff_1m%>% mutate(camp_number=parse_number(cmp_name),
                           region= case_when(camp_number %in% c(21:27)~ "Teknaf",
                                             cmp_name== "Nayapara RC"~"Teknaf",
                                             TRUE~ "Ukhiya")
)




buff_1m<-buff_1m%>% mutate(camp_number=parse_number(cmp_name),
                           region= case_when(camp_number %in% c(21:27)~ "Teknaf",
                                             cmp_name== "Nayapara RC"~"Teknaf",
                                             TRUE~ "Ukhiya")
)

# remove latrines
buff_1m<-buff_1m %>%
  filter(area_m2>3.25)



buff_1m_split<-buff_1m %>%
  split(.$hazard_code)

buff_1m_split$haz_storm_surge_max %>% nrow()



depths<-seq(0,2, by=0.05)

storm_surge_threshold_summary<-buff_1m_split$haz_storm_surge_max %>%
  filter(!is.na(value)) %>%
  summarise(shelters_affected=purrr::map(.x=depths,~sum(value>.,na.rm=T)) %>% unlist(),
            depths= depths)

flood_depth_rects <- data.frame(
  xstart = c(0.01,0.5,1),
  xend =c(0.5,1,2),
  col= forcats::as_factor( c("Low Depth","Moderate Depth","High Depth")),
  ymax= rep(max(storm_surge_threshold_summary$shelters_affected),3)
)


ggplot()+
  geom_path(data=storm_surge_threshold_summary, aes(x=depths,y=shelters_affected),color="red", size=2)+
  geom_rect(data=flood_depth_rects,
             mapping=aes(ymin=0,
                         ymax=max(storm_surge_threshold_summary$shelters_affected),
                         xmin=xstart,
                         xmax=xend, fill=col), alpha =0.5)+
  scale_fill_brewer()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0), labels=scales::comma)+
  annotate(
    geom = "curve", x = 0.25, y = 7500 ,
    xend = 0.5, yend = 10597,
    curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "black", size=1) +
  annotate(geom = "text", x = 0.25, y = 7400,
           label = "10,600", hjust = "center")+
  labs(x="water depth (m)", y="# of structures")+
  annotate(
    geom = "curve", x = 1.25, y = 7500 ,
    xend = 1, yend = 5046,
    curvature = -.3, arrow = arrow(length = unit(2, "mm")), color = "black", size=1) +
  annotate(geom = "text", x = 1.25, y = 7700,
           label = "~ 5,000", hjust = "center")+
  annotate(geom = "text", x = 0.35, y = 16000,
           label = "Low \n Depth", hjust = "center")+
  annotate(geom = "text", x = 0.75, y = 15500,
           label = "Moderate Depth", hjust = "center")+
  annotate(geom = "text", x = 1.5, y = 15500,
           label = "High Depth", hjust = "center")+
  labs(x="water depth (m)", y="# of structures")+
  ggtitle("10 Year Average Return Interval Storm Surge",subtitle = "Camps 21 - 27 considered")+
  theme_bw()+
  theme(axis.text.y = element_text(angle = 90),
        legend.position = "none"
        )


storm_surge_threshold_summary_by_camp<-buff_1m_split$haz_storm_surge_max %>%
  filter(!is.na(value)) %>%
  group_by(cmp_name) %>%
  summarise(shelters_affected=purrr::map(.x=depths,~sum(value>.,na.rm=T)) %>% unlist(),
            depths= depths,
            percent_affected=shelters_affected/nrow(buff_1m_split$haz_storm_surge_max))


# interesting graph
storm_surge_threshold_summary_by_camp %>%
  filter(depths==0.5) %>%
  ggplot(aes(x=reorder(cmp_name,-shelters_affected),y=shelters_affected))+geom_bar(stat="identity")





buff_1m_split$haz_pluvial_max %>%
  filter(!is.na(value)) %>%
  summarise(shelters_affected=purrr::map(.x=depths,~sum(value>.,na.rm=T)) %>% unlist(),
            depths= depths) %>%
  ggplot(aes(x=depths, y=shelters_affected))+geom_point() +
  scale_y_continuous(labels=scales::comma)+
  labs(x="water depth (m)", y="# of structures")+
  ggtitle("10 Year Average Return Interval Pluvial Flooding Event")+
  theme_bw()+
  theme(axis.text.y = element_text(angle = 90))



# ONLY HAVE MODEL FOR KUTUPALONG
wind_speeds<-seq(30, 150, by=10)
buff_1m_split$haz_wind_max$cmp_name %>% unique()

buff_1m_split$haz_wind_max %>%
  filter(!is.na(value)) %>%
  summarise(shelters_affected=purrr::map(.x=wind_speeds,~sum(value>.,na.rm=T)) %>% unlist(),
            wind_speeds= wind_speeds) %>%
  ggplot(aes(x=wind_speeds, y=shelters_affected))+geom_point()+
  scale_y_continuous(labels=scales::comma)+
  labs(x="Wind Speed (km/hr)", y="# of structures")+
  ggtitle("Category 4 Cyclone Wind Speeds")+
  theme_bw()+
  theme(axis.text.y = element_text(angle = 90))



total_structures_ukhiya<-buff_1m_split$haz_wind_max %>% filter(region=="Ukhiya") %>% nrow()
buff_1m_split$haz_wind_max %>%
  summarise(shelters_affected=purrr::map(.x=wind_speeds,~sum(value>.,na.rm=T)/total_structures_ukhiya) %>%
              unlist(),
            wind_speeds= wind_speeds) %>%
  ggplot(aes(x=wind_speeds, y=shelters_affected))+geom_point()+
  scale_y_continuous(labels=scales::percent)+
  labs(x="Wind Speed (km/hr)", y="# of structures")+
  ggtitle("Category 4 Cyclone Wind Speeds")+
  theme_bw()+
  theme(axis.text.y = element_text(angle = 90))


