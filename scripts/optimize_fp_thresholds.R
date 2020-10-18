


# read in and clean up data -----------------------------------------------

# load foot print

fp<-st_read(dsn = "../../../01_GIS_BASE_Data/02_landscape/01_infrastructure/01_shelter_footprint/03_unosat_footprints",
            "BGD_Camp_ShelterFootprint_UNOSAT_REACH_v1_07may2019")

unhcr_pop<-readxl::read_xlsx("inputs/UNHCR Population Factsheet - Block Level data_20200831_v1.xlsx","Final",skip = 2)
camp_boundary<- st_read("inputs", "Camp_boundary")
a2<- st_read("../../../01_GIS_BASE_Data/01_boundaries/03_camp/200908_RRC_Outline_Block_AL2","200908_RRC_Outline_Block_AL2", stringsAsFactors=F)




# fp<- st_read("../../../01_GIS_BASE_Data/02_landscape/01_infrastructure/01_shelter_footprint/03_unosat_footprints","BGD_Camp_ShelterFootprint_UNOSAT_REACH_v1_07may2019", stringsAsFactors=F)


unhcr_pop<-unhcr_pop %>% janitor::clean_names()

unhcr_pop_a1<-unhcr_pop %>%
  # janitor::clean_names() %>%
  filter(!is.na(camp)& is.na(block)) %>%
  mutate(
    camp=str_replace(camp, "Total","") %>% trimws(),
    total_families=as.numeric(total_families)
  ) %>%
  filter(camp!="Grand") %>%
  select(camp, everything(), -starts_with("block")) %>% print(n=nrow(.))


unhcr_pop_a2<-unhcr_pop %>%
  # janitor::clean_names() %>%
  filter(!is.na(block_ssid)) %>%#nrow()
  # mutate(
    # camp=str_replace(camp, "Total","") %>% trimws(),
    # total_families=as.numeric(total_families)
  # ) %>%
  # filter(camp!="Grand") %>%
  select(camp,block_ssid, everything()) #%>% print(n=nrow(.))
a2 %>% View()
a2 %>% nrow()

a1$camp[!a1$camp %in% fp$cmp_name]
a2$camp[!a2$camp %in% fp$cmp_name]
a1<-a1 %>%
  mutate(
    camp=str_replace_all(string = camp,c("Camp 20 Ext"="Camp 20 Extension","Camp 4 Ext" ="Camp 4 Extension"))
  )


# this st_make_valid is a bit heavy.
fp %>% nrow()
fp_valid<- st_make_valid(fp)
a2_valid<- st_make_valid(a2)
a2_valid %>% nrow()
fp_valid %>% nrow()

fp_centroid= st_centroid(fp_valid)
fp_centroid %>% nrow()

fp2<-fp_centroid %>% st_join(a2_valid %>% select(npm_cname,block_ssid))
fp2 %>% nrow()
fp_summary_a2<-fp2 %>%
  st_drop_geometry() %>%
  group_by(block_ssid) %>%
  summarise(
    num_facilities_a2=n(),
    area_facilities_a2=sum(area_m2)
  ) %>% left_join(unhcr_pop_a2)

fp_summary_a2 %>% ggplot(aes(x=num_facilities_a2, y=total_families))+geom_point()

fp_summary_a2 %>%
  ggplot(aes(x=area_facilities_a2, y=total_individuals))+geom_point()

# loop through thresholds to determine optimal ----------------------------

unhcr_pop_trim <- unhcr_pop_a1 %>%  select(camp,total_families)
fp_df<- fp %>% st_drop_geometry()
df_temp_j_list<-list()
df_temp_i_list<-list()
for (i in 3:30){
  print(i)
  fp_temp<-fp_df %>%
    filter(area_m2> i)
  for(j in 200:31){
    fp_temp2<-fp_temp %>%
      filter(area_m2<j)
    fp_summary_temp<- fp_temp2 %>%
      group_by(cmp_name) %>%
      summarize(fp_area= sum(area_m2, na.rm=T))
    fp_summary_temp_joined<-fp_summary_temp %>% left_join(unhcr_pop_trim,by=c("cmp_name"="camp"))
    mres_temp<-lm(total_families~fp_area,data = fp_summary_temp_joined) %>% summary()
    ptemp<-mres_temp$coefficients[2,4]
    df_temp<-data.frame(lower_thresh=i,upper_thresh=j,adj_r2=mres_temp$adj.r.squared,pval=ptemp)
    df_temp_j_list[[j]]<-df_temp
  }
 df_temp_j<-bind_rows(df_temp_j_list)
 df_temp_i_list[[i]]<-df_temp_j
}

# fp_df %>% saveRDS(file = "inputs/fp_df.rds")


# initial results ---------------------------------------------------------
# since our approach is area based... changing the lower thresholds has minimal impact

all_mods<-bind_rows(df_temp_i_list)
# write_csv(all_mods,paste0("inputs/",butteR::date_file_prefix(),"_shelter_threshold_optimization_results.csv"))
all_mods %>% arrange(desc(adj_r2)) %>% slice(1:5)

plot(all_mods$upper_thresh,all_mods$adj_r2)
plot(all_mods$lower_thresh,all_mods$adj_r2)


unhcr_pop %>% arrange(desc(total_families)) %>% slice(1:5) %>% data.frame()
fp_df_optimal_filter<-fp_df %>%
  filter(area_m2>4,area_m2<112) %>%
  group_by(cmp_name) %>%
  summarise(fp_area=sum(area_m2)) %>%
  left_join(unhcr_pop_trim,by=c("cmp_name"="camp"))

#HERE IS APPARENTLY THE BEST FIT
fp_df_optimal_filter %>%
  ggplot(aes(x=fp_area, y= total_families))+geom_point()#+
  ggrepel::geom_label_repel(aes(label=cmp_name))

#SINCE THE APPROACH IS AERIAL BASED THE LOWER THESHOLD DOES NOT MATTER AS MUCH

# should aim for 1 family per hh
RMSE = function(m, o){
    sqrt(mean((m - o)^2))
  }

fp_upper_optimal<-fp_df %>%
    filter(area_m2<112) %>%
    group_by(cmp_name) %>%
    summarise(fp_area=sum(area_m2),
              fp_num=sum(n())) %>%
    ungroup() %>%
    left_join(unhcr_pop_trim,by=c("cmp_name"="camp")) %>%
  mutate(fam_per_struc=total_families/fp_num)

RMSE(rep(1,nrow(fp_upper_optimal)),fp_upper_optimal$fam_per_struc)

df_lower_temp_list<-list()
for(i in seq(0,20,0.5)){
  print(i)
  i_char<-as.character(i)
  fp_lower_thresholded<-fp_df %>%
  filter(area_m2<112) %>%
  filter(area_m2>i) %>%
  group_by(cmp_name) %>%
  summarise(fp_area=sum(area_m2),
            fp_num=sum(n())) %>%
  ungroup() %>%
  left_join(unhcr_pop_trim,by=c("cmp_name"="camp")) %>%
  mutate(fam_per_struc=total_families/fp_num)

rmse_val<-RMSE(rep(1,nrow(fp_lower_thresholded)),fp_lower_thresholded$fam_per_struc)
df_lower_temp<-data.frame(lower_thresh=i, rmse=rmse_val)
df_lower_temp_list[[i_char]]<-df_lower_temp
}

rmse_val<-RMSE(rep(1,nrow(fp_lower_thresholded)),fp_lower_thresholded$fam_per_struc)


df_lower_res<-bind_rows(df_lower_temp_list)




df_temp_j_list<-list()
df_temp_i_list<-list()
for (i in 150:80){
  print(i)
  fp_temp<-fp_df %>%
    filter(area_m2< i)
  for(j in 2:30){
    fp_temp2<-fp_temp %>%
      filter(area_m2>j)
    fp_summary_temp<- fp_temp2 %>%
      group_by(cmp_name) %>%
      summarize(fp_area= sum(area_m2, na.rm=T))
    fp_summary_temp_joined<-fp_summary_temp %>% left_join(unhcr_pop_trim,by=c("cmp_name"="camp"))
    mres_temp<-lm(total_families~fp_area,data = fp_summary_temp_joined) %>% summary()
    ptemp<-mres$coefficients[2,4]
    df_temp<-data.frame(upper_thresh=i,lower_thresh=j,adj_r2=mres_temp$adj.r.squared,pval=ptemp)
    df_temp_j_list[[j]]<-df_temp
  }
  df_temp_j<-bind_rows(df_temp_j_list)
  df_temp_i_list[[i]]<-df_temp_j
}

all_mods_rev<-bind_rows(df_temp_i_list)
all_mods_rev %>% arrange(desc(adj_r2)) %>% slice(1:5)

plot(all_mods_rev$upper_thresh,all_mods_rev$adj_r2)
plot(all_mods_rev$lower_thresh,all_mods_rev$adj_r2)
# plot(all_mods$upper_thresh,all_mods$adj_r2)




# what about doing the relatonship by # of structures ---------------------


unhcr_pop_trim <- unhcr_pop_a1 %>%  select(camp,total_families)
fp_df<- fp %>% st_drop_geometry()
df_temp_j_list<-list()
df_temp_i_list<-list()
for (i in 1:30){
  print(i)
  fp_temp<-fp_df %>%
    filter(area_m2> i)
  for(j in 200:31){
    fp_temp2<-fp_temp %>%
      filter(area_m2<j)
    fp_summary_temp<- fp_temp2 %>%
      group_by(cmp_name) %>%
      summarize(fp_num= n())
    fp_summary_temp_joined<-fp_summary_temp %>% left_join(unhcr_pop_trim,by=c("cmp_name"="camp"))
    mres_temp<-lm(total_families~fp_num,data = fp_summary_temp_joined) %>% summary()
    ptemp<-mres_temp$coefficients[2,4]
    df_temp<-data.frame(lower_thresh=i,upper_thresh=j,adj_r2=mres_temp$adj.r.squared,pval=ptemp)
    df_temp_j_list[[j]]<-df_temp
  }
  df_temp_j<-bind_rows(df_temp_j_list)
  df_temp_i_list[[i]]<-df_temp_j
}


all_mods2<-bind_rows(df_temp_i_list)
# write_csv(all_mods,paste0("inputs/",butteR::date_file_prefix(),"_shelter_threshold_optimization_results.csv"))
# all_mods2 %>% arrange(desc(adj_r2)) %>% slice(1:5)
# all_mods %>% arrange(desc(adj_r2)) %>% slice(1:5)





