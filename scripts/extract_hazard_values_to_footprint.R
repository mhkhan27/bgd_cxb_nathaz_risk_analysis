library(raster)
library(sf)
library(tidyverse )
library(exactextractr)



# load natahaz rasters ----------------------------------------------------
# research_phase<-c("pilot","production")[1]
buff_dist<-c(0,1,2)[2]


flood_arup<-raster("../01_floods/03_raster/02_arup/Arup_Pluvial_Flooding_Merged_46N.tif")
flood_deltares<-raster("../01_floods/03_raster/03_deltares/t10_24h_culv_maxwd_5m_46N.tif")

wind_sanders<-raster("../02_cyclones/02_cyclone_hazards/01_wind_modeling/QGIS/reach_mapped_cases/cyclone_max.tif")
# storm_surge_10<-raster("../../02_Themes/07_Natural_Hazards/02_cyclones/02_cyclone_hazards/02_storm_surge_modeling/200403 Stage 5 Issue/C23/10yrpluv_100yrST_d_max.asc")
storm_surge_c23_10<-raster("../02_cyclones/02_cyclone_hazards/02_storm_surge_modeling/200403 Stage 5 Issue/C23/10yrpluv_10yrST_d_max.asc")
storm_surge_southern_10<-raster("../02_cyclones/02_cyclone_hazards/02_storm_surge_modeling/200403 Stage 5 Issue/C24-27/C24_C27_10yrpluv_10yrST_d_Max.asc")
# storm_surge_southern_10<-raster("../02_cyclone_hazards/02_storm_surge_modeling/200403 Stage 5 Issue/C24-27/ ")

landslide_simp<- raster("../NatHaz_GIS_Files_V2_12june2019/01_raster/Landslide_Susceptibility_3classes_june2019.tif")



# Can I get just tidal effect from storm surge ----------------------------

storm_surge_c23_10 %>% hist()

# define raster math function - dont really need to name this function
diff_rasters <- function(b1, b2){
  diff <- b1 - b2
  return(diff)
}

#for raster math you need the same extents
c23_precip<-raster::crop(x = flood_arup,y=storm_surge_c23_10)
extent(c23_precip)
# realized that in the precip raster NA is really 0
c23_precip <- reclassify(c23_precip, cbind(NA, 0))
extent(storm_surge_c23_10)
# c23 subtraction - shamlapur
c23_ss_minus_precip <- overlay(storm_surge_c23_10, c23_precip,
                               fun = diff_rasters)



# repeat two steps -- upon attempting...
# realized that the southern camps have a 2 m resolution that must have been
# resampled to 1 m when i merged the precip flood rasters... therefore load original
c24_27_precip<-raster("../01_floods/03_raster/02_arup/pluvial_flood_batch3_camps_24_27/C24_C27_BURSTS_10yr_d_Max.asc")

# c24_27_precip<-raster::crop(x = flood_arup,y=storm_surge_southern_10)
extent(c24_27_precip)
c24_27_precip <- reclassify(c24_27_precip, cbind(NA, 0))
c24_27_ss <- reclassify(storm_surge_southern_10, cbind(NA, 0))
extent(storm_surge_southern_10)

# c24_27 subtraction
c24_27_ss_minus_precip <- overlay(c24_27_ss, c24_27_precip,
                     fun = diff_rasters)

# c23_ss_minus_precip %>% hist()
# raster::writeRaster(c24_27_ss_minus_precip,"../02_cyclones/02_cyclone_hazards/02_storm_surge_modeling/storm_surge_gis/c24_27_ss10yr_precip10yr_minus_precip10yr_.tif")




fp_gdb<-"../../../01_GIS_BASE_Data/02_landscape/01_infrastructure/01_shelter_footprint/03_unosat_footprints"
fp<-st_read(fp_gdb,"BGD_Camp_ShelterFootprint_UNOSAT_REACH_v1_07may2019")
fp_utm<-fp %>% st_transform(crs=32646)

if(buff_dist>0){
  fp_1m<-fp_utm%>% st_buffer(dist = buff_dist)
  fp_1m_cast<-st_cast(fp_1m,"MULTIPOLYGON")
  fp_ready<-fp_1m_cast} else {
    fp_ready<-fp_utm
  }

width_list<- c(10,20, 30)
fp_ready %>% ggplot(aes(x=area_m2))+geom_histogram() +
  # scale_x_continuous(breaks = c(seq(0,20,1)))
  scale_x_log10(breaks = c(seq(0,5,1),seq(6,10,2),seq(15, 30,5)))
# scale_x_log10(labels=scales::label_number())
#needa do this again
raster::projection(wind_sanders)<- raster::projection(flood_deltares)

#add rasters attributes
# fp_1m_cast$deltares_max[!is.na(fp_1m_cast$deltares_max)] %>% max()
fp_1m_cast$arup_max<- exactextractr::exact_extract(x = flood_arup,fp_1m_cast,"max")
fp_1m_cast$deltares_max<- exactextractr::exact_extract(x = flood_deltares,fp_1m_cast,"max")



fp_1m_cast$storm_surge_c23<- exactextractr::exact_extract(x = storm_surge_c23_10,fp_1m_cast,"max")
fp_1m_cast$storm_surge_southern<- exactextractr::exact_extract(x = storm_surge_southern_10,fp_1m_cast,"max")

fp_1m_cast$only_ss_c23<- exactextractr::exact_extract(x = c23_ss_minus_precip,fp_1m_cast,"max")
fp_1m_cast$only_ss_c24_27<- exactextractr::exact_extract(x = c24_27_ss_minus_precip,fp_1m_cast,"max")


#needa do this again
fp_1m_cast$wind_max<- exactextractr::exact_extract(x = wind_sanders,fp_1m_cast,"max")
fp_1m_cast$landslide<- exactextractr::exact_extract(x = landslide_simp,fp_1m_cast,"max")


fp_1m_pt<-st_centroid(fp_1m_cast)

fp_1m_pt<- fp_1m_pt %>%
  mutate(pluvial_max=ifelse(!is.na(deltares_max),deltares_max,arup_max),
         storm_surge_max=ifelse(!is.na(storm_surge_c23),storm_surge_c23,storm_surge_southern),
         class_code= case_when(
           area_class=="Structure"~"S",
           area_class=="Bridge"~ "B",
           TRUE ~ "L"
         )
  )



fp_1m_pt_simp<-fp_1m_pt %>%
  dplyr::select(id,cmp_name,wind_max, landslide,pluvial_max, storm_surge_max,class_code)
fp_1m_pt_simp %>% nrow()
fp_1m_pt_simp$storm_surge_max %>% range(na.rm=T)

fp_with_area<-fp_1m_pt_simp %>% left_join(fp %>%st_drop_geometry() %>%  dplyr::select(id, area_m2), by="id")

# st_write(fp_2m_pt_simp,"camp_style_atlas/data/shelter_pts_hazard.shp")
fp_1m_pt_simp_w_coords<-fp_1m_pt_simp %>% st_transform(crs=4326) %>%  butteR::st_drop_geometry_keep_coords() %>%
  mutate(wind_max=wind_max*3.6)
colnames(fp_1m_pt_simp_w_coords)[5:8]<-paste0("haz_",colnames(fp_1m_pt_simp_w_coords)[5:8])
fp_1m_pt_simp_long<-fp_1m_pt_simp_w_coords %>% pivot_longer(cols= starts_with ("haz_"), names_to = "hazard_code")
fp_1m_pt_simp_long<-fp_1m_pt_simp_long %>% left_join(fp %>%st_drop_geometry() %>%  dplyr::select(id, area_m2), by="id") %>% distinct()

# saveRDS(fp_1m_pt_simp_long,"inputs/shelters_by_hazard_fixed.rds")
# write.csv(fp_1m_pt_simp_long,"nathaz_dashboard/nathaz_dashboard/data/shelter_1m_buffer_centroids_max_hazard.csv",row.names = F)


fp_1m_pt_simp<-fp_1m_pt %>%
  dplyr::select(id,cmp_name,only_ss_c23, only_ss_c24_27)

fp_1m_pt<- fp_1m_pt %>%
  mutate(
         storm_surge_only_max=ifelse(!is.na(only_ss_c23),only_ss_c23,only_ss_c24_27),
         class_code= case_when(
           area_class=="Structure"~"S",
           area_class=="Bridge"~ "B",
           TRUE ~ "L"
         )
  )

depths<-seq(0,2.5, by=0.05)
storm_surge_threshold_summary<-fp_1m_pt %>%
  filter(!is.na(storm_surge_only_max)) %>%
  summarise(structures_affected=purrr::map(.x=depths,~sum(value>.,na.rm=T)) %>% unlist(),
            depths= depths) %>%
  mutate(stucture_type="All Structures")

storm_surge_shelter_summary<-buff_1m_split$haz_storm_surge_max %>%
  filter(!is.na(value)) %>%
  filter(area_m2>=4, area_m2<=100) %>%
  summarise(structures_affected=purrr::map(.x=depths,~sum(value>.,na.rm=T)) %>% unlist(),
            depths= depths) %>%
  mutate(stucture_type="Shelters")

storm_surge_threshold_summary<- bind_rows(storm_surge_threshold_summary,storm_surge_shelter_summary)

flood_depth_rects <- data.frame(
  xstart = c(0.01,0.5,1,2),
  xend =c(0.5,1,2,2.5),
  col= forcats::as_factor( c("Low Depth","Moderate Depth","High Depth","Very High Depth")),
  ymax= rep(max(storm_surge_threshold_summary$structures_affected),4)
)







buff_1m<-read_rds("inputs/shelters_by_hazard_fixed.rds")
buff_1m




fp_2m<-fp_utm%>% st_buffer(dist = 2)
fp_2m_cast<-st_cast(fp_2m,"MULTIPOLYGON")
fp_2m100<-fp_2m %>% slice(1:100)
fp_2m100<-fp_2m_cast %>% slice(1:100)
raster::projection(flood_deltares)
raster::projection(wind_sanders)<- raster::projection(flood_deltares)
projection(storm_surge_c23_10)

projection(landslide_simp)

fp_2m_cast$deltares_max[!is.na(fp_2m_cast$deltares_max)] %>% max()
fp_2m_cast$arup_max<- exactextractr::exact_extract(x = flood_arup,fp_2m_cast,"max")
fp_2m_cast$deltares_max<- exactextractr::exact_extract(x = flood_deltares,fp_2m_cast,"max")
fp_2m_cast$wind_max<- exactextractr::exact_extract(x = wind_sanders,fp_2m_cast,"max")
fp_2m_cast$storm_surge_c23<- exactextractr::exact_extract(x = storm_surge_c23_10,fp_2m_cast,"max")
fp_2m_cast$storm_surge_southern<- exactextractr::exact_extract(x = storm_surge_southern_10,fp_2m_cast,"max")
fp_2m_cast$landslide<- exactextractr::exact_extract(x = landslide_simp,fp_2m_cast,"max")

# "camp_style_atlas/data/shelter_pts_hazard.csv"

fp_2m_pt<-st_centroid(fp_2m_cast)

fp_2m_pt<- fp_2m_pt %>%
  mutate(pluvial_max=ifelse(!is.na(deltares_max),deltares_max,arup_max),
         storm_surge_max=ifelse(!is.na(storm_surge_c23),storm_surge_c23,storm_surge_southern),
         class_code= case_when(
           area_class=="Structure"~"S",
           area_class=="Bridge"~ "B",
           TRUE ~ "L"
         )
  )

fp_2m_pt_simp<-fp_2m_pt %>%
  select(id,cmp_name,wind_max, landslide,pluvial_max, storm_surge_max,class_code)

fp_2m_pt_simp$storm_surge_max %>% range(na.rm=T)
# st_write(fp_2m_pt_simp,"camp_style_atlas/data/shelter_pts_hazard.shp")
fp_2m_pt_simp_w_coords<-fp_2m_pt_simp %>% st_transform(crs=4326) %>%  butteR::st_drop_geometry_keep_coords() %>%
  mutate(wind_max=wind_max*3.6)
colnames(fp_2m_pt_simp_w_coords)[5:8]<-paste0("haz_",colnames(fp_2m_pt_simp_w_coords)[5:8])
fp_2m_pt_simp_long<-fp_2m_pt_simp_w_coords %>% pivot_longer(cols= starts_with ("haz_"), names_to = "hazard_code")

# write.csv(fp_2m_pt_simp_long,"camp_style_atlas/data/shelter_pts_hazard2.csv",row.names = F)

fp_2m_pt$area_class %>% table()

# play with point denisty
library(KernSmooth)
library(raster)

wind_100_pts<-fp_2m_pt_simp_long %>%
  filter(hazard_code=="haz_wind_max", value >=60)


# create (empty) rasters
cell_size <- 0.0001
lon_min <- min(wind_100_pts$X); lon_max <- max(wind_100_pts$X); lat_min <- min(wind_100_pts$Y); lat_max <- max(wind_100_pts$Y)
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1
us_fire_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=0.0001, crs="+proj=longlat +datum=WGS84")
us_fire_counts

# rasterize
us_fire_counts <- rasterize(wind_100_pts %>% select(X,Y) %>% as.matrix(), us_fire_counts, fun="count")
us_fire_counts
plot(log10(us_fire_counts), col=  RColorBrewer::brewer.pal(9,"BuPu"), sub="log10 Number of Fires")
plot(us_fire_counts, col=  RColorBrewer::brewer.pal(9,"BuPu"), sub="Number> 100 km/hr")



bilinear_data<-resample(us_fire_counts,us_fire_counts, method="bilinear")
bilinear_data<-resample(us_fire_counts,method="bilinear")
?resample
?focal
us_fire_counts_fm <- focal(us_fire_counts, w= matrix(1,3,3), mean, na.rm=T)
us_fire_counts_fm <-focal(us_fire_counts, w=matrix(1/9,nrow=3,ncol=3),na.rm=T)
us_fire_counts_fm <- focalWeight(us_fire_counts, 2, "Gauss")
disagg <- disaggregate(us_fire_counts, 3, method='bilinear')
disagg2 <- focal(disagg, w= matrix(1,1.5,1.5), mean, na.rm=T)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(us_fire_counts),
                    na.color = "transparent")
pal_disagg <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(disagg2),
                           na.color = "transparent")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), log10(values(us_fire_counts_fm)),
                    na.color = "transparent")


leaflet() %>% addTiles() %>%
  addRasterImage(us_fire_counts, colors = pal, opacity = 0.8,method =  "bilinear") %>%
  # addRasterImage(us_fire_counts)
  addLegend(pal = pal, values = values(us_fire_counts),
            title = "Surface temp")
leaflet() %>% addTiles() %>%
  addRasterImage(us_fire_counts_fm, colors = pal, opacity = 0.8,method =  "bilinear") %>%
  # addRasterImage(us_fire_counts)
  addLegend(pal = pal, values = values(us_fire_counts_fm),
            title = "Surface temp")

leaflet() %>% addTiles() %>%
  addRasterImage(dissag2, colors = pal_disagg, opacity = 0.8,method =  "bilinear") %>%
  # addRasterImage(us_fire_counts)
  addLegend(pal = pal_disag, values = values(dissag2),
            title = "Surface temp")

# Load a starbucks.shp point feature shapefile
wind_sf<-st_as_sf(wind_100_pts, coords = c("X","Y"), crs=4326)
s  <- as_Spatial(wind_sf)
starbucks    <- as(s, "ppp")

# Load a pop_sqmile.img population density raster layer
img  <- raster("pop_sqmile.img")
pop  <- as.im(img)







est<-bkde2D(x = wind_100_pts %>% select(X,Y) %>% as.matrix(),bandwidth = c(3,3),
            gridsize=c(4320,2160),
            range.x=list(c(-180,180),c(-90,90)))

min(wind_100_pts$Y)
est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster) <- CRS("+init=epsg:4326")
xmin(est.raster) <- 92.146
xmax(est.raster) <- 92.156
ymin(est.raster) <- 21.198
ymax(est.raster) <- 21.205
# visually inspect the raster output
plot(est.raster)
# write an ascii file
# writeRaster(est.raster,output,"ascii")





fp_2m100$max=exactextractr::exact_extract(x = flood_arup,fp_2m100,"max")

fp
fp_temp
?st_buffer

fp_2m$area2<-st_area(fp_2m)
fp_2m %>% select(area_m2,area2)
test2<- fp %>% slice(1:100)
test<-fp_2m %>% slice(1:100)

plot(test)
plot(test2)
test==test2
test$geometry
test2$geometry
colnames(test)
colnames(fp_temp)
test %>% st_geometry_type() %>% levels()
which(test %>% st_geometry_type() %>% is.null() )
fp_temp %>% st_geometry_type() %>% levels()


fp_temp$max=exactextractr::exact_extract(x = flood_arup,test2,"max")
st_area(test)
test %>% mutate(area2=sf::st_area())

fp_2m$max_narm=exactextractr::exact_extract(x = flood_arup,fp_2m, "max")



fp<-st_read("data","footprint_stormsurge_example")
fp_topojson<-geojsonio::topojson_json(fp)
raster::projection(plv_fl_46N)
?sf::sf_project
?sf::sf_add_proj_units(fp, crs=32636)
# install.packages("tabularaster")
library(tabularaster)
?cellnumbers
plv<-raster("../../02_Themes/07_Natural_Hazards/01_floods/03_raster/02_arup/Arup_Pluvial_Flooding_Merged_46N.tif")
system.time(raster("../../02_Themes/07_Natural_Hazards/01_floods/03_raster/02_arup/Arup_Pluvial_Flooding_Merged_46N.tif"))
plv_cell_num<-system.time(tabularaster::cellnumbers(plv,fp))
system.time(tabularaster::cellnumbers(plv,fp) )

fp %>% st_crs
fp_temp<-fp %>% slice(1:1000)
fp_temp$max_narm=exactextractr::exact_extract(x = plv,fp_temp, function(values, coverage_fraction) max(values,na.rm=T))
fp_temp$max=exactextractr::exact_extract(x = plv,fp_temp,"max")
fp_temp<-fp_temp %>% mutate(asdf=ifelse(max_narm==-Inf,NA,max_narm))
all(fp_temp$max==fp_temp$asdf,na.rm=T)
?extract
raster::extract(x = plv, y=fp[1:100,], df= T, small=T, method=mean)


fp_temp<-fp[100:200,]
cell_num_temp<-cellnumbers(plv,fp_temp)
raster::extract(plv,cell_num_temp$cell_)

#to slow
# plv_ext<-raster::extract(plv,plv_cell_num$cell_)



plv_cell_num$object_ %>% unique() %>% length()
system.time(for(i in 1:100){
  print(i)
  shelt_temp<-unique(plv_cell_num$object_) [i]
  print("adf")
  cell_lookup<-plv_cell_num[plv_cell_num$object_==shelt_temp,]
  print("aaaaaaaa")
  ex_r<-raster::extract(plv, cell_lookup$cell_)
}
)
#43.59 seconds
#8 hours like this!
(((68771/100)*43.59)/60)/60


unique(plv_cell_num$object_ ) %>% length()
asdf<-unique(plv_cell_num$object_ )

plv_cell_num2<-plv_cell_num %>% rename(aaa="object_")
plv_cell_num %>% colnames()
t100<-plv_cell_num2 %>% filter(aaa %in% asdf)

system.time(ex_r<-raster::extract(plv, t100$cell_))
t100 %>%
  mutate(v=raster::extract(plv, cell_))
