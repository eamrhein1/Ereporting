# create an image with our target NOAA codes for outreach

# HarvestArea	LOCATION	Bushels
dat = as.data.frame(rbind(c("092", "292",	"TANGIER SOUND -N. OF WENONA",	82203.75),
                          c("037", "537",	"CHOPTANK RIVER TRIB. -BROAD CREEK",	39227.65),
                          c("043", "043", "FISHING BAY", 22869.25),
                          c("068", "168",	"PATUXENT RIVER -BELOW BENEDICT",	21923),
                          c("037", "137",	"CHOPTANK RIVER -BELOW RT. 50",	13235),
                          c("029", "229",	"CHESAPEAKE BAY -S. OF COVE PT. & W. OF SHIP CHANNEL",	11754),
                          c("078", "078",	"ST. MARY'S RIVER",	11020),
                          c("053", "053",	"LITTLE CHOPTANK RIVER",	10062.5),
                          c("039", "039",	"EASTERN BAY",	9552.75),
                          c("062", "062",	"NANTICOKE RIVER -GENERAL",	8355),
                          c("092", "192",	"TANGIER SOUND -S. OF WENONA",	7263.5),
                          c("037", "437",	"CHOPTANK RIVER TRIB. -HARRIS CREEK",	6542)))
names(dat) = c("id","noaa_code","code_name","count")
# 037 = 137, 537, 437
# 068 = 168
# 092 = 292, 192
# 029 = 229



# load code shapefiles
#source("~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Basemaps, NOAA Charts/NOAAcodes/importNOAAcodes.R")
# import data
dir.shp = "~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Basemaps, NOAA Charts/NOAAcodes/"

# load shapefile
#NOAAshp = shapefile(paste(dir.shp,"NOAA_Codes_Full_Bay_utm2", sep=""))
NOAAshp = shapefile(paste(dir.shp,"noaaoys_Project", sep=""))
NOAAshp = spTransform(NOAAshp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# make a dataframe
shp_df = broom::tidy(NOAAshp, region = "NOAACODE") 

# join
oys_shp = left_join(shp_df, dat, by = "id") %>% 
  mutate(target_code = ifelse(!is.na(count),"yes","no"))

code_names = as.data.frame(cbind(unique(oys_shp$id[oys_shp$target_code %in% "yes"]),
                   c(mean(oys_shp$lat[oys_shp$id %in% "029"]),
                     mean(oys_shp$lat[oys_shp$id %in% "037"]),
                     mean(oys_shp$lat[oys_shp$id %in% "039"]),
                     mean(oys_shp$lat[oys_shp$id %in% "043"]),
                     mean(oys_shp$lat[oys_shp$id %in% "053"]),
                     mean(oys_shp$lat[oys_shp$id %in% "062"]),
                     mean(oys_shp$lat[oys_shp$id %in% "068"]),
                     mean(oys_shp$lat[oys_shp$id %in% "078"]),
                     mean(oys_shp$lat[oys_shp$id %in% "092"])),
                   c(mean(oys_shp$long[oys_shp$id %in% "029"]),
                     mean(oys_shp$long[oys_shp$id %in% "037"]),
                     mean(oys_shp$long[oys_shp$id %in% "039"]),
                     mean(oys_shp$long[oys_shp$id %in% "043"]),
                     mean(oys_shp$long[oys_shp$id %in% "053"]),
                     mean(oys_shp$long[oys_shp$id %in% "062"]),
                     mean(oys_shp$long[oys_shp$id %in% "068"]),
                     mean(oys_shp$long[oys_shp$id %in% "078"]),
                     mean(oys_shp$long[oys_shp$id %in% "092"]))))
names(code_names) = c("id", "lat", "long")

p = ggplot() + geom_polygon(data = oys_shp, aes(x = long, y = lat, group = group, fill = target_code), col = "black") + 
  theme_bw() + labs(x="Longitude", y="Latitude") + 
  theme(text = element_text(size=20)) + 
  scale_fill_manual(values=c("lightgrey","gold")) 

p + geom_label(data = code_names, aes(x=long, y=lat, label = id), size = 8) +
  labs(y="Latitude",x="Longitude")+
  theme(text = element_text(size = 20),
        legend.position = "none")
