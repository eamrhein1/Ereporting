#--------------------- #
# load packages
#--------------------- #
require(dplyr)
require(ggplot2)
library(ggmap)
library(rgeos)
#--------------------- #

#--------------------- #
# data
#--------------------- #
# load regions
source("U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/code/importRegions.R")

# load split
r1split_zips = read.csv("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/R1split/new_regions_r1split_3and4combo.csv")

# change polygon to df with zips
r1_df = broom::tidy(r1, region = "Tidal_Co_1") 
r2_df = broom::tidy(r2, region = "Tidal_Co_1") 
r3_df = broom::tidy(r3, region = "Tidal_Co_1") 
r4_df = broom::tidy(r4, region = "Tidal_Co_1") 
r5_df = broom::tidy(r5, region = "Tidal_Co_1") 
r6_df = broom::tidy(r6, region = "Tidal_Co_1") 

# join with shapefiles
r1_df = r1_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id")
r2_df = r2_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id") 
r3_df = r3_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id")
r4_df = r4_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id")
r5_df = r5_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id")
r6_df = r6_df %>% left_join(., rename(r1split_zips, id = zipcode) %>% mutate(id = as.character(id)), by = "id")

# calc centroids for labels
trueCentroids = as.data.frame(cbind(c("1a","1b","2","3 and 4","5","6"), 
                                    rbind(mean(r1_df$lat[r1_df$region %in% "1a"]),
                                          mean(r1_df$lat[r1_df$region %in% "1b"]),
                                          mean(r2_df$lat),
                                          mean(c(mean(r3_df$lat),mean(r4_df$lat))),
                                          mean(r5_df$lat),
                                          mean(r6_df$lat)),
                                    rbind(mean(r1_df$long[r1_df$region %in% "1a"]),
                                          mean(r1_df$long[r1_df$region %in% "1b"]),
                                          mean(r2_df$long),
                                          mean(c(mean(r3_df$long),mean(r4_df$long))),
                                          mean(r5_df$long),
                                          mean(r6_df$long))))
names(trueCentroids) = c("region","lat","long") 
trueCentroids = mutate(trueCentroids, region = paste("Region ", region, sep = ""),
                       lat = as.numeric(as.character(lat)),
                       long = as.numeric(as.character(long)))

# load google map
#register_google(key = "") #use once and only for Kaycee
MD = get_map(location = c(lon = -76, lat = 38.5), zoom = 8, maptype = "satellite") #hybrid #watercolor #terrain
#--------------------- #

#--------------------- #
# plot data
#--------------------- #
p = ggmap(MD)
p2 = p + 
  geom_polygon(data = r1_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  geom_polygon(data = r2_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  geom_polygon(data = r3_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  geom_polygon(data = r4_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  geom_polygon(data = r5_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  geom_polygon(data = r6_df, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  scale_fill_manual(values=c("lightgrey","#999999", "gold","#E69F00", "#56B4E9","cornflowerblue"))+
  geom_label(data = trueCentroids, aes(x=long, y=lat, label = region), size = 8) +
  labs(y="Latitude",x="Longitude")+
  theme(text = element_text(size = 20),
        legend.position = "none")
p2
#--------------------- #

# #--------------------- #
# # export
# #--------------------- #
# dir.out = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/output/final_report_2019/"
# ggsave(paste(dir.out, "Figure3_regions_googlemap.png", sep = ""), p2)
# #--------------------- #

# had to save manually
