require(dplyr)
require(ggplot2)
library(ggmap)
# al1 = get_map(location = c(lon = -86.304474, lat = 32.362563), zoom = 11, maptype = 'roadmap')
# al1MAP = ggmap(al1)
# al1MAP

source("U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/code/importRegions.R")

trueCentroids = mutate(trueCentroids, region = paste("Region ", region, sep = ""))

MD = get_map(location = c(lon = -76, lat = 38.5), zoom = 8, maptype = "satellite") #hybrid #watercolor #terrain

p = ggmap(MD)
p2 = p + geom_polygon(data = zips_new, aes(x = long, y = lat, group = group, fill = as.character(region)))+
  scale_fill_manual(values=c("lightgrey","#999999", "gold","#E69F00", "#56B4E9","cornflowerblue"))+
  geom_label(data = trueCentroids, aes(x=long, y=lat, label = region), size = 8) +
  labs(y="Latitude",x="Longitude")+
  theme(text = element_text(size = 20),
        legend.position = "none")
p2

dir.out = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/output/all2018data/"
ggsave(paste(dir.out, "Figure1_regions_googlemap.png", sep = ""), p2)


# p = ggplot()+
#   geom_polygon(data = state, aes(x=long, y=lat, group=group))+
#   geom_polygon(data = zips_new, aes(x = long, y = lat, group = group, fill = as.character(region)))+
#   scale_fill_manual(values=c("lightgrey","#999999", "gold","#E69F00", "#56B4E9","cornflowerblue"))
# 
# 
# library("ggplot2")
# theme_set(theme_bw())
# library("sf")
# library("rworldmap")
# library("rworldxtra")
# library("ggspatial")
# 
# world <- getMap(resolution = "high")
# class(world)
# 
# world <- st_as_sf(world)
# class(world)

# us <- map_data("state")
# 
# DE = Polygons(list(Polygon(filter(us,region %in% "delaware") %>% dplyr::select(long, lat))), ID = "DE")
# DC = Polygons(list(Polygon(filter(us,region %in% "district of columbia") %>% dplyr::select(long, lat))), ID = "DC")
# VA_main = Polygons(list(Polygon(filter(us,region %in% "virginia", subregion %in% "main") %>% dplyr::select(long, lat))), ID = "VAm")
# VA_other = Polygons(list(Polygon(filter(us,region %in% "virginia", !subregion %in% "main") %>% dplyr::select(long, lat))), ID = "VAo")
# PA = Polygons(list(Polygon(filter(us,region %in% "pennsylvania") %>% dplyr::select(long, lat))), ID = "PA")
# 
# Polys = SpatialPolygons(list(DE, DC, VA_main, VA_other, PA))
# surrounding_states = SpatialPolygonsDataFrame(Polys, data.frame(N = c("DE","DC","VAm","VAo","PA"), row.names = c("DE", "DC","VAm","VAo","PA")))
# proj4string(surrounding_states) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# p = ggplot() +
#   geom_polygon(data = surrounding_states, aes(x=long, y=lat, group=group), fill = "antiquewhite1")+
#   geom_polygon(data = state, aes(x=long, y=lat, group=group), fill = "antiquewhite1")+
#   #geom_sf(data = world, fill = "antiquewhite1") +
#   #coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")+
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   #coord_sf(xlim = c(-77.3, -75), ylim = c(37.9, 39.7))+
#   geom_polygon(data = zips_new, aes(x = long, y = lat, group = group, fill = as.character(region)))+
#   scale_fill_manual(values=c("lightgrey","#999999", "gold","#E69F00", "#56B4E9","cornflowerblue"))+
#   theme(panel.background = element_rect(fill = "aliceblue"),
#         text = element_text(size = 20),
#         legend.position = "none")+
#   coord_cartesian(xlim = c(-77.3, -75), ylim = c(37.9, 39.7))
# 
# p2 = p + 
#   geom_label(data = trueCentroids, aes(x=long, y=lat, label = region), size = 8)+
#   #ggtitle("Figure 1. Roving monitor regions")+
#   labs(y="Latitude",x="Longitude")+
#   theme_bw()+
#   theme(text = element_text(size = 20),
#         legend.position = "none")+
#   coord_map(projection = "mercator") +
#   coord_cartesian(xlim = c(-77.3, -75), ylim = c(37.9, 39.64))
# 
# p2

# issues with auto save - save manually
# dir.out = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/output/all2018data/"
# ggsave(paste(dir.out, "Figure1_regions.png", sep = ""), p2)


#register_google(key = "") #use once and only for Kaycee
#myMap <- get_map(location=myLocation, source="osm",  color="bw")

