# -------------------- #
# RM charter regions
# created by K. Coleman Dec. 2020
# -------------------- #


# -------------------- #
# load packages
# -------------------- #
require(dplyr)
require(ggplot2)
library(ggmap) #devtools::install_github("dkahle/ggmap")
# register_google(key = "FILL IN API") #use once and only for Kaycee
#myMap <- get_map(location=myLocation, source="osm",  color="bw")
# -------------------- #


# -------------------- #
# load data
# -------------------- #
Zregions = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions/charter_regions_May2020.csv")
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Roving Monitors/code/importRegions.R")
# -------------------- #


# -------------------- #
# regions
# -------------------- #
R1 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 1"]) %>% 
  mutate(charter_region = "Region 1")
R2 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 2"]) %>%
  mutate(charter_region = "Region 2")
R3 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 3"]) %>%
  mutate(charter_region = "Region 3")
R4 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 4"]) %>%
  mutate(charter_region = "Region 4")

# centroids
centers = as.data.frame(matrix(data=NA, nrow = 4, ncol=3))
names(centers) = c("long", "lat", "region")
centers$region = c("Region 1","Region 2","Region 3","Region 4")
centers$lat = c(mean(R1$lat),mean(R2$lat),mean(R3$lat),mean(R4$lat))
centers$long = c(mean(R1$long),mean(R2$long)-0.3,mean(R3$long)+0.1,mean(R4$long))
# -------------------- #


# -------------------- #
# load background map
# -------------------- #
MD = get_map(location = c(lon = -76, lat = 38.5), zoom = 8, maptype = "satellite") #hybrid #watercolor #terrain
# -------------------- #


# -------------------- #
# plot
# -------------------- #
p = ggmap(MD)
p2 = p + 
  geom_polygon(data=R1, aes(x=long, y=lat, group=group),fill="lightgrey") +
  geom_polygon(data=R2, aes(x=long, y=lat, group=group),fill="#E69F00") +
  geom_polygon(data=R3, aes(x=long, y=lat, group=group),fill="gold") +
  geom_polygon(data=R4, aes(x=long, y=lat, group=group),fill="cornflowerblue")+
  geom_label(data = centers, aes(x=long, y=lat, label = region), size = 8) +
  labs(y="Latitude",x="Longitude")+
  theme(text = element_text(size = 20),
        legend.position = "none")
p2
# -------------------- #


# -------------------- #
# export
# -------------------- #
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/finalReport2020/"
ggsave(paste(dir.out, "Figure1_regions_googlemap.png", sep = ""), p2)
# -------------------- #




