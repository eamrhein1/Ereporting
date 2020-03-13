# --------- #
# charter region proposals
# --------- #


# --------- #
# load packages
# --------- #
require(rgdal)
require(dplyr)
require(rgeos)
require(raster)
require(ggplot2)
# --------- #


# --------- #
# set paths
# --------- #
dir.out = "U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions"
# --------- #


# --------- #
# load data
# --------- #
source("U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Roving Monitors/code/importRegions.R")
rm(test, counties, lesser_counties, trueCentroids)
# --------- #


# --------- #
# option 1: 3 RMs divide at Bay Bridge
# --------- #
op1_R1 = counties %>% filter(id %in% c("Anne Arundel","Calvert","Charles","St. Mary's",
                                       "Prince George's")) %>%
  mutate(charter_region = "Region 1")
op1_R2 = counties %>% filter(id %in% c("Kent","Baltimore","Baltimore City",
                                       "Cecil","Harford")) %>%
  mutate(charter_region = "Region 2")
op1_R3 = counties %>% filter(id %in% c("Talbot","Worcester","Caroline",
                                       "Dorchester","Somerset","Queen Anne's",
                                       "Wicomico")) %>%
  mutate(charter_region = "Region 3")

p = ggplot() +
  geom_polygon(data = op1_R1, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op1_R2, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op1_R3, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  theme_bw() + labs(y = "Latitude", x = "Longitude") + 
  theme(text = element_text(size = 20))
p 
ggsave(paste(dir.out, "charter_regions_option1.png", sep=""), p)
# 
# coordinates(op1_R1) = ~long+lat
# coordinates(op1_R2) = ~long+lat
# coordinates(op1_R3) = ~long+lat
# 
# dat1 <- data.frame(id = rep(1,5), row.names = unique(op1_R1$id), region = rep(1,5))
# dat2 <- data.frame(id = rep(2,5), row.names = unique(op1_R2$id), region = rep(2,5))
# dat3 <- data.frame(id = rep(3,7), row.names = unique(op1_R3$id), region = rep(3,7))
# 
# points2polygons <- function(df,data) {
#   get.grpPoly <- function(group,ID,df) {
#     Polygon(coordinates(df[df$id==ID & df$group==group,]))
#   }
#   get.spPoly  <- function(ID,df) {
#     Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
#   }
#   spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
#   SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
# }
# spDF_R1 <- points2polygons(op1_R1,dat1)
# spDF_R2 <- points2polygons(op1_R2,dat2)
# spDF_R3 <- points2polygons(op1_R3,dat3)
# 
# spDF_R1.coords = coordinates(spDF_R1)
# spDF_R1.id <- cut(spDF_R1.coords[,1], quantile(spDF_R1.coords[,1]), include.lowest=TRUE)
# R1union <- unionSpatialPolygons(spDF_R1, spDF_R1.id)
# 
# writeOGR(spDF,dsn=".",layer="chart_regions_option1", driver="ESRI Shapefile")
# --------- #


# --------- #
# option 2: 4 RMs similar divide to 2019 Dec. RMs
# --------- #
op1_R1 = counties %>% filter(id %in% c("Anne Arundel","Calvert","Charles","St. Mary's",
                                       "Prince George's")) %>%
  mutate(charter_region = "Region 1")
op1_R2 = counties %>% filter(id %in% c("Kent","Baltimore","Baltimore City",
                                       "Cecil","Harford")) %>%
  mutate(charter_region = "Region 2")
op1_R3 = counties %>% filter(id %in% c("Talbot","Worcester","Caroline",
                                       "Dorchester","Somerset","Queen Anne's",
                                       "Wicomico")) %>%
  mutate(charter_region = "Region 3")
op1_R4 = counties %>% filter(id %in% c("Talbot","Worcester","Caroline",
                                       "Dorchester","Somerset","Queen Anne's",
                                       "Wicomico")) %>%
  mutate(charter_region = "Region 4")

p = ggplot() +
  geom_polygon(data = op1_R1, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op1_R2, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op1_R3, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  theme_bw() + labs(y = "Latitude", x = "Longitude") + 
  theme(text = element_text(size = 20))
p 
ggsave(paste(dir.out, "charter_regions_option1.png", sep=""), p)
# --------- #
