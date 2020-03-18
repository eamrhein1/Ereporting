# --------- #
# charter region proposals
# --------- #


# --------- #
# load packages
# --------- #
require(rgdal)
require(dplyr)
require(raster)
require(ggplot2)
require(rgeos)
# --------- #


# --------- #
# set paths
# --------- #
#dir.out = "U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions"
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions"
# --------- #


# --------- #
# load data
# --------- #
#source("U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Roving Monitors/code/importRegions.R")
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Roving Monitors/code/importRegions.R")
rm(test, lesser_counties, trueCentroids)
# --------- #


# --------- #
# option 1: 3 RMs divide at Bay Bridge
# --------- #
#"21043" "21219" "21221" "21222" "21225" "21226" "21227" "21228" "21230" "21250"
op1_R1 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 1]),
                                   "20701", "20711", "20723", "20724", "20733", 
                                   "20751", "20755", "20758", "20764", "20765", 
                                   "20776", "20778", "20779", "20794", "20866", 
                                   "21012", "21032", "21035", "21037", "21054", 
                                   "21056", "21060", "21061", "21075", "21076", 
                                   "21077", "21090", "21108", "21113", "21114", 
                                   "21122", "21140", "21144", "21146", "21240", 
                                   "21401", "21402", "21403", "21405", "21409")) %>%
  mutate(charter_region = "Region 1")
op1_R2 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 3]), 
                                   unique(zips$id[zips$region %in% 4]),
                                   "21013", "21030", "21031", "21042", "21043", 
                                   "21048", "21051", "21053", "21057", "21071", 
                                   "21074", "21082", "21087", "21093", "21102", 
                                   "21104", "21111", "21117", "21120", "21128", 
                                   "21131", "21133", "21136", "21152", "21153", 
                                   "21155", "21156", "21161", "21162", "21163", 
                                   "21201", "21202", "21204", "21205", "21206", 
                                   "21207", "21208", "21209", "21210", "21211", 
                                   "21212", "21213", "21214", "21215", "21216", 
                                   "21217", "21218", "21219", "21220", "21221", 
                                   "21222", "21223", "21224", "21225", "21226",
                                   "21227", "21228", "21229", "21230", "21230", 
                                   "21231", "21234", "21235", "21236", "21237", 
                                   "21239", "21241", "21244", "21250", "21251",
                                   "21252", "21286", "21287", "21784", "21651")) %>%
  mutate(charter_region = "Region 2")
op1_R3 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 6]),
                                   "21601", "21612", "21617", "21619", "21625", 
                                   "21629", "21632", "21636", "21638", "21639", 
                                   "21647", "21652", "21654", "21655", "21657", 
                                   "21658", "21660", "21662", "21663", "21665", 
                                   "21666", "21671", "21673", "21676", "21679",
                                   "21649", "21636", "21640", "21623", "21668", 
                                   "21644", "21607")) %>%
  mutate(charter_region = "Region 3")

p = ggplot() +
  geom_polygon(data = op1_R1, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op1_R2, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  #geom_polygon(data = counties, aes(x = long, y=lat, group = group), fill = NA, col = "black")
  geom_polygon(data = op1_R3, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  theme_bw() + labs(y = "Latitude", x = "Longitude") + 
  theme(text = element_text(size = 20))
p 
ggsave(paste(dir.out, "charter_regions_option1.png", sep=""), p)

# op1_R1 = counties %>% filter(id %in% c("Anne Arundel","Calvert","Charles","St. Mary's",
#                                        "Prince George's")) %>%
#   mutate(charter_region = "Region 1")
# op1_R2 = counties %>% filter(id %in% c("Kent","Baltimore","Baltimore City",
#                                        "Cecil","Harford")) %>%
#   mutate(charter_region = "Region 2")
# op1_R3 = counties %>% filter(id %in% c("Talbot","Worcester","Caroline",
#                                        "Dorchester","Somerset","Queen Anne's",
#                                        "Wicomico")) %>%
#   mutate(charter_region = "Region 3")
# 
# p = ggplot() +
#   geom_polygon(data = op1_R1, aes(x= long, y = lat, group = group, fill = charter_region)) + 
#   geom_polygon(data = op1_R2, aes(x= long, y = lat, group = group, fill = charter_region)) + 
#   geom_polygon(data = op1_R3, aes(x= long, y = lat, group = group, fill = charter_region)) + 
#   theme_bw() + labs(y = "Latitude", x = "Longitude") + 
#   theme(text = element_text(size = 20))
# p 
# ggsave(paste(dir.out, "charter_regions_option1.png", sep=""), p)
# # 
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
op2_R1 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 1]),
                                   "20711", "20733", "20751", "20754", "20758", 
                                   "20764", "20765", "20778", "20779"),
                         !id %in% c("20705", "20706", "20707", "20708", "20710", 
                                    "20712", "20715", "20716", "20720", "20721", 
                                    "20722", "20737", "20740", "20742", "20743", 
                                    "20769", "20770", "20771", "20774", "20781", 
                                    "20782", "20783", "20784", "20785")) %>% 
  mutate(charter_region = "Region 1")
op2_R2 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 3]),
                                   "20701", "20723", "20724", "20755", "20794", 
                                   "20776", "20866", "21012", "21013", "21030", 
                                   "21031", "21032", "21035", "21037", "21042", 
                                   "21043", "21048", "21051", "21053", "21054", 
                                   "21056", "21057", "21060", "21061", "21071", 
                                   "21074", "21075", "21076", "21077", "21082", 
                                   "21087", "21090", "21093", "21102", "21104", 
                                   "21108", "21111", "21113", "21114", "21117", 
                                   "21120", "21122", "21128", "21131", "21133", 
                                   "21136", "21140", "21144", "21146", "21152", 
                                   "21153", "21155", "21156", "21161", "21162", 
                                   "21163", "21201", "21202", "21204", "21205", 
                                   "21206", "21207", "21208", "21209", "21210", 
                                   "21211", "21212", "21213", "21214", "21215", 
                                   "21216", "21217", "21218", "21219", "21220", 
                                   "21221", "21222", "21223", "21224", "21225", 
                                   "21226", "21227", "21228", "21229", "21230", 
                                   "21230", "21231", "21234", "21235", "21236", 
                                   "21237", "21239", "21240", "21241", "21244", 
                                   "21250", "21251", "21252", "21286", "21287", 
                                   "21401", "21402", "21403", "21405", "21409",
                                   "21784",
                                   "20705", "20706", "20707", "20708", "20710", 
                                   "20712", "20715", "20716", "20720", "20721", 
                                   "20722", "20737", "20740", "20742", "20743", 
                                   "20769", "20770", "20771", "20774", "20781", 
                                   "20782", "20783", "20784", "20785")) %>%
  mutate(charter_region = "Region 2")
op2_R3 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 4]),
                                   "21617", "21619", "21636", 
                                   "21638", "21639", "21657", "21658", 
                                   "21660", "21666", "21679", "21649", 
                                   "21636", "21640", "21623", "21668", "21644", 
                                   "21607", "21651")) %>%
  mutate(charter_region = "Region 3")
op2_R4 = zips %>% filter(id %in% c(unique(zips$id[zips$region %in% 6]),
                                   "21601", "21612", "21632", "21652", "21654", 
                                   "21655", "21662", "21663", "21665", "21671",
                                   "21673", "21629", "21647", "21676", "21625")) %>%
                           mutate(charter_region = "Region 4")
                                   

p = ggplot() +
  geom_polygon(data = op2_R1, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op2_R2, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op2_R3, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  geom_polygon(data = op2_R4, aes(x= long, y = lat, group = group, fill = charter_region)) + 
  theme_bw() + labs(y = "Latitude", x = "Longitude") + 
  theme(text = element_text(size = 20))
p 
ggsave(paste(dir.out, "charter_regions_option2.png", sep=""), p)
# --------- #
