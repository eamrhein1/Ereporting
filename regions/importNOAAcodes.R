# read in NOAA codes

# load packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(broom)

# import data
dir.shp = "G:/NOAAcodes/"

# load shapefile
NOAAshp = shapefile("G:/NOAAcodes/noaaoys_Project")
NOAAshp = spTransform(NOAAshp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# make a dataframe
shp_df = broom::tidy(NOAAshp, region = "NOAACODE") 

# plot
ggplot()+
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = id), col = "black") + 
  theme_bw() + labs(x="Longitude", y="Latitude") + 
  theme(text = element_text(size=20))

# codes used: 5   7   9  13  14  25  27  29  31  37  39  41  43  45  47  53  55  57  59  60  62  63  66  68  72  74  76  78  80  82  88 90  92  94  96  99 112 131 137 168 237 268



