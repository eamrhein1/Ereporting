# --------- #
# load packages
# --------- #
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(broom)
library(raster)
library(readxl)
# --------- #

# --------- #
# HOW TO load shapefile
# --------- #
# import codes 
source("G:/NOAAcodes/importNOAAcodes.R")

# import data
dat <- read_excel("U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Charter/Year 1/References/2019_DNRCharterData.xlsx")

# summarise data
dat2 = dat %>% group_by(NOAACODE1) %>% summarise(n=n()) %>% rename(id = NOAACODE1)

# join data
shp_df2 = left_join(shp_df, dat2, by="id")

# plot data
ggplot()+
  geom_polygon(data = shp_df2, aes(x = long, y = lat, group = group, fill = n, col = id))
