# --------------------- #
# Use 2019 harvest data for clams and oysters to inform outreach and RM effort
# --------------------- #

# --------------------- #
# load packages
# --------------------- #
library(readxl)
library(dplyr)
library(ggplot2)
library(raster)
library(broom)
# --------------------- #


# --------------------- #
# load data
# --------------------- #
dat <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Shellfish/References/ClamSummary2019.xlsx")
dat2 <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Shellfish/References/OysterSummary2019.xlsx")

dir.shp = "~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Basemaps, NOAA Charts/NOAAcodes/"
NOAAshp = shapefile(paste(dir.shp, "noaaoys_Project", sep=""))
NOAAshp = spTransform(NOAAshp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# need buyticket shapefile for oceanside bays
NOAAshp2 = shapefile(paste(dir.shp, "NOAACodes_ShellfishAll_ForBuyTicketBook", sep=""))
NOAAshp2 = spTransform(NOAAshp2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# make a dataframe
shp_df = broom::tidy(NOAAshp, region = "NOAACODE") 
shp_df2 = broom::tidy(NOAAshp2, region = "NOAACODE") 

# join
dat = rename(dat, id = NOAACODE)
dat2 = rename(dat2, id = Area)

clam_shp = left_join(shp_df, dat, by = "id") %>%
  group_by(id, Species) %>%
  mutate(Bushels = sum(Bushels, na.rm=TRUE))

clam_shp2 = left_join(shp_df2, dat, by = "id") %>%
  group_by(id, Species) %>%
  mutate(Bushels = sum(Bushels, na.rm=TRUE))

oys_shp = left_join(shp_df, dat2, by = "id") %>%
  group_by(id) %>%
  mutate(Bushels = sum(Bushels, na.rm=TRUE))

oys_shp2 = left_join(shp_df2, dat2, by = "id") %>%
  group_by(id) %>%
  mutate(Bushels = sum(Bushels, na.rm=TRUE))
# --------------------- #


# --------------------- #
# plots
# --------------------- #
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Shellfish/Planning/"
p = ggplot() + 
  geom_polygon(data = shp_df2, aes(x=long, y=lat, group = group), col="black", fill="white") +
  geom_polygon(data = clam_shp2[clam_shp2$Species %in% "Razor",], aes(x=long, y=lat, group=group, fill = Bushels)) + 
  labs(x="Longitude",y="Latitude",title="2019 Razor Clam Harvest")
p
ggsave(paste(dir.out, "2019RazorClamHarvest.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = shp_df2, aes(x=long, y=lat, group = group), col="black", fill="white") +
  geom_polygon(data = clam_shp2[clam_shp2$Species %in% "Soft Clam",], aes(x=long, y=lat, group=group, fill = Bushels)) + 
  labs(x="Longitude",y="Latitude",title="2019 Soft Shell Clam Harvest")
p
ggsave(paste(dir.out, "2019SoftShellClamHarvest.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = shp_df2, aes(x=long, y=lat, group = group), col="black", fill="white") +
  geom_polygon(data = clam_shp2[clam_shp2$Species %in% "Hard Clam",], aes(x=long, y=lat, group=group, fill = Bushels)) + 
  labs(x="Longitude",y="Latitude",title="2019 Hard Shell Clam Harvest")
p
ggsave(paste(dir.out, "2019HardShellClamHarvest.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = shp_df2, aes(x=long, y=lat, group = group), col="black", fill="white") +
  geom_polygon(data = oys_shp2, aes(x=long, y=lat, group=group, fill = Bushels)) + 
  labs(x="Longitude",y="Latitude",title="2019 Oyster Harvest")
p
ggsave(paste(dir.out, "2019OysterHarvest.png",sep=""),p)
# --------------------- #
