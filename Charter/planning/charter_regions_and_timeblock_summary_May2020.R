# ------------------ #
# check that timeblocks and proprosed regions should work now that the charter fishery has been operational for a few weeks
# ------------------ #

# ------------------ #
# load packages
# ------------------ #
require(dplyr)
library(readxl)
require(ggplot2)
# ------------------ #

# ------------------ #
# load data
# ------------------ #
# regions
Zregions = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions/charter_regions_May2020.csv")

# charter data (source = https://jira.fisheryfacts.com/browse/FACTSMD-788)
dat <- read_excel("~/Downloads/Charter Data (1).xls")
names(dat) = c("tripID", "date","captainsName","vessel","SH_time",
               "EH_time","SH_landing_time", "landing_time","landing_loc","zipcode")
# ------------------ #

# ------------------ #
# summarise
# ------------------ #
# proposed blocks
# 0900 – 1300
# 1100 – 1500
# 1400 – 1800
# 1700 – 2100

# time
hr_sum = dat %>% mutate(landing_time = as.character(landing_time),
                        time = sapply(strsplit(landing_time, " "), tail, 1),
                        hr = sapply(strsplit(time, ":"), head, 1)) %>% 
  group_by(hr) %>%
  summarise(n=n())
EH_hr_sum = dat %>% mutate(EH_time = as.character(EH_time),
                           time = sapply(strsplit(EH_time, " "), tail, 1),
                           hr = sapply(strsplit(time, ":"), head, 1)) %>% 
  group_by(hr) %>%
  summarise(n=n())

p = ggplot() + geom_bar(data = hr_sum, aes(hr, n), stat="identity") + 
  theme_bw() +
  ggtitle("Trips per Landing Hour") +
  labs(y = "Number of Trips", x = "Hour") + 
  theme(text = element_text(size = 20))
p 
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/charter_landingHours_May2020.png", p)

p = ggplot() + 
  geom_bar(data = hr_sum, aes(hr, n), stat="identity") + 
  geom_bar(data = EH_hr_sum, aes(hr, n), stat="identity", fill = "magenta", color = "black", position = "dodge", width =0.5)+
  theme_bw() +
  ggtitle("Trips per Landing Hour with EH (pink)") +
  labs(y = "Number of Trips", x = "Hour") + 
  theme(text = element_text(size = 20))
p 
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/charter_landingHours_and_EHhours_May2020.png", p)

# regions
region_sum = left_join(dat, regions, by = "zipcode") %>%
  filter(!is.na(charter_region)) %>% 
  group_by(., charter_region) %>%
  summarise(n=n())

# load zip shapefile
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Roving Monitors/code/importRegions.R")

R1 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 1"]) %>% 
  mutate(charter_region = "Region 1")
R2 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 2"]) %>%
  mutate(charter_region = "Region 2")
R3 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 3"]) %>%
  mutate(charter_region = "Region 3")
R4 = zips %>% filter(id %in% Zregions$zipcode[Zregions$charter_region %in% "Region 4"]) %>%
  mutate(charter_region = "Region 4")

zip_sum = dat %>% group_by(zipcode) %>%
  summarise(n=n()) %>% 
  mutate(id = as.character(zipcode))

R1 = left_join(R1, zip_sum, by = "id")
R2 = left_join(R2, zip_sum, by = "id")
R3 = left_join(R3, zip_sum, by = "id")
R4 = left_join(R4, zip_sum, by = "id")

p = ggplot() +
  geom_polygon(data = R1, aes(x= long, y = lat, group = group, fill = n)) + 
  geom_polygon(data = R2, aes(x= long, y = lat, group = group, fill = n)) + 
  geom_polygon(data = R3, aes(x= long, y = lat, group = group, fill = n)) + 
  geom_polygon(data = R4, aes(x= long, y = lat, group = group, fill = n)) + 
  theme_bw() + labs(y = "Latitude", x = "Longitude") + 
  theme(text = element_text(size = 20)) +
  ggtitle("Number of Trips by Zip")
p 
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/charter_trips_by_Zip_May2020.png", p)
# ------------------ #

