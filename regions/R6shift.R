# test where the R6 expansion should be for December. 
# region 6 has very few watermen in December so we are trying enhance efficiency by 
# shifting region 6 north, since the RM for region 5 also covers region 4

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/R6shift/"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
source("U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/code/importRegions.R")
#rm(r2,r3,r4,r5,r6,lesser_counties)

# change polygon to df with zips
r1_df = broom::tidy(r1, region = "Tidal_Co_1") 
r2_df = broom::tidy(r2, region = "Tidal_Co_1") 
r3_df = broom::tidy(r3, region = "Tidal_Co_1") 
r4_df = broom::tidy(r4, region = "Tidal_Co_1") 
r5_df = broom::tidy(r5, region = "Tidal_Co_1") 
r6_df = broom::tidy(r6, region = "Tidal_Co_1") 

# load fishing data
WM2018 <- read_excel(paste(dir.in, "WatermenData041819.xlsx", sep=""), sheet = 1)
Dec = WM2018 %>% mutate(mo = month(TripDate), 
                        day = day(TripDate),
                        Zip = as.character(Zip)) %>% 
  filter(mo %in% 12, day < 16) %>%
  left_join(., zip_region_list, by="Zip") %>%
  filter(region %in% c(4,5,6))

# ggplot() + geom_histogram(data = Dec, aes(x = region), stat = "count")
# Dec %>% group_by(region) %>% distinct(FisherName) %>% summarize(n=n())

# take spaces out of names
names(WM2018) = gsub(" ", "", names(WM2018), fixed = TRUE)
# -------------------- # 

# -------------------- # 
# spatial descriptive plots
# -------------------- # 
# summarize
r4_trips = filter(Dec, region %in% 4) %>% group_by(Zip) %>% summarize(n = n()) %>% rename(id = Zip)
r5_trips = filter(Dec, region %in% 5) %>% group_by(Zip) %>% summarize(n = n()) %>% rename(id = Zip)
r6_trips = filter(Dec, region %in% 6) %>% group_by(Zip) %>% summarize(n = n()) %>% rename(id = Zip)

r4_df_trips = left_join(r4_df, r4_trips, by = "id") 
r5_df_trips = left_join(r5_df, r5_trips, by = "id") 
r6_df_trips = left_join(r6_df, r6_trips, by = "id") 

# zipcode use
p = ggplot() + geom_polygon(data = r4_df_trips, aes(x = long, y = lat, group = group, fill = n), col = "black") + 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Region 4 trips Dec. 2018")
p
ggsave(paste(dir.out, "R4trips.png", sep=""), p)

p = ggplot() + geom_polygon(data = r5_df_trips, aes(x = long, y = lat, group = group, fill = n), col = "black") + 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Region 5 trips Dec. 2018")
p
ggsave(paste(dir.out, "R5trips.png", sep=""), p)

p = ggplot() + geom_polygon(data = r6_df_trips, aes(x = long, y = lat, group = group, fill = n), col = "black")+ 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Region 6 trips Dec. 2018")
p
ggsave(paste(dir.out, "R6trips.png", sep=""), p)

# division
r5_df_trips2 = r5_df_trips %>% mutate(div = ifelse(lat < 38.8,"south","north"),
                                      div = replace(div, id %in% c("21647", "21676"), "south"))

ggplot() + geom_polygon(data = r5_df_trips2, aes(x = long, y = lat, group = group, fill = div), col = "black") + 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Region 5 split")
# -------------------- # 

# -------------------- # 
# new zips with regions
# -------------------- # 
x = r5_df_trips2 %>% dplyr::filter(div %in% "south") %>% distinct(id)
y = r5_df_trips2 %>% dplyr::filter(div %in% "north") %>% distinct(id) %>% filter(!id %in% x$id)

new_zips = zip_region_list %>% mutate(region = replace(region, region %in% 4, "4 and 5 north"),
                                      region = replace(region, Zip %in% x$id, "5 south and 6"),
                                      region = replace(region, Zip %in% y$id, "4 and 5 north"),
                                      region = replace(region, region %in% 6, "5 south and 6"),
                                      region = replace(region, region %in% c(2,3), "2 and 3")) 

# plot
r5_df_test = left_join(r5_df, new_zips %>% rename(id = Zip), by = "id") 
p = ggplot() + geom_polygon(data = r5_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Region 5 split")
p
ggsave(paste(dir.out, "R5split.png", sep=""), p)

r1_df_test = left_join(r1_df, new_zips %>% rename(id = Zip), by = "id") 
r2_df_test = left_join(r2_df, new_zips %>% rename(id = Zip), by = "id") 
r3_df_test = left_join(r3_df, new_zips %>% rename(id = Zip), by = "id") 
r4_df_test = left_join(r4_df, new_zips %>% rename(id = Zip), by = "id") 
r6_df_test = left_join(r6_df, new_zips %>% rename(id = Zip), by = "id") 

p = ggplot() + 
  geom_polygon(data = r1_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  geom_polygon(data = r2_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  geom_polygon(data = r3_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  geom_polygon(data = r4_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  geom_polygon(data = r5_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  geom_polygon(data = r6_df_test, aes(x = long, y = lat, group = group, fill = region), col = "black")+ 
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("Dec. 2019 Proposed Regions")
p
ggsave(paste(dir.out, "Dec2019Regions.png", sep=""), p)
# -------------------- # 

# export 
write.csv(new_zips, paste(dir.out, "Dec2019_region_zips.csv", sep=""), row.names = FALSE)
