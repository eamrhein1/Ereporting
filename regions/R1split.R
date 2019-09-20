# -------------------- # 
# this script is to determine where to split R1 efforts to sustain two monitors
# -------------------- # 

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
#library(ggmap)
library(readxl)
library(tidyr)
library(lubridate)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/R1split/"
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

# ggplot() + geom_polygon(data = state, aes(x = long, y = lat, group = group), col = "lightgrey", fill = "lightgrey") +
#   geom_polygon(data = r1, aes(x = long, y = lat, group = group))+
#   theme_bw()+
#   labs(y="Latitude",x="Longitude")+
#   theme(text = element_text(size = 20),
#         legend.position = "none")

# load fishing data
RM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 1)
WM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 2)
WM2018 <- read_excel(paste(dir.in, "WatermenData041819.xlsx", sep=""), sheet = 1)

# take spaces out of names
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
names(WM) = gsub(" ", "", names(WM), fixed = TRUE)
names(WM2018) = gsub(" ", "", names(WM2018), fixed = TRUE)
# -------------------- # 

# -------------------- # 
# filter/format
# -------------------- # 
# RM data
RM = mutate(RM, AssignedMonitor = ifelse(is.na(AssignedMonitor),ReportedBy,AssignedMonitor))
R1_trips_monitored = filter(RM, AssignedMonitor %in% "THOMAS GREEN Jr") %>% dplyr::select(TripID) %>% distinct() %>%
  left_join(., dplyr::select(WM,TripID,EHZip), by="TripID") %>% distinct()
R1_trips_summary = R1_trips_monitored %>% 
  group_by(EHZip) %>% 
  summarise(n=n()) %>% 
  rename(id = EHZip) %>% 
  mutate(id = as.character(id))
r1_df_RM = left_join(r1_df, R1_trips_summary, by = "id") %>% mutate(n = as.numeric(n))

# WM data 2019
R1_WM = filter(WM, EHZip %in% zip_region_list$Zip[zip_region_list$region %in% 1]) %>% 
  dplyr::select(TripID, EHZip) %>%
  distinct()
R1_trips_summary_WM = R1_WM %>% 
  group_by(EHZip) %>% 
  summarise(n=n()) %>% 
  rename(id = EHZip) %>% 
  mutate(id = as.character(id))
r1_df_WM = left_join(r1_df, R1_trips_summary_WM, by = "id") %>% mutate(n = as.numeric(n))

# WM data 2018
R1_WM2018_AugDec  = dplyr::select(WM2018, TripID, TripDate, Zip) %>%
  mutate(TripDate = as.Date(TripDate),
         mo = month(TripDate)) %>% 
  filter(mo %in% c(8,9,10,11,12),
         Zip %in% zip_region_list$Zip[zip_region_list$region %in% 1])
R1_trips_summary_WM_2018 = R1_WM2018_AugDec %>% 
  group_by(Zip) %>% 
  summarise(n=n()) %>% 
  rename(id = Zip) %>% 
  mutate(id = as.character(id))
r1_df_WM2018 = left_join(r1_df, R1_trips_summary_WM_2018, by = "id") %>% mutate(n = as.numeric(n))

R1_WM2018_Aug  = R1_WM2018_AugDec %>% filter(mo %in% 8) %>% group_by(Zip) %>% summarise(n=n()) %>%  rename(id = Zip) %>% mutate(id = as.character(id))
R1_WM2018_Sept  = R1_WM2018_AugDec %>% filter(mo %in% 9) %>% group_by(Zip) %>% summarise(n=n()) %>%  rename(id = Zip) %>% mutate(id = as.character(id))
R1_WM2018_Oct  = R1_WM2018_AugDec %>% filter(mo %in% 10) %>% group_by(Zip) %>% summarise(n=n()) %>%  rename(id = Zip) %>% mutate(id = as.character(id))
R1_WM2018_Nov  = R1_WM2018_AugDec %>% filter(mo %in% 11) %>% group_by(Zip) %>% summarise(n=n()) %>%  rename(id = Zip) %>% mutate(id = as.character(id))
R1_WM2018_Dec  = R1_WM2018_AugDec %>% filter(mo %in% 12) %>% group_by(Zip) %>% summarise(n=n()) %>%  rename(id = Zip) %>% mutate(id = as.character(id))
r1_df_WM2018_Aug = left_join(r1_df, R1_WM2018_Aug, by = "id") %>% mutate(n = as.numeric(n))
r1_df_WM2018_Sept = left_join(r1_df, R1_WM2018_Sept, by = "id") %>% mutate(n = as.numeric(n))
r1_df_WM2018_Oct = left_join(r1_df, R1_WM2018_Oct, by = "id") %>% mutate(n = as.numeric(n))
r1_df_WM2018_Nov = left_join(r1_df, R1_WM2018_Nov, by = "id") %>% mutate(n = as.numeric(n))
r1_df_WM2018_Dec = left_join(r1_df, R1_WM2018_Dec, by = "id") %>% mutate(n = as.numeric(n))

# RM locations on map
# Victoria = White Plains, MD; Zip = 20695 
# TJ = Mechanicsville, MD ; zip = 20659

rms = as.data.frame(rbind(c("TJ", 20659, 1),
                          c("Victoria", 20695, 1)))
names(rms) = c("name","id","n")
r1_df_RMloc = left_join(r1_df, rms, by = "id") %>% mutate(n = as.numeric(n))

rmCentroids = as.data.frame(cbind(c("TJ","Victoria"), 
                                  rbind(mean(zips$long[zips$id %in% 20659]),
                                        mean(zips$long[zips$id %in% 20695])),
                                  rbind(mean(zips$lat[zips$id %in% 20659]),
                                        mean(zips$lat[zips$id %in% 20695]))))
names(rmCentroids) = c("names","long","lat")
rmCentroids = mutate(rmCentroids, 
                     long = as.numeric(as.character(long)), 
                     lat = as.numeric(as.character(lat)), 
                     names = as.character(names))
# -------------------- # 

# -------------------- # 
# explorative plots
# -------------------- # 
p = ggplot() + 
  geom_polygon(data = r1_df_RM, aes(x = long, y = lat, group = group, fill = n), col = "seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Zip codes moniotered by current monitor TJ")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "RM_zip_occurrence_TJ.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='lemonchiffon',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Zip codes used by watermen in 2019 \nwhile monitors have been active")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_while_RMs_present.png",sep=""),p)

#ggplot()+geom_histogram(data = R1_trips_summary_WM, aes(x = reorder(id,n), y=n), stat="identity")+coord_flip()
#ggplot()+geom_histogram(data = R1_trips_summary_WM, aes(x = reorder(id,n), y=log(n)), stat="identity")+coord_flip()

p = ggplot() + 
  geom_polygon(data = r1_df_WM, aes(x = long, y = lat, group = group, fill = log(n)), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='nl(occurrence)') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Zip codes used by watermen in 2019 \nwhile monitors have been active")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_while_RMs_present_log.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Aug.-Dec. 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_AugDec2018.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018_Aug, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("August 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_Aug2018.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018_Sept, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("September 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_Sept2018.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018_Oct, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("October 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_Oct2018.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018_Nov, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("November 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_Nov2018.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018_Dec, aes(x = long, y = lat, group = group, fill = n), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='occurrence') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("December 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_Dec2018.png",sep=""),p)

#ggplot()+geom_histogram(data = R1_trips_summary_WM_2018, aes(x = reorder(id,n), y=n), stat="identity")+coord_flip()
#ggplot()+geom_histogram(data = R1_trips_summary_WM_2018, aes(x = reorder(id,n), y=log(n)), stat="identity")+coord_flip()

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018, aes(x = long, y = lat, group = group, fill = log(n)), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='nl(occurrence)') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Aug.-Dec. 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_AugDec2018_log.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_RMloc, aes(x = long, y = lat, group = group, fill = n)) + 
  scale_fill_gradient("yellow", na.value = "grey")+
  geom_label(data = rmCentroids, aes(x = long, y = lat, label = names))+
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Location of RMs")+
  theme(text = element_text(size = 20),
        legend.position = "none")
p
ggsave(paste(dir.out, "RM_location.png",sep=""),p)

p = ggplot() + 
  geom_polygon(data = r1_df_WM2018, aes(x = long, y = lat, group = group, fill = log(n)), col="seashell") + 
  scale_fill_gradient2(low='yellow',mid="gold",high='navy',na.value = "seashell2",name='nl(occurrence)') +
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Aug.-Dec. 2018 watermen zip codes used")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "WM_zip_occurrence_AugDec2018_log.png",sep=""),p)
# -------------------- #   

# -------------------- #   
# test a split
# -------------------- # 
# # ----- SOLUTION 1 ----- #
# r1a = zips[zips$lat<38.4,] %>% distinct(id) %>% filter(!id %in% c("20657","20685","20688","20629"))
# r1b = zips[!zips$id %in% r1a$id,] %>% distinct(id)
# 
# p = ggplot() + 
#   geom_polygon(data = r1_df, aes(x = long, y = lat, group = group), fill="white") + 
#   geom_polygon(data = filter(r1_df,id %in% r1a$id), aes(x = long, y = lat, group = group), fill="cornflowerblue")+ 
#   geom_polygon(data = filter(r1_df,id %in% r1b$id), aes(x = long, y = lat, group = group), fill="gold")+
#   theme_bw()+
#   labs(x="Longitude",y="Latitude")+
#   ggtitle("Proposed R1 split")+
#   theme(text = element_text(size = 20))+
#   geom_label(data = rmCentroids, aes(x = long, y = lat, label = c("a","b")),size=10)
# p
# ggsave(paste(dir.out, "R1split_solution1_map.png",sep=""),p)
# 
# r1_df_split = r1_df %>% 
#   mutate(subregion = ifelse(id %in% r1a$id, "a","b"))
#  
# # ggplot() + 
# #   geom_polygon(data = r1_df_split, aes(x = long, y = lat, group = group, fill = subregion)) 
# 
# r1a_augdec = R1_WM2018_AugDec %>% filter(mo %in% c(8,9,10,11,12)) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 1, month = "Total")
# r1_aug = R1_WM2018_AugDec %>% filter(mo %in% 8) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 8, month = "Aug.")
# r1_sept = R1_WM2018_AugDec %>% filter(mo %in% 9) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 9, month = "Sep.")
# r1_oct = R1_WM2018_AugDec %>% filter(mo %in% 10) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 10, month = "Oct.")
# r1_nov = R1_WM2018_AugDec %>% filter(mo %in% 11) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 11, month = "Nov.")
# r1_dec = R1_WM2018_AugDec %>% filter(mo %in% 12) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 12, month = "Dec.")
# 
# R1_split_sum = rbind(r1a_augdec, r1_aug, r1_sept, r1_oct, r1_nov, r1_dec)
# 
# p = ggplot() + 
#   geom_histogram(data = R1_split_sum, aes(x = reorder(month,mon), y = n, fill = month), stat="identity")+
#   facet_wrap(~subregion, nrow=2)+
#   theme_bw()+
#   labs(x="Longitude",y="Latitude")+
#   ggtitle("Aug.-Dec. 2018 trip occurrences by subregion")+
#   theme(text = element_text(size = 20), legend.position = "none")+
#   geom_label(data = R1_split_sum, aes(x = reorder(month,mon), y = n, label=n), 
#             position = 'identity', stat = 'identity', size = 5)
# p
# ggsave(paste(dir.out, "R1split_solution1_hist.png",sep=""),p)

# ----- SOLUTION 2 ----- #
r1z = as.data.frame(unique(zips$id[zips$region %in% 1]))
names(r1z) = "id"

r1a = zips[zips$lat<38.51 | zips$id %in% "20658",] %>% distinct(id) %>% 
  filter(!id %in% c("20657","20685","20688","20629","20615","20678","20676"),
         id %in% r1z$id)
r1b = r1z %>% filter(!id %in% r1a$id) 

p = ggplot() + 
  geom_polygon(data = regions, aes(x = long, y = lat, group = group), fill="black") + 
  geom_polygon(data = filter(r1_df,id %in% r1a$id), aes(x = long, y = lat, group = group), fill="darkred")+ 
  geom_polygon(data = filter(r1_df,id %in% r1b$id), aes(x = long, y = lat, group = group), fill="orange")+
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Proposed R1 split")+
  theme(text = element_text(size = 20))+
  geom_label(data = rmCentroids, aes(x = (long), y = (lat)+0.035, label = c("a","b")),size=10)
p
ggsave(paste(dir.out, "R1split_solution2_map.png",sep=""),p)

r1_df_split = r1_df %>% 
  mutate(subregion = ifelse(id %in% r1a$id, "a","b"))

# ggplot() + 
#   geom_polygon(data = r1_df_split, aes(x = long, y = lat, group = group, fill = subregion)) 

r1a_augdec = R1_WM2018_AugDec %>% filter(mo %in% c(8,9,10,11,12)) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 1, month = "Total")
r1_aug = R1_WM2018_AugDec %>% filter(mo %in% 8) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 8, month = "Aug.")
r1_sept = R1_WM2018_AugDec %>% filter(mo %in% 9) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 9, month = "Sep.")
r1_oct = R1_WM2018_AugDec %>% filter(mo %in% 10) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 10, month = "Oct.")
r1_nov = R1_WM2018_AugDec %>% filter(mo %in% 11) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 11, month = "Nov.")
r1_dec = R1_WM2018_AugDec %>% filter(mo %in% 12) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
  group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 12, month = "Dec.")

R1_split_sum = rbind(r1a_augdec, r1_aug, r1_sept, r1_oct, r1_nov, r1_dec)

p = ggplot() + 
  geom_histogram(data = R1_split_sum, aes(x = reorder(month,mon), y = n, fill = month), stat="identity")+
  facet_wrap(~subregion, nrow=2)+
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Aug.-Dec. 2018 trip occurrences by subregion")+
  theme(text = element_text(size = 20), legend.position = "none")+
  geom_label(data = R1_split_sum, aes(x = reorder(month,mon), y = n, label=n), 
             position = 'identity', stat = 'identity', size = 5)
p
ggsave(paste(dir.out, "R1split_solution2_hist.png",sep=""),p)

# # ----- SOLUTION 3 ----- #
# r1a = zips[zips$lat<38.56 | zips$id %in% c("20658","20613"),] %>% distinct(id) %>% 
#   filter(!id %in% c("20657","20685","20688","20629","20615","20678","20676","20639"))
# r1b = zips[!zips$id %in% r1a$id,] %>% distinct(id) 
# 
# p = ggplot() + 
#   geom_polygon(data = r1_df, aes(x = long, y = lat, group = group), fill="white") + 
#   geom_polygon(data = filter(r1_df,id %in% r1a$id), aes(x = long, y = lat, group = group), fill="cornflowerblue")+ 
#   geom_polygon(data = filter(r1_df,id %in% r1b$id), aes(x = long, y = lat, group = group), fill="gold")+
#   theme_bw()+
#   labs(x="Longitude",y="Latitude")+
#   ggtitle("Proposed R1 split")+
#   theme(text = element_text(size = 20))+
#   geom_label(data = rmCentroids, aes(x = (long)-0.05, y = (lat)+0.1, label = c("a","b")),size=10)
# p
# ggsave(paste(dir.out, "R1split_solution3_map.png",sep=""),p)
# 
# r1_df_split = r1_df %>% 
#   mutate(subregion = ifelse(id %in% r1a$id, "a","b"))
#  ggplot() + 
#    geom_polygon(data = r1_df_split, aes(x = long, y = lat, group = group, fill = subregion)) 
# 
# r1a_augdec = R1_WM2018_AugDec %>% filter(mo %in% c(8,9,10,11,12)) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 1, month = "Total")
# r1_aug = R1_WM2018_AugDec %>% filter(mo %in% 8) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 8, month = "Aug.")
# r1_sept = R1_WM2018_AugDec %>% filter(mo %in% 9) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 9, month = "Sep.")
# r1_oct = R1_WM2018_AugDec %>% filter(mo %in% 10) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 10, month = "Oct.")
# r1_nov = R1_WM2018_AugDec %>% filter(mo %in% 11) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 11, month = "Nov.")
# r1_dec = R1_WM2018_AugDec %>% filter(mo %in% 12) %>% mutate(id = as.character(Zip), subregion = ifelse(id %in% r1a$id, "a","b")) %>% 
#   group_by(subregion) %>% summarise(n=n()) %>% mutate(mon = 12, month = "Dec.")
# 
# R1_split_sum = rbind(r1a_augdec, r1_aug, r1_sept, r1_oct, r1_nov, r1_dec)
# 
# p = ggplot() + 
#   geom_histogram(data = R1_split_sum, aes(x = reorder(month,mon), y = n, fill = month), stat="identity")+
#   facet_wrap(~subregion, nrow=2)+
#   theme_bw()+
#   labs(x="Longitude",y="Latitude")+
#   ggtitle("Aug.-Dec. 2018 trip occurrences by subregion")+
#   theme(text = element_text(size = 20), legend.position = "none")+
#   geom_label(data = R1_split_sum, aes(x = reorder(month,mon), y = n, label=n), 
#              position = 'identity', stat = 'identity', size = 5)
# p
# ggsave(paste(dir.out, "R1split_solution3_hist.png",sep=""),p)

# -------------------- # 




# -------------------- # 
# -------------------- # 
# -------------------- # 
# Went with solution 2
# create document for Electric Edge
# -------------------- # 
# r1a (south)
r1a = mutate(r1a, region = "1a") %>% rename(zipcode = id)
# r1b (north)
r1b = mutate(r1b, region = "1b") %>% rename(zipcode = id)
# also combine 3+4 for simplicity
r3and4 = zip_region_list %>% 
  filter(region %in% c(3,4)) %>% 
  rename(zipcode = Zip) %>%
  mutate(region = "3 and 4")
# combine
old_regions = zip_region_list %>% 
  filter(region %in% c(2,5,6)) %>% 
  rename(zipcode = Zip) 
new_regions = rbind(r1a,r1b,r3and4,old_regions) %>% arrange(region)

write.csv(new_regions, paste(dir.out, "new_regions_r1split_3and4combo.csv", sep=""), row.names = FALSE)
any(duplicated(new_regions$zipcode))

# test plot
new_regions = new_regions %>% rename(id = zipcode)
r3and4_df = rbind(r3_df, r4_df)

p = ggplot() + 
  geom_polygon(data = regions, aes(x = long, y = lat, group = group), fill="white", col = "black") + 
  geom_polygon(data = filter(r1_df,id %in% new_regions$id[new_regions$region %in% "1a"]), aes(x = long, y = lat, group = group), fill="yellow")+ 
  geom_polygon(data = filter(r1_df,id %in% new_regions$id[new_regions$region %in% "1b"]), aes(x = long, y = lat, group = group), fill="lightgrey")+ 
  geom_polygon(data = filter(regions,id %in% "2"), aes(x = long, y = lat, group = group), fill="cornflowerblue")+ 
  geom_polygon(data = filter(r3and4_df,id %in% new_regions$id[new_regions$region %in% "3 and 4"]), aes(x = long, y = lat, group = group), fill="gold")+ 
  geom_polygon(data = filter(regions,id %in% "5"), aes(x = long, y = lat, group = group), fill="grey")+ 
  geom_polygon(data = filter(regions,id %in% "6"), aes(x = long, y = lat, group = group), fill="navy")+ 
  theme_bw()+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Proposed Region redesign")+
  theme(text = element_text(size = 20))
p
ggsave(paste(dir.out, "region_redesign_r1split_r3_and_r4_combo.png",sep=""),p)

# -------------------- # 

# -------------------- # 
# split priority list
# -------------------- # 
fish <- read_excel("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Roving Monitor Pilot/Documentation/Resources for RMs/RM scheduling and priority lists/Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sheet = 2)
crab <- read_excel("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Roving Monitor Pilot/Documentation/Resources for RMs/RM scheduling and priority lists/Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sheet = 4)
loc <- read_excel("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Roving Monitor Pilot/Documentation/Resources for RMs/RM scheduling and priority lists/Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sheet = 5)

loc_r1a = filter(loc, Zip %in% r1a$zipcode)
loc_r1b = filter(loc, Zip %in% r1b$zipcode)

fish_r1a = filter(fish, DNRid %in% loc_r1a$DNRid)
fish_r1b = filter(fish, DNRid %in% loc_r1b$DNRid)

crab_r1a = filter(crab, DNRid %in% loc_r1a$DNRid)
crab_r1b = filter(crab, DNRid %in% loc_r1b$DNRid)

write.csv(loc_r1a, paste(dir.out, "r1a_locations.csv", sep=""), row.names = FALSE)
write.csv(loc_r1b, paste(dir.out, "r1b_locations.csv", sep=""), row.names = FALSE)
write.csv(fish_r1a, paste(dir.out, "r1a_fish.csv", sep=""), row.names = FALSE)
write.csv(fish_r1b, paste(dir.out, "r1b_fish.csv", sep=""), row.names = FALSE)
write.csv(crab_r1a, paste(dir.out, "r1a_crab.csv", sep=""), row.names = FALSE)
write.csv(crab_r1b, paste(dir.out, "r1b_crab.csv", sep=""), row.names = FALSE)
# -------------------- # 

