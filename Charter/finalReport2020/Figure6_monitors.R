# -------------------- #
# compare monitor reports to charter trips
# created by K. Coleman Dec. 2020
# -------------------- #


# -------------------- #
# load packages
# -------------------- #
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
# -------------------- #


# -------------------- #
# load data
# -------------------- #
trips <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_122920.csv")
reports <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_122920.csv")
timeslots <- read_excel("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Charter/Year 1/Final Report/RM shifts worked for analysis.xlsx")
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
names(timeslots) = gsub(" ","", names(timeslots))

# filter so that the trip data is only last report
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
trips = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) 

# filter so that the report data is only last report
names(reports) = gsub(" ","", names(reports))
names(reports) = gsub("#","", names(reports))
names(reports) = gsub("/","", names(reports))

reports = reports %>%
  group_by(TripID) %>%
  mutate(lastR = ifelse(MonitorReport == max(MonitorReport),"yes","no")) %>%
  filter(lastR %in% "yes") %>%
  dplyr::select(-lastR) 

# fix species name differences
trips = trips %>%
  mutate(Species = replace(Species, Species %in% "DRUM, BLACK", "BLACK DRUM"),                 
         Species = replace(Species, Species %in% "SEA BASS, BLACK", "BLACK SEA BASS"),            
         Species = replace(Species, Species %in% "BLUEFISH, UNC", "BLUEFISH"),
         Species = replace(Species, Species %in% "CATFISH, BLUE", "BLUE CATFISH"),               
         Species = replace(Species, Species %in% "CATFISH, CHANNEL", "CHANNEL CATFISH"),            
         Species = replace(Species, Species %in% "MACKEREL, SPANISH" , "SPANISH MACKEREL"),          
         Species = replace(Species, Species %in% "SEA TROUT, SPOTTED", "SPOTTED SEA TROUT"),         
         Species = replace(Species, Species %in% "BASS, STRIPED", "STRIPED BASS"),               
         Species = replace(Species, Species %in% "SEA TROUT, GRAY / WEAKFISH", "WEAKFISH OR GRAY SEA TROUT"), 
         Species = replace(Species, Species %in% "PERCH, WHITE", "WHITE PERCH"))  
# -------------------- #


# -------------------- #
# Compare data
# -------------------- #
###### Angler Count
AC_combo = inner_join(dplyr::select(trips, TripID, AnglerCount) %>% distinct(), 
                      dplyr::select(reports, TripID, AnglerCount, Result) %>% 
                        filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>% 
                        distinct() %>% dplyr::select(-Result), 
                      by = "TripID")
names(AC_combo) = c("TripID","captain_ac","monitor_ac")
AC_combo = AC_combo %>% mutate(ac_diff = captain_ac - monitor_ac)

# percent of trips monitors missed the angler count
(length(which(is.na(AC_combo$monitor_ac)==TRUE))/length(AC_combo$monitor_ac))*100
# percent of trips monitors angler count was the same as captain angler count
(length(which(AC_combo$ac_diff==0))/length(AC_combo$monitor_ac))*100
# range counts were off by
min(AC_combo$ac_diff, na.rm=T)
max(AC_combo$ac_diff, na.rm=T)
rm(AC_combo)

###### Species Count
spp_count_combo = inner_join(dplyr::select(trips, TripID, Species, Count, Disposition) %>% 
                         distinct() %>%
                         mutate(Disposition = replace(Disposition, Disposition %in% "Kept - fileted", "Kept")) %>%
                         group_by(TripID, Species, Disposition) %>%
                         summarise(Count = sum(Count)), 
                      dplyr::select(reports, TripID, SpeciesGrade, Count, Disposition, Result) %>% 
                        rename(Species = SpeciesGrade) %>%
                        filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>% 
                        distinct() %>% 
                        mutate(Disposition = replace(Disposition, Disposition %in% "Kept - fileted", "Kept")) %>%
                        group_by(TripID, Species, Disposition) %>%
                        summarise(Count = sum(Count)), 
                      by = c("TripID","Species","Disposition")) %>%
  rename(trip_count = Count.x,
         monitor_count = Count.y) %>%
  mutate(count_diff = trip_count - monitor_count)

# range counts were off by
mean(spp_count_combo$count_diff, na.rm=T)
median(spp_count_combo$count_diff, na.rm=T)
min(spp_count_combo$count_diff, na.rm=T)
max(spp_count_combo$count_diff, na.rm=T)

# plot
p = ggplot() + 
  geom_boxplot(data = spp_count_combo, aes(x = Species, y = trip_count), col="blue") + 
  geom_boxplot(data = spp_count_combo, aes(x = Species, y = monitor_count),col="gold") + 
  facet_grid(Disposition~., scales='free')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
p

rm(spp_count_combo)

###### Species Weight
spp_quantity_combo = inner_join(dplyr::select(trips, TripID, Species, Quantity, Disposition) %>% 
                               distinct() %>%
                               mutate(Disposition = replace(Disposition, Disposition %in% "Kept - fileted", "Kept")) %>%
                               group_by(TripID, Species, Disposition) %>%
                               summarise(Quantity = sum(Quantity)), 
                             dplyr::select(reports, TripID, SpeciesGrade, Quantity, Disposition, Result) %>% 
                               rename(Species = SpeciesGrade) %>%
                               filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>% 
                               distinct() %>% 
                               mutate(Disposition = replace(Disposition, Disposition %in% "Kept - fileted", "Kept")) %>%
                               group_by(TripID, Species, Disposition) %>%
                               summarise(Quantity = sum(Quantity)), 
                             by = c("TripID","Species","Disposition")) %>%
  rename(trip_quantity = Quantity.x,
         monitor_quantity = Quantity.y) %>%
  mutate(quantity_diff = trip_quantity - monitor_quantity)

# range weights were off by
mean(spp_quantity_combo$quantity_diff, na.rm=T)
median(spp_quantity_combo$quantity_diff, na.rm=T)
min(spp_quantity_combo$quantity_diff, na.rm=T)
max(spp_quantity_combo$quantity_diff, na.rm=T)

# plot
p = ggplot() + 
  geom_boxplot(data = spp_quantity_combo, aes(x = Species, y = trip_quantity), col="blue") + 
  geom_boxplot(data = spp_quantity_combo, aes(x = Species, y = monitor_quantity),col="gold") + 
  facet_grid(Disposition~., scales='free')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")
p

rm(spp_quantity_combo)
# -------------------- #

