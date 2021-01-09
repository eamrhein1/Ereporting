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
trips <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_010720.csv")
reports <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_010720.csv")
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

# remove false data
reports = filter(reports, !ReportedBy %in% "Baleze Danoit")

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

# how many monitoring trips were done
length(unique(reports$TripID))
length(unique(reports$TripID[reports$Onboard %in% "Y"]))
length(unique(reports$TripID[reports$Onboard %in% "N"]))

# how many successful monitoring trips were there
length(unique(reports$TripID[reports$Result %in% c("MONITORED", "MONITORED (on paper)")]))
(length(unique(reports$TripID[reports$Result %in% c("MONITORED", "MONITORED (on paper)")]))/ length(unique(reports$TripID))) * 100

length(unique(reports$TripID[reports$Result %in% c("MONITORED", "MONITORED (on paper)") & reports$Onboard %in% "Y"]))

length(unique(reports$TripID[reports$Result %in% c("MONITORED", "MONITORED (on paper)") & reports$Onboard %in% "N"]))
(length(unique(reports$TripID[reports$Result %in% c("MONITORED", "MONITORED (on paper)") & reports$Onboard %in% "N"]))/ length(unique(reports$TripID[reports$Onboard %in% "N"]))) * 100
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

# percent of trips monitors missed the species count
(length(which(is.na(spp_count_combo$count_diff)==TRUE))/length(spp_count_combo$count_diff))*100
# percent of trips monitors angler count was the same as captain angler count
(length(which(spp_count_combo$count_diff==0))/length(spp_count_combo$count_diff))*100
# range counts were off by
mean(spp_count_combo$count_diff, na.rm=T)
sd(spp_count_combo$count_diff, na.rm=T)

median(spp_count_combo$count_diff, na.rm=T)
min(spp_count_combo$count_diff, na.rm=T)
max(spp_count_combo$count_diff, na.rm=T)
# visually inspect which species had counts off 
# typically more bait fish had higher counts off
x = filter(spp_count_combo, !count_diff %in% 0)

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

# how many fish were filleted before they reacted the dock (e.g. captain said kept, monitor said kept fileted)
# this may not be right
x = dplyr::select(reports, TripID, SpeciesGrade, Quantity, Disposition, Result) %>% 
  rename(Species = SpeciesGrade) %>%
  filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>% 
  distinct() %>% 
  filter(!Disposition %in% "Released") 
y = dplyr::select(trips, TripID, Species, Quantity, Disposition) %>%
  distinct() %>%
  filter(!Disposition %in% "Released",
         TripID %in% x$TripID)
not_matched = full_join(x,y,by=c("TripID","Species","Disposition")) %>%
  mutate(q = sum(Quantity.x, Quantity.y, na.rm=FALSE)) %>%
  filter(is.na(q)) %>%
  group_by(TripID, Species) %>% 
  summarise(n=n()) %>% 
  filter(!n %in% 1)

spp_disp_change = 
  
# combo 
spp_quantity_combo = inner_join(dplyr::select(trips, TripID, Species, Quantity, Disposition) %>%
                                  distinct() %>%
                                  filter(Disposition %in% c("Kept")), 
                             dplyr::select(reports, TripID, SpeciesGrade, Quantity, Disposition, Result) %>% 
                               rename(Species = SpeciesGrade) %>%
                               filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>% 
                               distinct() %>% 
                               filter(Disposition %in% c("Kept")), 
                             by = c("TripID","Species","Disposition")) %>%
  rename(trip_quantity = Quantity.x,
         monitor_quantity = Quantity.y) %>%
  mutate(quantity_diff = trip_quantity - monitor_quantity) 

# percent of reports that match
(length(which(spp_quantity_combo$quantity_diff==0))/length(spp_quantity_combo$quantity_diff))*100

# range weights were off by
mean(spp_quantity_combo$quantity_diff, na.rm=T)
sd(spp_quantity_combo$quantity_diff, na.rm=T)

median(spp_quantity_combo$quantity_diff, na.rm=T)
min(spp_quantity_combo$quantity_diff, na.rm=T)
max(spp_quantity_combo$quantity_diff, na.rm=T)

# visually inspect which species had counts off 
# typically more bait fish had higher counts off
x = filter(spp_quantity_combo, !quantity_diff %in% 0)

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

