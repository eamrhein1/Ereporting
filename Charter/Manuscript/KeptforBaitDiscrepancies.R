library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(lubridate)
library(readr)

# ------------- #
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RMdat <- rbind(RMdat2020, RMdat2021)
# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
Captaindat <- rbind(dat2020, dat2021)

# fix names
names(Captaindat) = gsub(" ","", names(Captaindat))
names(Captaindat) = gsub("#","", names(Captaindat))
names(Captaindat) <- gsub("\\.", "", names(Captaindat))
names(RMdat) = gsub(" ","", names(RMdat))
names(RMdat) = gsub("#","", names(RMdat))
names(RMdat) <- gsub("\\.", "", names(RMdat))
names(RMdat) <- gsub("/", "", names(RMdat))

# filter to last trip reported
dat = Captaindat %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together
# ------------- #

# ------------- #
#Look at onboard reports only for bait
# ------------- #
obKB = RMdat %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

#Number of onboard trips with Spot KB (16)
length(unique(obKB$TripID))

#Number of captain reports with released SB during onboard trips (12) 
ob_tripKB = Captaindat %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")
length(unique(ob_tripKB$TripID))

#Percentage of onboard trips with Spot releases where captains also reported Spot releases (75%)
(length(unique(ob_tripKB$TripID))/length(unique(obKB$TripID))*100)

#Compare number of Spot KB from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumKB = obKB %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripKB_sum = ob_tripKB %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSpotKB = left_join(ob_sumKB, ob_tripKB_sum, by = "TripID")
names(TotalSpotKB) = c("TripID", "ObserverKB", "CaptainKB")

#Total number of Spot KB
sum(ob_sumKB$count) #onboard observer reported 892
sum(ob_tripKB_sum$count) #Captains reported 643 (72.1%)
(sum(ob_tripKB_sum$count)/sum(ob_sumKB$count))*100

######Look at Roving monitor reports for spot kept for bait######
# ------------- #
#Number of RM reports with spot kept for bait (73)
RMKB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         SpeciesGrade %in% "SPOT")
length(unique(RMKB$TripID))

#Monitored captain reports with Spot KB (33)
RM_tripKB = Captaindat %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")
length(unique(RM_tripKB$TripID))

#Percentage of monitored trips with SPOT KB where captains also reported Spot KB (45%)
(length(unique(RM_tripKB$TripID))/length(unique(RMKB$TripID))*100)

#Compare number of spot KB from RM reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumKB = RMKB %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_trip_sumKB = RM_tripKB %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalKB_RM = left_join(RM_sumKB, RM_trip_sumKB, by = "TripID")
names(TotalKB_RM) = c("TripID", "RMKB", "CaptainKB")

#Total Spot KB from RM reports and Captain reports
sum(RM_sumKB$count)
# Total RM striped bass releases: 4917
sum(RM_trip_sumKB$count)
# Total captain striped bass releases on same trips: 2285 (46.5%)
(sum(RM_trip_sumKB$count)/sum(RM_sumKB$count))*100


####2020 vs 2021 SPOT KB####
##Look at onboard reports only for SPOT KB##
# ------------- #

# ------------- #
#loaddata
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RMdat <- rbind(RMdat2020, RMdat2021)
# -------------------- #

# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
Captaindat <- rbind(dat2020, dat2021)
# ------------- #


# fix names
names(Captaindat) = gsub(" ","", names(Captaindat))
names(Captaindat) = gsub("#","", names(Captaindat))
names(Captaindat) <- gsub("\\.", "", names(Captaindat))
names(RMdat) = gsub(" ","", names(RMdat))
names(RMdat) = gsub("#","", names(RMdat))
names(RMdat) <- gsub("\\.", "", names(RMdat))
names(RMdat) <- gsub("/", "", names(RMdat))

# filter to last trip reported
dat = Captaindat %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together
# ------------- #
##Look at onboard reports only for Spot kept for bait in 2020##
# ------------- #
obKB = RMdat %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

#filter for 2020 only
class(obKB$Date)
obKB$Date = as.Date(obKB$Date, "%d/%m/%Y")

obKB2020 <- obKB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))


#Number of onboard trips with Spot KB in 2020 (8)
length(unique(obKB2020$TripID))

#Number of 2020 captain reports with Spot KB during onboard trips (15) 
ob_tripKB = Captaindat %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

class(ob_tripKB$Date)
ob_tripKB$Date = as.Date(ob_tripKB$Date, "%d/%m/%Y")

ob_tripKB2020 <- ob_tripKB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

length(unique(ob_tripKB2020$TripID))

#Percentage of 2020 onboard trips with Spot KB where captains reported spot KB (62.5%)
(length(unique(ob_tripKB2020$TripID))/length(unique(obKB2020$TripID))*100)

#Compare number of SPOT KB from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumKB2020 = obKB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripKB2020_sum = ob_tripKB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalKB2020 = left_join(ob_sumKB2020, ob_tripKB2020_sum, by = "TripID")
names(TotalKB2020) = c("TripID", "ObserverKB", "CaptainKB")

#Total number of SPOT Reported as Kept for bait Onboard trips only (2020) 
sum(ob_sumKB2020$count) #onboard observer reported 392
sum(ob_tripKB2020_sum$count) #Captains reported 171 (43.6%)
(sum(ob_tripKB2020_sum$count)/sum(ob_sumKB2020$count))*100

####2021####
# ------------- #

#filter for 2021 only
class(obKB$Date)
obKB$Date = as.Date(obKB$Date, "%d/%m/%Y")

obKB2021 <- obKB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))


#Number of onboard trips with Spot KB in 2021 (8)
length(unique(obKB2021$TripID))

#Number of 2020 captain reports with SPOT kb during onboard trips (7) 
ob_tripKB = Captaindat %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

class(ob_tripKB$Date)
ob_tripKB$Date = as.Date(ob_tripKB$Date, "%d/%m/%Y")

ob_tripKB2021 <- ob_tripKB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

length(unique(ob_tripKB2021$TripID))

#Percentage of 2020 onboard trips with SB releases where captains also reported SB releases (87.5%)
(length(unique(ob_tripKB2021$TripID))/length(unique(obKB2021$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumKB2021 = obKB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripKB2021_sum = ob_tripKB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalKB2021 = left_join(ob_sumKB2021, ob_tripKB2021_sum, by = "TripID")
names(TotalKB2021) = c("TripID", "ObserverKB", "CaptainKB")

#Total number of SPOT Reported as Kept for bait Onboard trips only (2021) 
sum(ob_sumKB2021$count) #onboard observer reported 500
sum(ob_tripKB2021_sum$count) #Captains reported 472 (94.4%)
(sum(ob_tripKB2021_sum$count)/sum(ob_sumKB2021$count))*100

######Look at Roving monitor reports for spot kept for bait 2020 vs 2021######
# ------------- #
####2020####
RMKB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         SpeciesGrade %in% "SPOT")
length(unique(RMKB$TripID))

class(RMKB$Date)
RMKB$Date = as.Date(RMKB$Date, "%d/%m/%Y")

RMKB2020 <- RMKB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

#Number of RM trips with Spot KB in 2020 (38)
length(unique(RMKB2020$TripID))

#Number of 2020 captain reports with SPOT kb during RM trips (15) 
RM_tripKB = Captaindat %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

class(RM_tripKB$Date)
RM_tripKB$Date = as.Date(RM_tripKB$Date, "%d/%m/%Y")

RM_tripKB2020 <- RM_tripKB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

length(unique(RM_tripKB2020$TripID))

#Percentage of 2020 RM trips with spot KB where captains also reported SB releases (39.5%)
(length(unique(RM_tripKB2020$TripID))/length(unique(RMKB2020$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumKB2020 = RMKB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_tripKB2020_sum = RM_tripKB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
RMTotalKB2020 = left_join(RM_sumKB2020, RM_tripKB2020_sum, by = "TripID")
names(RMTotalKB2020) = c("TripID", "RMKB", "CaptainKB")

#Total number of SPOT Reported as Kept for bait RM trips only (2020) 
sum(RM_sumKB2020$count) #Roving Monitors reported 2635
sum(RM_tripKB2020_sum$count) #Captains reported 1115 (42.32%)
(sum(RM_tripKB2020_sum$count)/sum(RM_sumKB2020$count))*100


####2021####
RMKB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         SpeciesGrade %in% "SPOT")
length(unique(RMKB$TripID))

class(RMKB$Date)
RMKB$Date = as.Date(RMKB$Date, "%d/%m/%Y")

RMKB2021 <- RMKB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

#Number of RM trips with Spot KB in 2021 (35)
length(unique(RMKB2021$TripID))

#Number of 2021 captain reports with SPOT kb during RM trips (18) 
RM_tripKB = Captaindat %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

class(RM_tripKB$Date)
RM_tripKB$Date = as.Date(RM_tripKB$Date, "%d/%m/%Y")

RM_tripKB2021 <- RM_tripKB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

length(unique(RM_tripKB2021$TripID))

#Percentage of 2021 RM trips with spot KB where captains also reported SB releases (51.43%)
(length(unique(RM_tripKB2021$TripID))/length(unique(RMKB2021$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumKB2021 = RMKB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_tripKB2021_sum = RM_tripKB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
RMTotalKB2021 = left_join(RM_sumKB2021, RM_tripKB2021_sum, by = "TripID")
names(RMTotalKB2021) = c("TripID", "RMKB", "CaptainKB")

#Total number of SPOT Reported as Kept for bait Roving Monitor trips only (2021) 
sum(RM_sumKB2021$count) #Roving Monitors reported 2282
sum(RM_tripKB2021_sum$count) #Captains reported 1170 (51.27%)
(sum(RM_tripKB2021_sum$count)/sum(RM_sumKB2021$count))*100

#Anova
aovdata = mutate(RMKB2021, spot_releases2021 = select(SpeciesGrade, Count))

#Replace NA with 0
TotalReleases_RM[is.na(TotalReleases_RM)] <- 0

dat_aov_20202021spotRM = aov(TripID ~ RM_sumKB2021 + RM_tripKB2021, data = TotalReleases_RM)
summary(dat_aov_20202021spotRM)

dat_aov_obreports = aov(TripID ~ CaptainReleases + ObserverReleases, data = TotalReleases)
summary(dat_aov_obreports)

