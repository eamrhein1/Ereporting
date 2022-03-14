library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(lubridate)

# ------------- #
#loaddata
# ------------- #
RMdat <- read.csv("C:/Users/Eric Amrhein/Downloads/2021rmreports(2).csv")
Captaindat <- read.csv("C:/Users/Eric Amrhein/Downloads/2021chartertripsfinal.csv")

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
##Look at onboard reports only for Striped bass##
# ------------- #
obSB = RMdat %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS") %>%
  as.Date(obSB$Date, "%d/%m/%Y")

#filter for 2020 only
class(obSB$Date)
obSB$Date = as.Date(obSB$Date, "%d/%m/%Y")

obSB2020 <- obSB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

  
#Number of onboard trips with released SB in 2020 (21)
length(unique(obSB2020$TripID))

#Number of 2020 captain reports with released SB during onboard trips (15) 
ob_tripSB = Captaindat %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

class(ob_tripSB$Date)
ob_tripSB$Date = as.Date(ob_tripSB$Date, "%d/%m/%Y")

ob_tripSB2020 <- ob_tripSB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

length(unique(ob_tripSB2020$TripID))

#Percentage of 2020 onboard trips with SB releases where captains also reported SB releases (71%)
(length(unique(ob_tripSB2020$TripID))/length(unique(obSB2020$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumSB2020 = obSB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripSB2020_sum = ob_tripSB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases2020 = left_join(ob_sumSB2020, ob_tripSB2020_sum, by = "TripID")
names(TotalSBReleases2020) = c("TripID", "ObserverReleases", "CaptainReleases")

#Total number of Striped bass discards
sum(ob_sumSB2020$count) #onboard observer reported 230
sum(ob_tripSB2020_sum$count) #Captains reported 188 (81%)
(sum(ob_tripSB2020_sum$count)/sum(ob_sumSB2020$count))*100

##Onboard for 2021 SB Releases##
obSB2021 <- obSB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

#Number of OB trips with released SB in 2021
length(unique(obSB2021$TripID)) (31)

#Number of 2021 captain reports with released SB during onboard trips (19) 
ob_tripSB = Captaindat %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

ob_tripSB2021 <- ob_tripSB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

length(unique(ob_tripSB2021$TripID))

#Percentage of 2021 onboard trips with SB releases where captains also reported SB releases (61%)
(length(unique(ob_tripSB2021$TripID))/length(unique(obSB2021$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumSB2021 = obSB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripSB2021_sum = ob_tripSB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases2021 = left_join(ob_sumSB2021, ob_tripSB2020_sum, by = "TripID")
names(TotalSBReleases2021) = c("TripID", "ObserverReleases", "CaptainReleases")

#Total number of Striped bass discards
sum(ob_sumSB2021$count) #onboard observer reported 296
sum(ob_tripSB2021_sum$count) #Captains reported 156 (53%)
(sum(ob_tripSB2021_sum$count)/sum(ob_sumSB2021$count))*100


######Look at all reports for striped bass releases######
# ------------- #
##2020##
#Number of RM reports with SB releases in 2020 (133)
RMSB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         SpeciesGrade %in% "STRIPED BASS")

#Filter for 2020
class(RMSB$Date)
RMSB$Date = as.Date(RMSB$Date, "%d/%m/%Y")

RMSB2020 <- RMSB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

length(unique(RMSB2020$TripID))

#Monitored captain reports with SB releases (60)
RM_tripSB = Captaindat %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

#Filter for 2020
class(RM_tripSB$Date)
RM_tripSB$Date = as.Date(RM_tripSB$Date, "%d/%m/%Y")

RM_tripSB2020 <- RM_tripSB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

length(unique(RM_tripSB2020$TripID))

#Percentage of monitored trips with releases where captains also reported releases (45%)
(length(unique(RM_tripSB2020$TripID))/length(unique(RMSB2020$TripID))*100)

#Compare number of discards from RM reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumSB2020 = RMSB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_trip_sumSB2020 = RM_tripSB2020 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases_RM2020 = left_join(RM_sumSB, RM_trip_sumSB, by = "TripID")
names(TotalSBReleases_RM2020) = c("TripID", "RMReleases", "CaptainReleases")

#Total SB Releases from RM reports and Captain reports
sum(RM_sumSB2020$count)
# Total 2020 RM striped bass releases: 1542
sum(RM_trip_sumSB2020$count)
# Total captain striped bass releases on same trips: 623 (40%)
(sum(RM_trip_sumSB2020$count)/sum(RM_sumSB2020$count))*100

##2021##
#Number of RM reports with SB releases in 2021 (163)
RMSB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         SpeciesGrade %in% "STRIPED BASS")

#Filter for 2021
class(RMSB$Date)
RMSB$Date = as.Date(RMSB$Date, "%d/%m/%Y")

RMSB2021 <- RMSB %>%
  select(TripID, DNRID, MonitorReport, AssignedMonitor, ReportedBy, SpeciesGrade, Disposition, Quantity, Unit, Count, Comments, Result, Scheduled, AnglerCount, Date, TimeChecked, TimeLeft, TimeReturned, Onboard, Gear, GearAmount, GearMethod, Dependent, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

length(unique(RMSB2021$TripID))

#Monitored captain reports with SB releases (65)
RM_tripSB = Captaindat %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

#Filter for 2021
class(RM_tripSB$Date)
RM_tripSB$Date = as.Date(RM_tripSB$Date, "%d/%m/%Y")

RM_tripSB2021 <- RM_tripSB %>%
  select(TripID, DNRID, WatermenName, License, Date, SH, EH, SHSubmittedTime, EHSubmittedTime, SHLandingTime, EHLandingTime, SHAddress, SHZip, EHAddress, EHZip, AnglerCount, Fishery, Gear, GearAmount, GearMethod, Species, Disposition, Quantity, Unit, Count, SizeClass, ReasonforRelease, HookingPosition) %>%
  filter(between(Date, as.Date("2021-01-01"), as.Date("2021-12-31")))

length(unique(RM_tripSB2021$TripID))

#Percentage of monitored trips with releases where captains also reported releases (39%)
(length(unique(RM_tripSB2021$TripID))/length(unique(RMSB2021$TripID))*100)

#Compare number of discards from RM reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumSB2021 = RMSB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_trip_sumSB2021 = RM_tripSB2021 %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases_RM2021 = left_join(RM_sumSB, RM_trip_sumSB, by = "TripID")
names(TotalSBReleases_RM2021) = c("TripID", "RMReleases", "CaptainReleases")

#Total SB Releases from RM reports and Captain reports
sum(RM_sumSB2021$count)
# Total 2021 RM striped bass releases: 1519
sum(RM_trip_sumSB2021$count)
# Total captain striped bass releases on same trips: 635 (42%)
(sum(RM_trip_sumSB2021$count)/sum(RM_sumSB2021$count))*100

