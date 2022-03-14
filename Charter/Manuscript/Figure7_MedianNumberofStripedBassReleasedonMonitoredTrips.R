library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)

# ------------- #
#loaddata
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
#Look at onboard reports only for Striped bass
# ------------- #
obSB = RMdat %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

#Number of onboard trips with released SB (52)
length(unique(obSB$TripID))

#Number of captain reports with released SB during onboard trips (34) 
ob_tripSB = Captaindat %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")
length(unique(ob_tripSB$TripID))

#Percentage of onboard trips with SB releases where captains also reported SB releases (65%)
(length(unique(ob_tripSB$TripID))/length(unique(obSB$TripID))*100)

#Compare number of SB discards from Onboard reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
ob_sumSB = obSB %>% group_by(TripID) %>% summarize(count = sum(Count))
ob_tripSB_sum = ob_tripSB %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases = left_join(ob_sumSB, ob_tripSB_sum, by = "TripID")
names(TotalSBReleases) = c("TripID", "ObserverReleases", "CaptainReleases")

#Total number of Striped bass discards
sum(ob_sumSB$count) #onboard observer reported 526
sum(ob_tripSB_sum$count) #Captains reported 344 (65%)
(sum(ob_tripSB_sum$count)/sum(ob_sumSB$count))*100

# #Look at mean difference and standard deviation of difference
TotalSBReleases %>% rowwise() %>% 
  mutate(CaptainReleases = replace(CaptainReleases, is.na(CaptainReleases), 0),
         diff = ObserverReleases - CaptainReleases) %>%
  ungroup() %>%
  summarise(mean_diff = mean(diff), var_diff = var(diff), sd_diff = sd(diff))

######Look at all reports for striped bass releases######
# ------------- #
#Number of RM reports with SB releases (291)
RMSB = RMdat %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         SpeciesGrade %in% "STRIPED BASS")
length(unique(RMSB$TripID))

#Monitored captain reports with SB releases (125)
RM_tripSB = Captaindat %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")
length(unique(RM_tripSB$TripID))

#Percentage of monitored trips with releases where captains also reported releases (42.5%)
(length(unique(RM_tripSB$TripID))/length(unique(RMSB$TripID))*100)

#Compare number of discards from RM reports to captain reports
#Join by trip ID and set up dataset comparing total releases reported
RM_sumSB = RMSB %>% group_by(TripID) %>% summarize(count = sum(Count))
RM_trip_sumSB = RM_tripSB %>% group_by(TripID) %>% summarize(count = sum(Count))
TotalSBReleases_RM = left_join(RM_sumSB, RM_trip_sumSB, by = "TripID")
names(TotalSBReleases_RM) = c("TripID", "RMReleases", "CaptainReleases")

#Total SB Releases from RM reports and Captain reports
sum(RM_sumSB$count) 
# Total RM striped bass releases: 3092
sum(RM_trip_sumSB$count)
# Total captain striped bass releases on same trips: 1258 (41.1%)
(sum(RM_trip_sumSB$count)/sum(RM_sumSB$count))*100

#Look at mean difference and standard deviation of difference
TotalSBReleases_RM %>% rowwise() %>%
  mutate(CaptainReleases = replace(CaptainReleases, is.na(CaptainReleases), 0),
         diff = RMReleases - CaptainReleases) %>%
  ungroup() %>%
  summarize(mean_diff = mean(diff), var_diff = var(diff), sd_diff = sd(diff))

#Boxplot
#RMSB_sum = RMSB %>% filter(Onboard == "N") %>% group_by(TripID) %>% summarize(count = sum(Count)) # remove OB from RM
allSB_release_summary = rbind(RM_sumSB %>% mutate(type = "Roving Monitor"),
                              RM_trip_sumSB %>% mutate(type  = "Captain"),
                              ob_sumSB %>% mutate(type = "Onboard Observer"))
p4 = ggplot() + 
  geom_boxplot(data = allSB_release_summary, aes(x = type, y = count, fill = type)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 20), legend.position = "none") +
  labs(x="Reporter",y="Release Count (Striped Bass)") +
  ggtitle("Median Number of SB Reported as Released on Monitored Trips") 
p4

sums = as.data.frame(rbind(c("Reported by Monitor","Roving Monitors", sum(RM_sumSB$count)), # Total RM striped bass releases: 3061
                           c("Reported by Captain","Roving Monitors", sum(RM_trip_sumSB$count)), # Total captain striped bass releases on same trips: 1258 (41%)
                           c("Reported by Monitor","Onboard Observer",sum(ob_sumSB$count)), #onboard observer reported 526
                           c("Reported by Captain","Onboard Observer",sum(ob_tripSB_sum$count)))) #Captains reported 344 
names(sums) = c("ReportedBy","MonitorType","Releases")
sums = mutate(sums, Releases = as.numeric(Releases))  

p5 = ggplot() + 
  geom_bar(data = sums, aes(x = MonitorType, y = Releases, fill = ReportedBy), position = "dodge", stat="identity") + 
  geom_text(data = sums, aes(x = MonitorType, y = Releases, label = Releases, position = "dodge"), check_overlap = TRUE, hjust = 1, nudge_y = 50) + 
  labs(x = "Monitor Type", y = "Sum of Striped Bass Released", title = "Sum of Striped Bass Released on Monitored Trips 2020-2021", fill = " ") + 
  scale_fill_manual(values = c("Reported by Captain" = "steelblue1", "Reported by Monitor" = "navy")) + 
  theme_bw() + 
  theme(legend.position="top") 
p5
