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
reports = reports %>%
  group_by(TripID) %>%
  mutate(lastR = ifelse(MonitorReport == max(MonitorReport),"yes","no")) %>%
  filter(lastR %in% "yes") %>%
  dplyr::select(-lastR) 
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

###### Species Weight

###### Releases


# -------------------- #

