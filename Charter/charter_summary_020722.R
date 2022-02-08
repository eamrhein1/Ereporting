# FACTS summary for quarterly report
# Aug 1 2021 - Jan. 31 2022 

# packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

# load data
# # trip data
# dat = read_csv("Library/CloudStorage/OneDrive-SharedLibraries-OysterRecoveryPartnership,Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_020722.csv")
# names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
# dat = mutate(dat, Date = as.POSIXct(Date, format = "%m/%d/%Y"),
#              mo = month(Date),
#              yr = year(Date)) %>%
#   filter(mo > 7, yr %in% 2021)
# 
# length(unique(dat$TripID))

# RM charter data
RM = read_csv("Library/CloudStorage/OneDrive-SharedLibraries-OysterRecoveryPartnership,Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_020722.csv")
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
names(RM) = gsub("#", "", names(RM), fixed = TRUE)

RM = RM %>% 
  dplyr::select(TripID, Date, Result, Onboard, AssignedMonitor, MonitorReport) %>% 
  group_by(TripID) %>%
  mutate(lastR = ifelse(MonitorReport == max(MonitorReport),"yes","no")) %>%
  filter(lastR %in% "yes") %>%
  dplyr::select(-lastR, -MonitorReport) %>%
  distinct() %>% ungroup() %>% 
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"),
         mo = month(Date),
         yr = year(Date)) %>%
  filter(mo > 7, yr %in% 2021)

length(unique(RM$TripID))

length(unique(RM$TripID[RM$Result %in% c("MONITORED", "MONITORED (on paper)")]))

RM %>% dplyr::select(TripID, mo, Result) %>%
  distinct() %>% #remove duplicate reports from updating
  filter(!Result %in% c("MONITORED", "MONITORED (on paper)")) %>% 
  group_by(Result) %>%
  summarise(n=n())

RM %>% 
  dplyr::select(TripID, mo, Result, Onboard) %>%
  filter(Onboard %in% "N") %>%
  distinct() %>% #remove duplicate reports from updating
  mutate(success = NA, 
         success = ifelse(Result %in% c("MONITORED", "MONITORED (on paper)"), "yes", "no")) %>% 
  group_by(mo, success) %>%
  summarise(n=n())

RM %>% 
  dplyr::select(TripID, mo, Result, Onboard) %>%
  filter(Onboard %in% "Y") %>%
  distinct() %>% #remove duplicate reports from updating
  mutate(success = NA, 
         success = ifelse(Result %in% c("MONITORED", "MONITORED (on paper)"), "yes", "no")) %>% 
  group_by(mo, success) %>%
  summarise(n=n())
