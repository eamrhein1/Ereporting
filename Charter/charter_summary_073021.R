# FACTS summary for quarterly report
# May 1 - July 30, 2021 (am)

# packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

# dir
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/"

# load data
# trip data
dat = read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_073021.csv")
names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
dat = mutate(dat, Date = as.POSIXct(Date, format = "%m/%d/%Y"),
             mo = month(Date),
             yr = year(Date)) %>%
  filter(yr %in% 2021)

length(unique(dat$TripID))

# RM charter data
RM = read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_073021.csv")
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
RM = RM %>% dplyr::select(TripID, Date, Result, Onboard) %>% distinct() %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"),
         mo = month(Date),
         yr = year(Date)) %>%
  filter(yr %in% 2021)

length(unique(RM$TripID))

length(unique(RM$TripID[RM$Result %in% c("MONITORED", "MONITORED (on paper)")]))

RM %>% filter(!Result %in% c("MONITORED", "MONITORED (on paper)")) %>% 
  group_by(Result) %>%
  summarise(n=n())

# on board trips
O = RM %>% filter(Onboard %in% "Y") %>%
  distinct() 
length(unique(O$TripID))
length(unique(O$TripID[O$Result %in% c("MONITORED", "MONITORED (on paper)")]))

RM %>% filter(Onboard %in% "Y", Result %in% c("MONITORED", "MONITORED (on paper)")) %>%
  dplyr::select(TripID, Result) %>% 
  distinct() %>% 
  summarise(n=n())
