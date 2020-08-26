# FACTS summary for quarterly report
# February 1 - July 31, 2020

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
dat = read_excel("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/FACTSMD-851.xls")
names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
dat = mutate(dat, Date = as.POSIXct(Date, format = "%m/%d/%Y"),
             mo = month(Date)) %>%
  filter(mo %in% c(2,3,4,5,6,7),
         Fishery %in% "Charter")

length(unique(dat$TripID))

# RM charter data
RM = read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_080620.csv")
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
RM = RM %>% dplyr::select(TripID, Date, Result) %>% distinct() %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"),
         mo = month(Date)) %>%
  filter(mo %in% c(6,7))

length(unique(RM$TripID))

length(unique(RM$TripID[RM$Result %in% c("MONITORED", "MONITORED (on paper)")]))

RM %>% filter(!Result %in% c("MONITORED", "MONITORED (on paper)")) %>% 
  group_by(Result) %>%
  summarise(n=n())
