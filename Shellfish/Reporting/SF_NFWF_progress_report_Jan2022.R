# ---------- #
# summarize shellfish harvester and dealer data for progress report
# ---------- #


# ---------- #
# load packages
# ---------- #
library(dplyr)
library(ggplot)
library(readr)
library(readxl)
# ---------- #


# ---------- #
# load data
# ---------- #
buytickets <- read.csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishBuyTickets_122821.csv")
reports <- read.csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishMonitorReports_122821.csv")
trips <- read.csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishTrips_122821.csv")
trips = trips %>% filter(!Harvester.Name %in% "Test Fisher")
#trips[trips$Harvester.Name %in% "Test Fisher",]
#outreach <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Shellfish/Reporting/Progress Reports/SFCandidates3_Dec2021.xlsx", skip = 1)
outreach <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Shellfish/Reporting/Progress Reports/SFCandidates3_Dec2021.xlsx", 
                       sheet = "the 100 outreach", skip = 1)
names(outreach) = gsub(" ","",names(outreach))
# ---------- #


# ---------- #
# summarize data
# ---------- #
# number of buy tickets
length(unique(buytickets$TICKET.NUMBER))
# number of dealers
length(unique(buytickets$DEALER.NAME))
# number of dealers who are also harvesters
length(unique(trips$Harvester.Name[which(trips$Harvester.Name == buytickets$DEALER.NAME)]))


# number of trips
length(unique(trips$Trip.ID))
# number of harvesters
length(unique(trips$DNR.ID))


# number of roving monitor reports
length(unique(reports$Trip.ID))
# number of successful reports
length(unique(reports$Trip.ID[reports$Result %in% c("Monitored", "Monitored (on paper)")]))
(length(unique(reports$Trip.ID[reports$Result %in% c("Monitored", "Monitored (on paper)")]))/length(unique(reports$Trip.ID)))*100
(length(unique(reports$Trip.ID[reports$Result %in% c("Monitored", "Monitored (on paper)")]))/length(unique(trips$Trip.ID)))*100
# reasons why unsuccessful
reports %>% 
  group_by(Result) %>%
  summarize(n=n())

# outreach
outreach_made = outreach %>% dplyr::select(Name, Datecontacted) %>% 
  mutate(attempts = unlist(lapply(strsplit(Datecontacted,","), length)))
sum(outreach_made$attempts)
length(unique(outreach$Name[!is.na(outreach$Datecontacted)]))

# ---------- #
