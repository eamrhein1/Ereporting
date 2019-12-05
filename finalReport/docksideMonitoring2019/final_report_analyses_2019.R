# -------------------- # 
# This script is for the roving monitor final report tables and figures
# -------------------- # 

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
library(utf8) #unsure whats up with this
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.in2 = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/Documentation/Resources for RMs/RM scheduling and priority lists/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/final_report_2019/"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
# load fishing data
RM <- read_excel(paste(dir.in,"FACTSMD-684.xlsx", sep=""), sheet = 1)
WM <- read_excel(paste(dir.in,"FACTSMD-684.xlsx", sep=""), sheet = 2)

# rename
names(RM) = c("TripID","MonitorReportNum","AssignedMonitor",
              "ReportedBy","SpeciesGrade","Quantity","Unit",           
              "Comments","Result","Scheduled","CrewCount","Time")

# take spaces out of names
names(WM) = gsub(" ", "", names(WM), fixed = TRUE)

# needs to be changed in the data
RM = RM %>% mutate(AssignedMonitor = replace(AssignedMonitor, TripID %in% c(565820, 569269, 569582, 574640, 
                                                                            578963, 569640, 569665, 579730,
                                                                            566638, 584714, 584748, 584813, 
                                                                            588244), "Becky Rusteberg K"),
                   AssignedMonitor = replace(AssignedMonitor, TripID %in% c(582379, 582924, 583278, 585968), "Steve Harris Womack"))
# -------------------- #

# -------------------- #
# manipulate data
# -------------------- # 
# -------------------- #

# -------------------- #
# basic stats
# -------------------- #
# number of RM trips
length(unique(RM$TripID))

# number of WM trips
length(unique(WM$TripID))

# portion of trips monitored
(length(unique(RM$TripID))/length(unique(WM$TripID)))*100

# join fishery to RM based on trip ID
RM = left_join(RM, dplyr::select(WM, TripID, Fishery) %>% distinct, by = "TripID")

# portion of trips monitored for FF
(length(unique(RM$TripID[RM$Fishery %in% "Finfish"]))/length(unique(WM$TripID[WM$Fishery %in% "Finfish"])))*100
length(unique(WM$TripID[WM$Fishery %in% "Finfish"]))
length(unique(RM$TripID[RM$Fishery %in% "Finfish"]))

# portion of trips monitored for BC
(length(unique(RM$TripID[RM$Fishery %in% "Blue Crab"]))/length(unique(WM$TripID[WM$Fishery %in% "Blue Crab"])))*100
length(unique(WM$TripID[WM$Fishery %in% "Blue Crab"]))
length(unique(RM$TripID[RM$Fishery %in% "Blue Crab"]))

# successfully monitored
# be careful of reports where they were edited but show both results in data
SuccessTbl = RM %>% dplyr::select(Result, TripID) %>% distinct 
x = RM[RM$TripID %in% SuccessTbl$TripID[duplicated(SuccessTbl$TripID)],]

RM %>% dplyr::select(Result, TripID, MonitorReportNum) %>% distinct %>% 
  mutate(TripNum = paste(TripID, MonitorReportNum, sep="_")) %>%
  filter(!TripNum %in% paste(x$TripID, 1, sep="_")) %>% 
  mutate(Success = ifelse(Result %in% c("MONITORED","MONITORED (on paper)"),"Success","Fail")) %>% 
  group_by(Success) %>% summarize(n=n()) %>%
  mutate(perc = (n/(length(unique(RM$TripID))))*100)


# composed of __ % high, ___% medium, ___% low priority watermen.
BCP_OctDec <- read_excel(paste(dir.in2,"ECrabPriority Oct-Dec.xlsx", sep=""))
FFP_OctDec <- read_excel(paste(dir.in2,"EFishPriority Oct- Dec.xlsx", sep=""))
R1P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sep=""))
R2P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""))
R3P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region3_MaySept.xlsx", sep=""))
R4P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region4_MaySept.xlsx", sep=""))
R5P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region5_MaySept.xlsx", sep=""))
R6P <- read_excel(paste(dir.in2,"Roving_Monitor_Priority_All_Lists_Region6_MaySept.xlsx", sep=""))

# by region
# Roving monitors attempted to monitor ___ trips in region 1 (n=__ available trips), 
# ___ trips in region 2 (n = __ available trips_, 
# ___trips in region 3 (n=___ available trips), 
# ___ trips in region 4 (n=__ available trips), 
# ___ trips in region 5 (n=__ available trips), 
# and ___ trips in regions 6 (n=___available trips). 
# -------------------- #

# -------------------- #
# catch comparison
# -------------------- #

# -------------------- #
