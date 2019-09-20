# -------------------- # 
# this script is to determine where to split R1 efforts to sustain two monitors
# -------------------- # 

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/list_of_watermen_to_contact_Aug2019"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
# load fishing data
RM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 1)
WM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 2)

# take spaces out of names
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
names(WM) = gsub(" ", "", names(WM), fixed = TRUE)

# needs to be changed in the data
RM = RM %>% mutate(AssignedMonitor = replace(AssignedMonitor, TripID %in% c(565820, 569269, 569582, 574640, 
                                                                            578963, 569640, 569665, 579730,
                                                                            566638, 584714, 584748, 584813, 
                                                                            588244), "Becky Rusteberg K"),
                   AssignedMonitor = replace(AssignedMonitor, TripID %in% c(582379, 582924, 583278, 585968), "Steve Harris Womack"))
# -------------------- # 

# -------------------- #
# create list of watermen from not monitored reports
# -------------------- #
r = RM %>% group_by(TripID) %>% summarise(date = first(AssignedMonitor), 
                                          type = first(Result),
                                          comments = first(Comments))
w = WM %>% group_by(TripID) %>% summarise(date = first(Date), 
                                          wm = first(WatermenName))
rw = left_join(r,w,by="TripID")
names(rw) = c("tripid","rm","type","comments","date","wm")
rm(r,w)

nm = rw %>% filter(!type %in% c("MONITORED","MONITORED (on paper)"),
                   !is.na(wm))
write.csv(nm, paste(dir.out, "not_monitored_details.csv",sep="/"), row.names = FALSE)

WM_nm = filter(WM, TripID %in% rw$tripid)
write.csv(WM_nm, paste(dir.out, "watermen_not_monitored_details.csv",sep="/"), row.names = FALSE)

m = rw %>% filter(type %in% c("MONITORED","MONITORED (on paper)"),
                   !is.na(wm))
write.csv(m, paste(dir.out, "monitored_details.csv",sep="/"), row.names = FALSE)

WM_m = filter(WM, TripID %in% rw$tripid)
write.csv(WM_m, paste(dir.out, "watermen_monitored_details.csv",sep="/"), row.names = FALSE)
# -------------------- #
