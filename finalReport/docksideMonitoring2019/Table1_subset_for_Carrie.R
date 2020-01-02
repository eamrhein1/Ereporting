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
library(htmlTable)
library(tableHTML)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.in2 = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/Documentation/Resources for RMs/RM scheduling and priority lists/"
dir.in3 = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/temp/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/final_report_2019/"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
# regions
source("U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/code/importRegions.R")

# load fishing data
RM <- read_excel(paste(dir.in,"FACTSMD-684.xlsx", sep=""), sheet = 1)
WM <- read_excel(paste(dir.in,"FACTSMD-684.xlsx", sep=""), sheet = 2)

# rename
names(RM) = c("TripID","DNRID","MonitorReportNum","AssignedMonitor",
              "ReportedBy","SpeciesGrade","Quantity","Unit", "Count",          
              "Comments","Result","Scheduled","CrewCount","Time")
names(WM) = c("TripID","DNRID","WatermenName","License","Date",           
              "SH","EH","SHSubmittedTime","EHSubmittedTime","SHLandingTime",  
              "EHLandingTime","SHAddress","SHZip","EHAddress","EHZip",         
              "CrewCount","Fishery","Gear","SpeciesGrade","Quantity",
              "Unit", "Count")

# take spaces out of names
#names(WM) = gsub(" ", "", names(WM), fixed = TRUE)

# needs to be changed in the data
RM = RM %>% mutate(AssignedMonitor = replace(AssignedMonitor, TripID %in% c(565820, 569269, 569582, 574640, 
                                                                            578963, 569640, 569665, 579730,
                                                                            566638, 584714, 584748, 584813, 
                                                                            588244), "Becky Rusteberg K"),
                   AssignedMonitor = replace(AssignedMonitor, TripID %in% c(582379, 582924, 583278, 585968), "Steve Harris Womack"))
# correct data
RM$Quantity[RM$TripID %in% 596007 & RM$SpeciesGrade %in% "FEMALES"] = 16
RM$Quantity[RM$TripID %in% 596007 & RM$SpeciesGrade %in% "MIXED MALES"] = 2

# likely an error but same on the paper report so leaving as is
#RM$Quantity[RM$TripID %in% 606012 & RM$SpeciesGrade %in% "PEELERS"] = 20
#RM$Quantity[RM$TripID %in% 606012 & RM$SpeciesGrade %in% "SOFT SHELL"] = 2
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- # 
# join fishery and name to RM based on trip ID
RM = left_join(RM, dplyr::select(WM, TripID, Fishery, WatermenName, Date) %>% distinct, by = "TripID")

# add regions
WM = left_join(WM, mutate(zip_region_list, Zip = as.numeric(Zip)), by = c("EHZip" = "Zip")) %>% 
  mutate(region = replace(region, is.na(region), "undefined"))
RM = left_join(RM, dplyr::select(WM, TripID, region) %>% distinct, by = "TripID")

# attr(WM$Date, "tzone") <- "EST"
# attr(RM$Date, "tzone") <- "EST"
RM = mutate(RM, Date = as.Date(as.character(Date), format = "%Y-%m-%d"))
WM = mutate(WM, Date = as.Date(as.character(Date), format = "%Y-%m-%d"))

WM = WM %>% filter(Date <= "2019-12-15")
# -------------------- #

# create table
# subset for Jul1 - Dec 15
WM = WM %>% filter(Date >= "2019-7-1")
RM = RM %>% filter(Date >= "2019-7-1")

tripSummary = as.data.frame(matrix(data = NA, ncol=7, nrow=7))
names(tripSummary) = c("Regions","AvailTrips","AttemptedTrips","SuccessfulTrips","AvailWM","AttemptedWM","SuccessfulWM")
tripSummary$Regions = c("1","2","3","4","5","6","Total")
tripSummary[tripSummary$Regions %in% "Total",2:7] = c(prettyNum(length(unique(WM$TripID)), big.mark = ","), 
                                                      paste(formatC(length(unique(RM$TripID))/length(unique(WM$TripID))*100, digits = 3), "% (n = ", length(unique(RM$TripID)), ")", sep=""),
                                                      paste(formatC((length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED")]))/length(unique(WM$TripID)))*100, digits=3), 
                                                            "% (n = ", length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED")])), ")", sep=""),
                                                      length(unique(WM$WatermenName)), 
                                                      paste(formatC((length(unique(RM$DNRID))/length(unique(WM$DNRID)))*100, digits=4), 
                                                            "% (n = ", length(unique(RM$DNRID)), ")", sep=""),
                                                      paste(formatC((length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED")]))/length(unique(WM$DNRID)))*100, digits=4), 
                                                            "% (n = ",length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED")])), ")", sep="")) 

for(n in c(1:6)){
  tripSummary$AvailTrips[n] = prettyNum(length(unique(WM$TripID[WM$region %in% n])), big.mark = ",")
  tripSummary$AttemptedTrips[n] = paste(formatC(length(unique(RM$TripID[RM$region %in% n]))/length(unique(WM$TripID[WM$region %in% n]))*100, digits = 3), "% (n = ", length(unique(RM$TripID[RM$region %in% n])), ")", sep="")
  tripSummary$SuccessfulTrips[n] = paste(formatC((length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED") &RM$region %in% n]))/length(unique(WM$TripID[WM$region %in% n])))*100, digits=3), 
                                         "% (n = ", length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED") &RM$region %in% n])), ")", sep="")
  tripSummary$AvailWM[n] = length(unique(WM$DNRID[WM$region %in% n]))
  tripSummary$AttemptedWM[n] = paste(formatC((length(unique(RM$DNRID[RM$region %in% n]))/length(unique(WM$DNRID[WM$region %in% n])))*100, digits=4), 
                                     "% (n = ", length(unique(RM$DNRID[RM$region %in% n])), ")", sep="")
  tripSummary$SuccessfulWM[n] = paste(formatC((length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED") & RM$region %in% n]))/length(unique(WM$DNRID[WM$region %in% n])))*100, digits=4), 
                                      "% (n = ",length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED") & RM$region %in% n])), ")", sep="") 
}
rm(n)

xTable =  htmlTable(tripSummary, rnames = FALSE,
                    caption="Table 1. Trip Summary for Roving Monitors July 1 to December 15, 2019",
                    header =  c("Region",
                                "Total Available Trips",	
                                "Attempted Trips Monitored",	
                                "Successful Trips Monitored",	
                                "Number of Available Watermen",	
                                "Number of Individual Watermen Attempted to be Monitored",
                                "Number of Individual Watermen Successfully Monitored"),
                    n.rgroup = c(6,1),
                    align = "lc",
                    align.header = "lccc",
                    css.cell = rbind(rep("font-size: 1.1em; padding-right: 0.6em", 
                                         times=7), matrix("font-size: 1.1em; padding-right: 0.6em", ncol=7, nrow=7)),
                    css.table = "margin-top: 1em; margin-bottom: 1em; table-layout: fixed; width: 1000px",
                    total = "tspanner",
                    css.total = c("border-top: 1px solid grey; font-weight: 900"),
                    n.tspanner = c(nrow(tripSummary)))
xTable 

write.table(xTable, 
            file=paste(dir.out, "Table1_for_Carrie.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)
