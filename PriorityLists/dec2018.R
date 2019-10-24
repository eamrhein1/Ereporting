# Nov 2018 data to plan where to cut time

# ------------------------- #
# upload packages
# ------------------------- #
require(dplyr)
require(ggplot2)
require(readxl)
library(chron)
library(reshape2)
library(htmlTable)
library(tableHTML)
library(webshot)
library(lubridate)
# ------------------------- #

# ------------------------- #
# import data
# ------------------------- #
dir.in = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/all2018data/"

WMdata_withDups <- read_excel(paste(dir.in, "WatermenData041819.xlsx", sep=""), sheet = 1)

# get rid of spaces in names
# issues with some names -> remove numbers (since duplicate names in different tables and only copied from output instead of talking with database directly)
names(WMdata_withDups) = gsub(" ", "", names(WMdata_withDups), fixed = TRUE)
# ------------------------- #


# ------------------------- #
# duplicate trips
# ------------------------- #
# look at hail information
# duplicated records mean change in hail
dupTripID = sort(unique(WMdata_withDups$TripID[duplicated(WMdata_withDups$TripID)]))
length(dupTripID)

dupTrips = filter(WMdata_withDups, TripID %in% dupTripID)

group_by(dupTrips, TripID) %>% summarise(n=n()) %>% filter(!n %in% 2)

lastDupTrip = dupTrips %>% 
  group_by(TripID) %>% 
  summarise(TripDate = last(TripDate),
            FisherName = last(FisherName),
            Address = last(Address),
            City = last(City),
            Zip = last(Zip),
            StartHailDate = last(StartHailDate),
            StartHailTime = last(StartHailTime),
            TimeProposedforLanding = last(TimeProposedforLanding),
            EndHailDate = last(EndHailDate),
            EndHailTime = last(EndHailTime),
            License = last(License),
            VRN = last(VRN),
            Crew = last(Crew),
            CheckedIn = last(CheckedIn),
            CheckStation = last(CheckStation),
            NOAACode = last(NOAACode),
            ActualLandingTime = last(ActualLandingTime),
            GearType = last(GearType),
            NumberofSets = last(NumberofSets),
            CountofNets = last(CountofNets),
            SoakTime = last(SoakTime),
            Species = last(Species),
            Disposition = last(Disposition),
            Quantity = last(Quantity),
            Count = last(Count))

WMdata = filter(WMdata_withDups, !TripID %in% dupTripID) 
WMdata = bind_rows(WMdata, lastDupTrip)

WMdata = WMdata %>% 
  mutate(ActualLandingTime = sapply(strsplit(as.character(ActualLandingTime), " "), tail, 1),
         TimeProposedforLanding = sapply(strsplit(as.character(TimeProposedforLanding), " "), tail, 1),
         EndHailTime = sapply(strsplit(as.character(EndHailTime), " "), tail, 1),
         StartHailTime = sapply(strsplit(as.character(StartHailTime), " "), tail, 1),
         EndHailDateTime = as.POSIXct(paste(EndHailDate, EndHailTime, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
         StartHailDateTime = as.POSIXct(paste(StartHailDate, StartHailTime, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
         proposedLandingDateTime = as.POSIXct(paste(TripDate, TimeProposedforLanding, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
         actualLandingDateTime = as.POSIXct(paste(TripDate, ActualLandingTime, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
         ActualLandingTime = strftime(actualLandingDateTime, format="%H:%M:%S"),
         TimeProposedforLanding = strftime(proposedLandingDateTime, format="%H:%M:%S"),
         StartHailTime = strftime(StartHailDateTime, format="%H:%M:%S"),
         EndHailTime = strftime(EndHailDateTime, format="%H:%M:%S"),
         timeDiffHours = difftime(actualLandingDateTime, EndHailDateTime, unit = "hours"),
         timeDiffDays = difftime(actualLandingDateTime, EndHailDateTime, unit = "days"),
         timeDiffMins = difftime(actualLandingDateTime, EndHailDateTime, unit = "mins"),
         timeDiffStartEndMin = difftime(StartHailDateTime, EndHailDateTime, unit = "mins"),
         FisherName = toupper(FisherName),
         fishery = NA, 
         fishery = ifelse(GearType %in% c("CRAB POTS","TROTLINES","CRAB POUNDS/BANK TRAPS",
                                          "SCRAPES/DREDGES","PEELER POTS"),"Crab","FinFish"),
         fishery = replace(fishery, GearType %in% "POTS - TURTLE","Turtle"),
         mo = as.numeric(format(actualLandingDateTime, format="%m")),
         yr = as.numeric(format(actualLandingDateTime, format="%Y")),
         hr = as.numeric(format(actualLandingDateTime, format="%H")),
         end_hr = as.numeric(format(EndHailDateTime, format="%H"))) %>%
  filter(yr %in% 2018)

rm(lastDupTrip, WMdata_withDups, dupTrips)
# ------------------------- #

# ------------------------- #
# processing
# ------------------------- #
dec = WMdata %>% filter(mo %in% 12)
# ------------------------- #

# ------------------------- #
# landing day
# ------------------------- #
x_root = dec %>% 
  mutate(weekday = weekdays(TripDate),
         index = NA,
         index = replace(index, weekday %in% "Monday",1),
         index = replace(index, weekday %in% "Tuesday",2),
         index = replace(index, weekday %in% "Wednesday",3),
         index = replace(index, weekday %in% "Thursday",4),
         index = replace(index, weekday %in% "Friday",5),
         index = replace(index, weekday %in% "Saturday",6),
         index = replace(index, weekday %in% "Sunday",7)) %>% 
  arrange(index)

x = x_root %>%
  group_by(weekday) %>% 
  summarise(n=n(),index=first(index)) %>% 
  arrange(index)
total = sum(x$n)

x2 = x_root %>%
  filter(GearType %in% c("POUND NET - FISH", "POTS - EEL", "TROTLINES", 
                         "CRAB POTS", "SCRAPES/DREDGES")) %>%
  group_by(weekday) %>% 
  summarise(n=n(),index=first(index)) %>% 
  arrange(index)
total2 = sum(x2$n)

x3 =x_root %>%
  filter(fishery %in% "Crab") %>% 
  group_by(weekday) %>% 
  summarise(n=n(),index=first(index)) %>% 
  arrange(index)
total3 = sum(x3$n)

x4 =x_root %>%
  filter(fishery %in% "FinFish") %>% 
  group_by(weekday) %>% 
  summarise(n=n(),index=first(index)) %>% 
  arrange(index)
total4 = sum(x4$n)

x5 =x_root %>%
  filter(fishery %in% "FinFish",
         GearType %in% c("POUND NET - FISH", "POTS - EEL", "GILL NET - DRIFT", 
                         "HOOK AND LINE", "COLLAPSIBLE TRAPS")) %>% 
  group_by(weekday) %>% 
  summarise(n=n(),index=first(index)) %>% 
  arrange(index)
total5 = sum(x5$n)

cbind(x$weekday, 
      format(round((x$n/total)*100,3),nsmall = 3),
      format(round((x2$n/total2)*100,3),nsmall = 3),
      format(round((x3$n/total)*100,3),nsmall = 3),
      format(round((x3$n/total3)*100,3),nsmall = 3),
      format(round((x4$n/total)*100,3),nsmall = 3),
      format(round((x4$n/total4)*100,3),nsmall = 3),
      format(round((x5$n/total5)*100,3),nsmall = 3))

weekdays = as.data.frame(matrix(nrow = 7, ncol = 1, data = x$weekday)) %>% 
  rename(weekday = V1) 

xTotals = weekdays %>% 
  left_join(., mutate(x, n = format(round((n/total)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x2, n = format(round((n/total2)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x3, n = format(round((n/total)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x3, n = format(round((n/total3)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x4, n = format(round((n/total)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x4, n = format(round((n/total4)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday") %>%
  left_join(., mutate(x5, n = format(round((n/total5)*100, 3), nsmall=3)) %>% dplyr::select(-index), by = "weekday")

names(xTotals) = c("Landing Day", 
                   "Percent of trips landed",
                   "Percent of trips landed for top 5 gear types",
                   "Percent of trips landed for crab gear types out of total trips",
                   "Percent of trips landed for crab gear types out of crab trips",
                   "Percent of trips landed for finfish gear types out of total trips", 
                   "Percent of trips landed for finfish gear types out of finfish trips", 
                   "Percent of trips landed for top 5 finfish gear types out of top 5 finfish trips")


xTable = tableHTML(xTotals, rownames = FALSE, widths = rep(140,8),
                   caption = "Summary of trips for Dec. 2018 by landing day.",
                   footer = txtMergeLines("<sup>&dagger;</sup>top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges",
                                          "<sup>&Dagger;</sup>top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps")) %>%
  add_css_thead(css = list(c('font-size', 'text-align'), c('25px','left'))) %>%
  add_css_tbody(css = list(c('font-size', 'text-align'), c('30px','left'))) %>%
  add_css_footer(css = list(c('font-size', 'text-align'), c('20px', 'left'))) %>%
  add_css_caption(css = list(c('font-size', 'text-align'), c('30px', 'left'))) %>%
  add_theme('scientific') %>% 
  add_css_conditional_column(conditional = "color_rank", color_rank_theme = "RAG", columns = 6, decreasing = TRUE) %>%
  add_css_conditional_column(conditional = "color_rank", color_rank_theme = "RAG", columns = c(2,3,4,5,7,8), decreasing = TRUE)

write.table(xTable, 
            file=paste(dir.out, "LandingDaysTableColored.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)
# ------------------------- #

# ------------------------- #
# landing hours in time blocks
# 7 to 13
# 11 to 17
# 15 to 21
# ------------------------- #
x = dec %>% filter(GearType %in% c("POUND NET - FISH", "POTS - EEL", "TROTLINES", 
                                   "CRAB POTS", "SCRAPES/DREDGES")) %>%
  dplyr::select(TripID, hr) %>%
  mutate(block = "Not in any block", 
         block = replace(block, hr %in% c(7,8,9,10,11,12,13), "7 am to 1 pm"),
         block = replace(block, hr %in% c(11,12,13,14,15,16,17), "11 am to 5 pm"),
         block = replace(block, hr %in% c(15,16,17,18,19,20,21), "3 pm to 9 pm")) %>% 
  group_by(block) %>%
  summarise(n=n())
total = sum(x$n)
to.add = x[1,] %>% mutate(block = "In a block", n = sum(x$n[!x$block %in% "Not in any block"]))
x = bind_rows(x, to.add)

# x2 = nov %>% filter(fishery %in% "Crab") %>%
#   dplyr::select(TripID, hr) %>%
#   mutate(block = "Not in any block", 
#          block = replace(block, hr %in% c(7,8,9,10,11,12,13), "7 am to 1 pm"),
#          block = replace(block, hr %in% c(11,12,13,14,15,16,17), "11 am to 5 pm"),
#          block = replace(block, hr %in% c(15,16,17,18,19,20,21), "3 pm to 9 pm")) %>% 
#   group_by(block) %>%
#   summarise(n=n()) 
# total2 = sum(x2$n)
# to.add = x2[1,] %>% mutate(block = "In a block", n = sum(x2$n[!x2$block %in% "Not in any block"]))
# x2 = bind_rows(x2, to.add)

x3 = dec %>% filter(fishery %in% "FinFish",
                    GearType %in% c("POUND NET - FISH", "POTS - EEL", "GILL NET - DRIFT", 
                                    "HOOK AND LINE", "COLLAPSIBLE TRAPS")) %>%
  dplyr::select(TripID, hr) %>%
  mutate(block = "Not in any block", 
         block = replace(block, hr %in% c(7,8,9,10,11,12,13), "7 am to 1 pm"),
         block = replace(block, hr %in% c(11,12,13,14,15,16,17), "11 am to 5 pm"),
         block = replace(block, hr %in% c(15,16,17,18,19,20,21), "3 pm to 9 pm")) %>% 
  group_by(block) %>%
  summarise(n=n())
total3 = sum(x3$n)
to.add = x3[1,] %>% mutate(block = "In a block", n = sum(x3$n[!x3$block %in% "Not in any block"]))
x3 = bind_rows(x3, to.add)

x4 = dec %>% filter(fishery %in% "FinFish") %>%
  dplyr::select(TripID, hr) %>%
  mutate(block = "Not in any block", 
         block = replace(block, hr %in% c(7,8,9,10,11,12,13), "7 am to 1 pm"),
         block = replace(block, hr %in% c(11,12,13,14,15,16,17), "11 am to 5 pm"),
         block = replace(block, hr %in% c(15,16,17,18,19,20,21), "3 pm to 9 pm")) %>% 
  group_by(block) %>%
  summarise(n=n())
total4 = sum(x4$n)
to.add = x4[1,] %>% mutate(block = "In a block", n = sum(x4$n[!x4$block %in% "Not in any block"]))
x4 = bind_rows(x4, to.add)


blocks = as.data.frame(matrix(nrow = 5, ncol = 1, data = x3$block)) %>% 
  rename(block = V1) %>% mutate(index = c(2,3,1,5,4)) %>% arrange(index) %>% dplyr::select(-index)

xTotals = blocks %>% 
  left_join(., mutate(x, n = format(round((n/total)*100, 3), nsmall=3)), by = "block") %>%
  #left_join(., mutate(x2, n = format(round((n/total2)*100, 3), nsmall=3)), by = "block") %>%
  left_join(., mutate(x3, n = format(round((n/total3)*100, 3), nsmall=3)), by = "block") %>%
  left_join(., mutate(x4, n = format(round((n/total4)*100, 3), nsmall=3)), by = "block") 

names(xTotals) = c("Time block",
                   "For all trips in the top 5 gear types",
                   "For all finfish trips in the top 5 gear types",
                   "For all finfish")

xTable = htmlTable(xTotals, rnames = FALSE,
                   caption="Percent of trips for Dec. 2018 by time block",
                   header =  c("Time block",
                               "Percent of all trips in the top 5 gear types<sup>&dagger;</sup>",
                               "Percent of all finfish trips in the top 5 gear types<sup>&Dagger;</sup>",
                               "Percent of all finfish"),
                   rgroup = c("By block",
                              "Summary"),
                   n.rgroup = c(3,2),
                   align = "lc",
                   align.header = "lccc",
                   css.cell = rbind(rep("font-size: 1.3em; padding-right: 0.5em", 
                                        times=4), matrix("font-size: 1.2em; padding-right: 0.5em", ncol=4, nrow=5)),
                   css.rgroup = "font-weight: 800; font-size: 1.2em;", 
                   css.rgroup.sep = "",
                   css.table = "margin-top: 1em; margin-bottom: 1em; table-layout: fixed; width: 800px;", 
                   tfoot = txtMergeLines("<sup>&dagger;</sup>Top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges.",
                                         "<sup>&Dagger;</sup>Top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps."))


write.table(xTable, 
            file=paste(dir.out, "PercentTimeBlocks.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)
# ------------------------- #
