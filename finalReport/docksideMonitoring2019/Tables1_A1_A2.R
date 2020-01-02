# all 2018 data for planning

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
# ------------------------- #

# ------------------------- #
# import data
# ------------------------- #
dir.in = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/rawdata/"
dir.out = "U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/output/all2018data/"

WMdata_withDups <- read_excel(paste(dir.in, "WatermenData041819.xlsx", sep=""), sheet = 1)

# get rid of spaces in names
# issues with some names -> remove numbers (since duplicate names in different tables and only copied from output instead of talking with database directly)
names(WMdata_withDups) = gsub(" ", "", names(WMdata_withDups), fixed = TRUE)

source("U:/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Roving Monitor Pilot/code/importRegions.R")
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
# landing day
# ------------------------- #
x_root = WMdata %>% 
  filter(!mo %in% c(1,2,3,4)) %>% 
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

weekdays = as.data.frame(matrix(nrow = 7, ncol = 1, data = x2$weekday)) %>% 
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
                   caption = "Table A2. Summary of trips from May to December in 2018 by landing day.",
                   footer = txtMergeLines("<sup>&dagger;</sup>top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges",
                                          "<sup>&Dagger;</sup>top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps")) %>%
  add_css_thead(css = list(c('font-size', 'text-align'), c('25px','left'))) %>%
  add_css_tbody(css = list(c('font-size', 'text-align'), c('30px','left'))) %>%
  add_css_footer(css = list(c('font-size', 'text-align'), c('20px', 'left'))) %>%
  add_css_caption(css = list(c('font-size', 'text-align'), c('30px', 'left'))) %>%
  add_theme('scientific') %>% 
  add_css_conditional_column(conditional = "color_rank", color_rank_theme = "White-Blue", columns = 6, decreasing = TRUE) %>%
  add_css_conditional_column(conditional = "color_rank", color_rank_theme = "White-Blue", columns = c(2,3,4,5,7,8), decreasing = TRUE)

write.table(xTable, 
            file=paste(dir.out, "LandingDaysTableColored.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

# ------------------------- #
# landing hours in time blocks
# 7 to 13
# 11 to 17
# 15 to 21
# ------------------------- #

x = WMdata %>% filter(!mo %in% c(1,2,3,4)) %>% 
  group_by(hr) %>% summarise(n=n()) %>% arrange(hr)
total = dim(WMdata %>% filter(!mo %in% c(1,2,3,4)))[1]

x2 = WMdata %>% filter(!mo %in% c(1,2,3,4),
                       GearType %in% c("POUND NET - FISH", "POTS - EEL", "TROTLINES", 
                                       "CRAB POTS", "SCRAPES/DREDGES")) %>% 
  group_by(hr) %>% summarise(n=n()) %>% arrange(hr)
total2 = dim(WMdata %>% filter(!mo %in% c(1,2,3,4),
                               GearType %in% c("POUND NET - FISH", "POTS - EEL", "TROTLINES", 
                                               "CRAB POTS", "SCRAPES/DREDGES")))[1]
x3 = WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "Crab") %>%
  group_by(hr) %>% summarise(n=n()) %>% arrange(hr)
total3 = dim(WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "Crab"))[1]

x4 = WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "FinFish") %>%
  group_by(hr) %>% summarise(n=n()) %>% arrange(hr)
total4 = dim(WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "FinFish"))[1]

x5 = WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "FinFish",
                       GearType %in% c("POUND NET - FISH", "POTS - EEL", "GILL NET - DRIFT", 
                                       "HOOK AND LINE", "COLLAPSIBLE TRAPS")) %>%
  group_by(hr) %>% summarise(n=n()) %>% arrange(hr)
total5 = dim(WMdata %>% filter(!mo %in% c(1,2,3,4), fishery %in% "FinFish",
                               GearType %in% c("POUND NET - FISH", "POTS - EEL", "GILL NET - DRIFT", 
                                               "HOOK AND LINE", "COLLAPSIBLE TRAPS")))[1]


# final table
times = as.data.frame(matrix(nrow = 24, ncol = 1, data = seq(1:24))) %>% 
  rename(hr = V1) %>% mutate(hr = as.numeric(hr))

xTotals = times %>% 
  left_join(., mutate(x, n = format(round((n/total)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x2, n = format(round((n/total2)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x3, n = format(round((n/total)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x3, n = format(round((n/total3)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x4, n = format(round((n/total)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x4, n = format(round((n/total4)*100, 3), nsmall=3)), by = "hr") %>%
  left_join(., mutate(x5, n = format(round((n/total5)*100, 3), nsmall=3)), by = "hr")

names(xTotals) = c("Landing Hour", 
                   "Percent of trips landed",
                   "Percent of trips landed for top 5 gear types",
                   "Percent of trips landed for crab gear types out of total trips",
                   "Percent of trips landed for crab gear types out of crab trips",
                   "Percent of trips landed for finfish gear types out of total trips", 
                   "Percent of trips landed for finfish gear types out of finfish trips", 
                   "Percent of trips landed for top 5 finfish gear types out of top 5 finfish trips")

# xTable = htmlTable(xTotals,
#           caption = "Table A1. Summary of trips from May to December in 2018 by landing hour.",
#           header =  c("Landing Hour", 
#                             "Percent of trips landed",
#                             "Percent of trips landed for top 5 gear types<sup>&dagger;</sup>",
#                             "Percent of trips landed for crab gear types out of total trips",
#                             "Percent of trips landed for crab gear types out of crab trips",
#                             "Percent of trips landed for finfish gear types out of total trips", 
#                             "Percent of trips landed for finfish gear types out of finfish trips", 
#                             "Percent of trips landed for top 5 finfish gear types out of top 5 finfish trips<sup>&Dagger;<sup/>"),
#           rnames = FALSE,
#           theme = 'scientific',
#           align.header = 'c',
#           align = 'c',
#           col.columns = "none", padding.rgroup = "&nbsp;&nbsp;", 
#           tfoot = txtMergeLines("<sup>&dagger;</sup>top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges",
#                                 "<sup>&Dagger;</sup>top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps"),
#           css.cell = "padding-left: .5em; padding-right: 0.5em;") 
# 
# write.table(xTable, 
#             file=paste(dir.out, "LandingHoursTable.html",sep=""), 
#             quote = FALSE,
#             col.names = FALSE,
# row.names = FALSE)

# ------------- #

# names(xTotals) = c("Landing Hour","a","b","c","d","e","f","g")
# dummyData = xTotals %>% mutate(a = replace(a,is.na(a),0),
#                                b = replace(b,is.na(b),0),
#                                c = replace(c,is.na(c),0),
#                                d = replace(d,is.na(d),0),
#                                e = replace(e,is.na(e),0),
#                                f = replace(f,is.na(f),0),
#                                g = replace(g,is.na(g),0))
# 
# css <- make_css_color_rank_theme(list(a = xTotals$a,
#                                       b = xTotals$b,
#                                       c = xTotals$c,
#                                       d = xTotals$d,
#                                       e = xTotals$e,
#                                       f = xTotals$f,
#                                       g = xTotals$g),
#                                  colors = c("dodgerblue2","skyblue","lightskyblue","white","yellow","gold","goldenrod2"))
#

#qu_25_75 <- quantile(as.numeric(xTotals[!is.na(xTotals[,2]),2]), c(0.25, 0.75))
xRange = as.numeric(c(xTotals[,2],xTotals[,3],xTotals[,4],xTotals[,5],xTotals[,6],xTotals[,7],xTotals[,8]))
qu_25_75 <- quantile(xRange[!is.na(xRange)], c(0.25, 0.75))
xTotals = mutate(xTotals, 
                 a = as.numeric(a),
                 b = as.numeric(b),
                 c = as.numeric(c),
                 d = as.numeric(d),
                 e = as.numeric(e),
                 f = as.numeric(f),
                 g = as.numeric(g))

xTable = tableHTML(xTotals, rownames = FALSE, widths = rep(140,8),
                   caption = "Table A1. Summary of trips from May to December in 2018 by landing hour.",
                   footer = txtMergeLines("<sup>&dagger;</sup>top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges",
                                          "<sup>&Dagger;</sup>top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps")) %>%
  add_css_thead(css = list(c('font-size', 'text-align'), c('25px','left'))) %>%
  add_css_tbody(css = list(c('font-size', 'text-align'), c('30px','left'))) %>%
  add_css_footer(css = list(c('font-size', 'text-align'), c('20px', 'left'))) %>%
  add_css_caption(css = list(c('font-size', 'text-align'), c('30px', 'left'))) %>%
  add_theme('scientific') %>% 
  # add_css_conditional_column(conditional = "<", value = qu_25_75[1], css=list('backgroud-color','dogerblue2'),columns=6) %>%
  # add_css_conditional_column(conditional = ">", value = qu_25_75[2], css=list('backgroud-color','goldenrod2'),columns=6) %>%
  # add_css_conditional_column(conditional = "between", value = qu_25_75, css=list('backgroud-color','white'),columns=6) %>%
  add_css_conditional_column(conditional = "<", value = qu_25_75[1], css=list('background-color',"dogerblue2"), columns = c("a","b","c","d","e","f","g")) %>%
  add_css_conditional_column(conditional = ">", value = qu_25_75[2], css=list('background-color',"goldenrod2"), columns = c(2,3,4,5,7,8)) %>%
  add_css_conditional_column(conditional = "between", value = qu_25_75, css=list('background-color',"white"), columns = c(2,3,4,5,7,8)) 
# add_css_conditional_column(conditional = "color_rank", color_rank_theme = "Custom", columns = 6, decreasing = FALSE, color_rank_css = css) %>%
# add_css_conditional_column(conditional = "color_rank", color_rank_theme = "Custom", columns = c(2,3,4,5,7,8), decreasing = FALSE, color_rank_css = css) 


write_tableHTML(xTable, file = paste(dir.out, "LandingHoursTableColored.html",sep=""))  

xTable %>% tableHTML_to_image()
# xTable = xtable(xTotals)
# options(xtable.floating = FALSE) 
# options(xtable.timestamp = "")
# print.xtable(xTable, type="html", file=paste(dir.out, "LandingHoursTable.html", sep = ""), 
#              floating=TRUE)
#---- #

x = WMdata %>% filter(!mo %in% c(1,2,3,4),
                      GearType %in% c("POUND NET - FISH", "POTS - EEL", "TROTLINES", 
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

x2 = WMdata %>% filter(!mo %in% c(1,2,3,4),
                       fishery %in% "Crab") %>%
  dplyr::select(TripID, hr) %>%
  mutate(block = "Not in any block", 
         block = replace(block, hr %in% c(7,8,9,10,11,12,13), "7 am to 1 pm"),
         block = replace(block, hr %in% c(11,12,13,14,15,16,17), "11 am to 5 pm"),
         block = replace(block, hr %in% c(15,16,17,18,19,20,21), "3 pm to 9 pm")) %>% 
  group_by(block) %>%
  summarise(n=n()) 
total2 = sum(x2$n)
to.add = x2[1,] %>% mutate(block = "In a block", n = sum(x2$n[!x2$block %in% "Not in any block"]))
x2 = bind_rows(x2, to.add)

x3 = WMdata %>% filter(!mo %in% c(1,2,3,4),
                       fishery %in% "FinFish",
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

blocks = as.data.frame(matrix(nrow = 5, ncol = 1, data = c(x$block, "In a block"))) %>% 
  rename(block = V1) %>% mutate(index = c(2,3,1,5,4)) %>% arrange(index) %>% dplyr::select(-index)

xTotals = blocks %>% 
  left_join(., mutate(x, n = format(round((n/total)*100, 3), nsmall=3)), by = "block") %>%
  left_join(., mutate(x2, n = format(round((n/total2)*100, 3), nsmall=3)), by = "block") %>%
  left_join(., mutate(x3, n = format(round((n/total3)*100, 3), nsmall=3)), by = "block") 

names(xTotals) = c("Time block","For all trips in the top 5 gear types","For all crab trips","For all finfish trips in the top 5 gear types")

# xTable = tableHTML(xTotals, rownames = FALSE, widths = rep(240,4),
#                    caption = "Table 1. Percent of trips from May to December in 2018 by time block.",
#                    footer = txtMergeLines("<sup>&dagger;</sup>top 5 gear types: pound net, eel pots, trotlines, crab pots, and scrapes/dredges",
#                                           "<sup>&Dagger;</sup>top 5 finfish gear types: pound net, eel pots, gill net, hook and line, and collapsible traps")) %>%
#   add_css_thead(css = list(c('font-size', 'text-align'), c('25px','left'))) %>%
#   add_css_tbody(css = list(c('font-size', 'text-align'), c('30px','left'))) %>%
#   add_css_footer(css = list(c('font-size', 'text-align'), c('20px', 'left'))) %>%
#   add_css_caption(css = list(c('font-size', 'text-align'), c('30px', 'left'))) %>%
#   add_theme('scientific') 

xTable = htmlTable(xTotals, rnames = FALSE,
                   caption="Table 1. Percent of trips from May to December in 2018 by time block",
                   header =  c("Time block",
                               "Percent of all trips in the top 5 gear types<sup>&dagger;</sup>",
                               "Percent of all crab trips",
                               "Percent of all finfish trips in the top 5 gear types<sup>&Dagger;</sup>"),
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
