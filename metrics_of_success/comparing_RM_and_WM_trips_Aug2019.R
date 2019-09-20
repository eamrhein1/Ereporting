# -------------------- # 
# this script is to determine metrics of success for roving monitors
# -------------------- # 

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
#library(ggmap)
library(readxl)
library(tidyr)
library(lubridate)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/RM_metrics_of_success/June2019/"
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
# tests
# -------------------- # 
# how many times does an rm monitor a wm 2x/month
r = RM %>% group_by(TripID) %>% summarise(date = first(AssignedMonitor))
w = WM %>% group_by(TripID) %>% summarise(date = first(Date), wm = first(WatermenName))
rw = left_join(r,w,by="TripID")
names(rw) = c("tripid","rm","date","wm")

#rw$date[grepl("/",rw$date) == FALSE] = as.character(as.Date(as.numeric(rw$date[grepl("/",rw$date) == FALSE]), origin = "1899-12-30"))
#rw$date[grepl("/",rw$date) == FALSE] = as.POSIXct(as.numeric(rw$date[grepl("/",rw$date) == FALSE]) * (60*60*24), origin="1899-12-30")
#rw$date[grepl("/",rw$date) == TRUE] = as.character(as.POSIXct(rw$date[grepl("/",rw$date) == TRUE], format = "%m/%d/%Y"))
rw = rw %>% mutate(date = as.Date(date), mo = month(date), d = day(date), day = weekdays(date))

test = rw %>% group_by(wm, mo, rm) %>% summarise(n=n()) %>% filter(n>1, !is.na(wm)) %>% arrange(rm, wm, mo)


# percent for trips monitored so far
(length(unique(RM$TripID))/length(unique(WM$TripID)))*100
#(length(unique(rw$tripid))/length(unique(WM$TripID)))*100

# summary of monitored types
ids = unique(RM$TripID)

monitored = RM %>% group_by(TripID) %>% 
  mutate(AssignedMonitor = replace(AssignedMonitor, is.na(AssignedMonitor), ReportedBy)) %>% 
  summarise(result = first(Result),
            comments = first(Comments),
            rm = first(AssignedMonitor)) 
p = ggplot() + geom_histogram(data = monitored, aes(x = result), stat = "count") + 
  facet_wrap(~rm) + 
  labs(x = "Type", y = "Frequency") +
  coord_flip() + 
  theme_bw() + 
  theme(text = element_text(size = 15))
p
ggsave(paste(dir.out, "monitored_by_RM_hist.png", sep=""), p)

m = monitored %>% mutate(type = NA,
                         type = replace(type, result %in% c("MONITORED","MONITORED (on paper)"),"success"),
                         type = replace(type, !result %in% c("MONITORED","MONITORED (on paper)"),"failure")) %>%
  group_by(type) %>% 
  summarise(n = n(), av = mean(type), std = sd(type))



m = monitored %>% group_by(result) %>% summarise(n = n())

p = ggplot() + geom_histogram(data = m, aes(x = reorder(result, n), y = n), stat = "identity") + 
  geom_label(data = m, aes(x = reorder(result, n), y = n, label = n), size = 12) +
  labs(x = "Type", y = "Frequency") +
  coord_flip() + 
  theme_bw() + 
  theme(text = element_text(size = 15))
p
ggsave(paste(dir.out, "monitored_hist.png", sep=""), p)

# metrics of 'why not-monitored'
not_monitored  = RM %>% group_by(TripID) %>% 
  summarise(result = first(Result),
            comments = first(Comments)) %>% 
  filter(!result %in% c("MONITORED","MONITORED (on paper)")) 
nm = not_monitored %>% group_by(result) %>% summarise(n = n())

p = ggplot() + geom_histogram(data = nm, aes(x = reorder(result, n), y = n), stat = "identity") + 
  geom_label(data = nm, aes(x = reorder(result, n), y = n, label = n), size = 12) +
  labs(x = "Type", y = "Frequency") +
  coord_flip() + 
  theme_bw() + 
  theme(text = element_text(size = 15))
p
ggsave(paste(dir.out, "not_monitored_hist.png", sep=""), p)

rm(m,nm)

# compare catch details
# -------------------- # 
