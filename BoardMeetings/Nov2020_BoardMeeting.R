# FACTS summary of trips by month for Nov. ORP board meeting

# packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

# dir
dir.in = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/"
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/"

# load data
dat = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/FACTSMD-898.csv")
names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
dat = mutate(dat, 
             Date = as.POSIXct(Date, format = "%d/%m/%Y"),
             mo = month(Date),
             yr = as.character(year(Date)), 
             month = mo, 
             month = replace(month, month %in% 1, "Jan."),
             month = replace(month, month %in% 2, "Feb."),
             month = replace(month, month %in% 3, "Mar."),
             month = replace(month, month %in% 4, "Apr."),
             month = replace(month, month %in% 5, "May."),
             month = replace(month, month %in% 6, "Jun."),
             month = replace(month, month %in% 7, "Jul."),
             month = replace(month, month %in% 8, "Aug."),
             month = replace(month, month %in% 9, "Sep."),
             month = replace(month, month %in% 10, "Oct."),
             month = replace(month, month %in% 11, "Nov."),
             month = replace(month, month %in% 12, "Dec."))
dat_sum = dat %>% 
  group_by(mo, month, yr) %>% 
  summarise(n=n())  
dat_sum_by_fish = dat %>% 
  group_by(mo, month, yr, Fishery) %>% 
  summarise(n=n()) 

# 2020 data
# charter2020 = read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_080520.csv")
# names(charter2020) = gsub(" ", "", names(charter2020), fixed = TRUE)
# charter2020 = dplyr::select(charter2020, TripID, Date) %>% distinct() %>%
#   mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"),
#          mo = month(Date),
#          yr = year(Date))
# charter2020_sum = charter2020 %>% 
#   group_by(mo, yr) %>% 
#   summarise(n=n()) %>%
#   filter(!mo %in% 8) %>%
#   mutate(type = "charter")

# plot
p = ggplot() + 
  geom_bar(data = dat_sum, aes(reorder(month, mo), n, fill = yr), stat = "identity" , position = "dodge") + 
  labs(x = "Month", y = "Number of trips", fill = "Year") + 
  theme_bw() +
  theme(text = element_text(size = 20)) 
p

p = ggplot() + 
  geom_bar(data = dat_sum_by_fish, aes(reorder(month, mo), n, fill = Fishery), stat = "identity" , position = "dodge") + 
  labs(x = "Month", y = "Number of trips") + 
  theme_bw() + 
  facet_wrap(~yr) +
  theme(text = element_text(size = 20)) 
p

dat_sum_by_fish = mutate(dat_sum_by_fish, 
                         forder = NA,
                         forder = ifelse(Fishery %in% "Finfish", 3, ifelse(Fishery %in% "Charter", 1, 2)))
p = ggplot() + 
  geom_bar(data = arrange(dat_sum_by_fish, forder), aes(reorder(month, mo), n, fill = reorder(Fishery, forder)), stat = "identity") + #, position = position_stack(reverse = TRUE)) + 
  labs(x = "Month", y = "Number of trips", fill = "Fishery") + 
  theme_bw() + 
  facet_wrap(~yr) +
  theme(text = element_text(size = 20)) 
p
ggsave(paste(dir.out, "Nov_BoardMeeting_Trip_Summary_by_Fishery_stacked2020.png", sep=""),p)
# ---------------- # 

# ---------------- # 
# trips monitored
# ---------------- # 
mdat = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_102920.csv")
mdat = mdat %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"),
         mo = month(Date),
         yr = as.character(year(Date)), 
         month = mo, 
         month = replace(month, month %in% 1, "Jan."),
         month = replace(month, month %in% 2, "Feb."),
         month = replace(month, month %in% 3, "Mar."),
         month = replace(month, month %in% 4, "Apr."),
         month = replace(month, month %in% 5, "May."),
         month = replace(month, month %in% 6, "Jun."),
         month = replace(month, month %in% 7, "Jul."),
         month = replace(month, month %in% 8, "Aug."),
         month = replace(month, month %in% 9, "Sep."),
         month = replace(month, month %in% 10, "Oct."),
         month = replace(month, month %in% 11, "Nov."),
         month = replace(month, month %in% 12, "Dec.")) 

mdat_sum = mdat %>% 
  dplyr::select(Trip.ID, Date, mo, month, yr) %>%
  distinct() %>%
  group_by(mo, month, yr) %>% 
  summarise(n=n()) %>% ungroup()

mdat_breakout_sum = mdat %>% 
  mutate(success = ifelse(Result %in% c("MONITORED","MONITORED (on paper)"),"Y","N")) %>%
  dplyr::select(Trip.ID, Date, mo, month, yr, Onboard, success) %>%
  distinct() %>%
  group_by(mo, month, yr, Onboard, success) %>% 
  summarise(n=n()) %>% ungroup()

# stats
sum(mdat_sum$n) # total trips attempted to be monitored
sum(mdat_breakout_sum$n[mdat_breakout_sum$Onboard %in% "Y" & mdat_breakout_sum$success %in% "Y"]) # total trips monitored by Eric
sum(mdat_breakout_sum$n[mdat_breakout_sum$Onboard %in% "N" & mdat_breakout_sum$success %in% "Y"]) # total trips monitored by RM

# plot
p = ggplot() + 
  geom_bar(data = mdat_sum, aes(reorder(month, mo), n, fill = yr), stat = "identity" , position = "dodge") +
  annotate(geom="text",label="*Closure 8/16-8/31", x="Aug.", y=75) + 
  labs(x = "Month", y = "Number of trips", fill = "Year") + 
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = 'none') +
  ggtitle("Charter Trips Attempted to be Monitored") 
p
ggsave(paste(dir.out, "Nov_BoardMeeting_Monitored_Summary2020.png", sep=""),p)


# onboard effort
calls = read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Pilot Projects/Charter/Year 1/Onboard Observer/Charter Outreach and Scheduling.xlsx")
names(calls) = gsub(" ","",names(calls))
calls = dplyr::select(calls, PhoneNumber, ContactedDate) 

# pull dates that are separated by commas and make them their own rows
to.modify = filter(calls, grepl(",", ContactedDate))
s <- strsplit(to.modify$ContactedDate, split = ",")
new.calls = data.frame(PhoneNumber = rep(to.modify$PhoneNumber, sapply(s, length)), ContactedDate = unlist(s))
new.calls$ContactedDate = as.POSIXct(new.calls$ContactedDate, format = "%m/%d/%Y")

# final calls list, join modified data
calls.final = filter(calls, !grepl(",",ContactedDate))
calls.final$ContactedDate = as.POSIXct(as.numeric(calls.final$ContactedDate) * (60*60*24), origin="1899-12-30", tz="GMT") # Excel date
calls.final = rbind(calls.final, new.calls)

# stats
length(calls.final$PhoneNumber[!is.na(calls.final$ContactedDate)]) # total calls made
# ---------------- # 

