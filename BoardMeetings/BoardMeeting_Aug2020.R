# FACTS summary of trips by month for Aug. ORP board meeting

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
dat = read_excel("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/FACTSMD-851.xls")
names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
dat = mutate(dat, Date = as.POSIXct(Date, format = "%m/%d/%Y"),
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
ggsave(paste(dir.out, "Aug_BoardMeeting_Trip_Summary2020.png", sep=""),p)

p = ggplot() + 
  geom_bar(data = dat_sum_by_fish, aes(reorder(month, mo), n, fill = Fishery), stat = "identity" , position = "dodge") + 
  labs(x = "Month", y = "Number of trips") + 
  theme_bw() + 
  facet_wrap(~yr) +
  theme(text = element_text(size = 20)) 
p
ggsave(paste(dir.out, "Aug_BoardMeeting_Trip_Summary_by_Fishery2020.png", sep=""),p)

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
ggsave(paste(dir.out, "Aug_BoardMeeting_Trip_Summary_by_Fishery_stacked2020.png", sep=""),p)
