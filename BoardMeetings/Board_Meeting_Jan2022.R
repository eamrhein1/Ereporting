# FACTS summary of trips by month for Feb. 2022 ORP 4th QTR (Oct.-Nov.) board meeting
# need an annual summary

# packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

# load data
charter_trips = read_csv("Library/CloudStorage/OneDrive-SharedLibraries-OysterRecoveryPartnership,Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_020722.csv")
shellfish_trips = read_csv("Library/CloudStorage/OneDrive-SharedLibraries-OysterRecoveryPartnership,Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishTrips_010422.csv")
names(shellfish_trips) = gsub(" ", "", names(shellfish_trips), fixed = TRUE)
names(charter_trips) = gsub(" ", "", names(charter_trips), fixed = TRUE)

dat = rbind(dplyr::select(shellfish_trips, TripID, Date) %>% mutate(type="Shellfish") %>% distinct(),
            dplyr::select(charter_trips, TripID, Date) %>% mutate(type="Charter") %>% distinct())
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
             month = replace(month, month %in% 12, "Dec.")) %>%
  filter(yr %in% 2021, !TripID %in% 1191070)

dat_sum = dat %>% 
  group_by(mo, month, yr, type) %>% 
  summarise(n=n())  

p = ggplot() + geom_bar(data = dat_sum, aes(x=reorder(month, mo), y=n, fill=type), stat="identity", position="stack") + 
  labs(x="Month",y="Number of Trips",title="Number of Trips in 2021", fill="Type") + 
  theme_bw() + 
  theme(text = element_text(size=20)) + 
  scale_fill_manual("Type", values = c("Charter" = "cornflowerblue", "Shellfish" = "goldenrod1"))
p 
ggsave("Library/CloudStorage/OneDrive-SharedLibraries-OysterRecoveryPartnership,Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/BoardMeetingSummaryQ4_2021.png", p)
