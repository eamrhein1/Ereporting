# -------------------- #
# plot trips per month 
# created by K. Coleman Dec. 2020
# -------------------- #

# -------------------- #
# load packages
# -------------------- #
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
# -------------------- #


# -------------------- #
# load data
# -------------------- #
dat <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_120920.csv")
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(dat) = gsub(" ","", names(dat))

dat = dat %>% dplyr::select(TripID, Date) %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y")) %>%
  mutate(mo = month(Date)) %>%
  distinct()

# summarize
dat_sum = dat %>% group_by(mo) %>%
  summarize(n=n())

# stats
length(unique(dat$TripID))
# -------------------- #


# -------------------- #
# plot
# -------------------- #
p = ggplot(data = dat_sum, aes(x=mo, y=n), fill="black") + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  theme(text = element_text(size = 20)) +
  labs(x="Month",y="Number of Trips") 
p 

# export
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/finalReport2020/Figure2_trips_per_month.png",p)
# -------------------- #

