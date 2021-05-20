# -------------------- #
# plot trips per month by season
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
dat <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_010720.csv")
Zregions = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/regions/charter_regions_May2020.csv")
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(dat) = gsub(" ","", names(dat))

dat = dat %>% dplyr::select(TripID, Date, EHZip) %>%
  distinct() %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y")) %>%
  mutate(mo = month(Date)) %>%
  rename(zipcode = EHZip)
  
#  join to region data
dat = left_join(dat, Zregions, by="zipcode")

# summarize
dat_sum = dat %>% group_by(charter_region, mo) %>%
  summarize(n=n()) %>% 
  mutate(charter_region = ifelse(is.na(charter_region),"Zipcode Error",charter_region))
  
# summary stats
dat_sum %>% group_by(charter_region) %>% 
  summarise(n=sum(n)) %>% arrange(n)
# -------------------- #


# -------------------- #
# plot
# -------------------- #
p = ggplot(data = dat_sum, aes(x=mo, y=n, col=charter_region)) + 
  geom_point(size = 5) + 
  geom_line(lwd = 2) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) +
  labs(x="Month",y="Number of Trips",col="Charter Region") +
  scale_colour_manual(values = c("lightgrey","#E69F00","gold","cornflowerblue","black"))
p 

# export
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/finalReport2020/Figure3_trips_per_month_region.png",p)
# -------------------- #
