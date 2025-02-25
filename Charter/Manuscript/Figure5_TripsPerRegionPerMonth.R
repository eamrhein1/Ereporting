# load packages
# -------------------- #
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
# -------------------- #
# load data
# -------------------- #
Zregions <- read_csv("C:/Users/Eric Amrhein/Documents/Oyster Recovery Partnership/STAT/charter_regions_May2020.csv")
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
#------------------ #
datpilot <- rbind(dat2020, dat2021)

# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(datpilot) = gsub(" ","", names(datpilot))

datpilot = datpilot %>% dplyr::select(TripID, Date, EHZip) %>%
  distinct() %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y")) %>%
  mutate(mo = month(Date)) %>%
  rename(zipcode = EHZip)

#  join to region data
dat = left_join(datpilot, Zregions, by="zipcode")

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
  labs(x="Month (2020-2021)",y="Number of Trips",col="Charter Region") +
  scale_colour_manual(values = c("lightgrey","#E69F00","gold","cornflowerblue","black"))
p 

# export
ggsave("C:/Figure5_trips_per_region.png",p)
# -------------------- #
