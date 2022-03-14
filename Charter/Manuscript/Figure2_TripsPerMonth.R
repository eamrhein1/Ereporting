##Trips Per Month in 2020-2021
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
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(dat2021) = gsub(" ","", names(dat2021))
names(dat2020) = gsub(" ","", names(dat2020))

add_date <- function(x){
  x %>% dplyr::select(TripID, Date) %>%
    mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y")) %>%
    mutate(mo = month(Date),
           yr=year(Date)) %>%
    distinct()
}
dat2021 = add_date(dat2021)
dat2020 = add_date(dat2020)
# summarize
dat2021_sum = dat2021 %>% group_by(mo) %>%
  summarize(n=n())
dat2020_sum = dat2020 %>% group_by(mo) %>%
  summarize(n=n())
# -------------------- #

# stats
# stats
length(unique(dat2021$TripID))
length(unique(dat2020$TripID))
# -------------------- #
# descriptive plot
dat = rbind(dat2020_sum %>% mutate(year="2020"), dat2021_sum %>% mutate(year="2021"))

p = ggplot() +
  geom_bar(data = dat, aes(x=mo, y=n, fill = year), stat="identity", position = "dodge") +
  scale_fill_manual("Year", values = c("2021" = "navy", "2020" = "lightsteelblue3")) +
  scale_x_continuous(breaks = seq(4, 12, 1)) +
  theme(text = element_text(size = 20)) +
  labs(x="Month",y = "Number of Trips")
p
# -------------------- #

# export
ggsave("C:/Users/Eric Amrhein/Documents/Oyster Recovery Partnership/STAT/Publication/Figure2_trips_per_month.png",p)

# -------------------- #
