### Angler count during 2020-2021 trips
# -------------------- #


# -------------------- #
# load packages
# -------------------- #
library(dplyr)
library(ggplot2)
library(readr)
# -------------------- #


# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
#------------------ #
dat <- rbind(dat2020, dat2021)
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(dat) = gsub(" ","", names(dat))
names(dat) = gsub("#","", names(dat))

dat = dat %>% 
  dplyr::select(TripID, AnglerCount, SH, EH) %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  distinct() #%>%
#filter(AnglerCount < 58) # remove outliers

# basic stats
min(dat$AnglerCount)
max(dat$AnglerCount)
median(dat$AnglerCount)
mean(dat$AnglerCount)
sd(dat$AnglerCount)
# -------------------- #


# -------------------- #
# plot
# -------------------- #
p = ggplot() + 
  geom_bar(data = dat, aes(x=AnglerCount), stat="count") + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  labs(x="Angler Count", y="Number of Trips") 
p

# export
ggsave("C:/Figure3_Angler_count.png",p)
# -------------------- #
