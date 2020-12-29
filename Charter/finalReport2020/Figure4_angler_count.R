# -------------------- #
# Angler count
# created by K. Coleman Dec. 2020
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
dat <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_122920.csv")
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
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/finalReport2020/Figure4_angler_count.png",p)
# -------------------- #




