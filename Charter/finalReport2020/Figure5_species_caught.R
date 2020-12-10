# -------------------- #
# plot species caught and released throughout the season
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
dat <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_120920.csv")
# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(dat) = gsub(" ","", names(dat))
names(dat) = gsub("#","", names(dat))

dat = dat %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

dat_sum = dat %>% group_by(Species, Disposition, TripID) %>%
  summarise(ct = sum(Count))

# check if kept for bait was most popular spot disposition
dat %>% filter(Species %in% "SPOT") %>%
  group_by(Disposition) %>% 
  summarise(n=n())

# check if spot were most common species kept for bait
dat %>% filter(Disposition %in% "Kept for bait") %>%
  group_by(Species) %>% 
  summarise(n=n())

# striped bass
median(dat_sum$ct[dat_sum$Species %in% "BASS, STRIPED" & dat_sum$Disposition %in% "Kept"])

# -------------------- #


# -------------------- #
# plot
# -------------------- #
p = ggplot(data = dat_sum, aes(x=Species, y=ct, col=Disposition)) + 
  geom_boxplot() + 
  facet_grid(Disposition~., scales='free')+
  theme_bw() + 
  theme(#text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  labs(y="Count") + 
  lims(y=c(0,350))  +
  scale_colour_manual(values = c("black","#E69F00","cornflowerblue"))
p

# export
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/output/charter/finalReport2020/Figure5_species_caught.png",p)
# -------------------- #




