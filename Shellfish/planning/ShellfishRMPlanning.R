library(dplyr)
library(ggplot)
library(readr)

dat = read_csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishTrips_102821.csv")
names(dat) = gsub(" ","",names(dat))
dat = dplyr::select(dat, TripID, EHLandingTime) %>% 
  mutate(hr = hour(dat$EHLandingTime))

ggplot(dat) + geom_bar(aes(hr)) + theme_bw()

dat = mutate(dat, block = NA,
             block = ifelse(hr %in% c(8,9,10,11),"first", block),
             block = ifelse(hr %in% c(10,11,12,13),"second", block),
             #block = ifelse(hr %in% c(12,13,14,15),"third", block),
             block = ifelse(hr %in% c(11,12,13,14),"third", block),
             block = ifelse(hr %in% c(14,15,16,17),"fourth", block),
             o = 1,
             o = ifelse(block %in% "second", 2, o),
             o = ifelse(block %in% "third", 3, o),
             o = ifelse(block %in% "fourth", 4, o))

ggplot(dat) + geom_bar(aes(reorder(block, o))) + theme_bw()
