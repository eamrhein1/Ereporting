library(dplyr)
library(ggplot)
library(readr)

dat = read_csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ShellfishTrips_102821.csv")
names(dat) = gsub(" ","",names(dat))
dat = dplyr::select(dat, TripID, EHLandingTime) %>% 
  mutate(hr = hour(dat$EHLandingTime))

ggplot(dat) + geom_bar(aes(hr)) + theme_bw()
