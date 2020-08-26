# check for spot discrepancies between charter captains and RMs/onboard observers

# packages
library(dplyr)
library(readr)

# load data
RM <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_081920.csv")
dat <- read_csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_08192020.csv")
names(dat) = gsub(" ", "", names(dat), fixed = TRUE)
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
names(RM) = gsub("/", "", names(RM), fixed = TRUE)
names(RM) = gsub("#", "", names(RM), fixed = TRUE)
names(dat) = gsub("#", "", names(dat), fixed = TRUE)

# compare data
RM = RM %>% 
  group_by(TripID) %>%
  mutate(lastReport = ifelse(MonitorReport == max(MonitorReport),"yes","no")) %>%
  filter(lastReport %in% "yes") %>%
  rename(Species = SpeciesGrade) %>%
  filter(Species %in% "SPOT") %>% 
  dplyr::select(TripID, Species, Count, Quantity, Disposition) %>%
  distinct()

dat = dat %>% 
  group_by(TripID) %>%
  mutate(lastReport = ifelse(SH == max(SH) & EH == max(EH),"yes","no")) %>%
  filter(lastReport %in% "yes") %>%
  filter(Species %in% "SPOT") %>% 
  dplyr::select(TripID, Species, Count, Quantity, Disposition) %>%
  distinct() %>% 
  filter(TripID %in% RM$TripID)

spot_combo = left_join(RM, dat, by = "TripID") %>% 
  filter(Count.x != Count.y | is.na(Count.y))


