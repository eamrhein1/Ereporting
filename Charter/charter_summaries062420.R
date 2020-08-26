# summarize the charter data so far
# factor in RM reports to look at BRP 

# request: Could I get a summary of some of the roving monitor data compared to the trips data. Our leadership has heard that theres compliance issues and poor data. So I'ld like to show them specifically. How many trips have been monitored. 

# -------------- #
# load packages
# -------------- #
require(dplyr)
require(ggplot2)
# -------------- #

# -------------- #
# load data
# -------------- #
trips = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterTrips_062420.csv")

RM = read.csv("~/Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/CharterMonitorReports_062420.csv")
RM$Trip.ID[RM$Trip.ID %in% 748012] = 748236 # Dan put in the wrong Trip.ID
RM = filter(RM, !Trip.ID %in% 739656) # 739656 should be deleted
# -------------- #


# -------------- #
# summarize data
# -------------- #
### number of trips
ntrips = length(unique(trips$Trip.ID))

### how many trips have been monitored
tripsAttempted = length(unique(RM$Trip.ID))
tripsMonitored = length(unique(RM$Trip.ID[RM$Result %in% c("MONITORED","MONITORED (on paper)")]))
# success rate
(tripsMonitored/tripsAttempted )*100

### reasons for trips not monitored
RM %>% group_by(Trip.ID) %>% distinct() %>% ungroup() %>%
  filter(!Result %in% c("MONITORED","MONITORED (on paper)")) %>%
  group_by(Result) %>% 
  summarise(n=n())

##### comparisons for successful trips between reports
monitored_trips = RM %>% filter(Result %in% c("MONITORED","MONITORED (on paper)")) %>%
  group_by(Trip.ID) %>%
  mutate(lastReport = ifelse(Monitor.Report.. == max(Monitor.Report..),"yes","no")) %>%
  filter(lastReport %in% "yes")

### check angler count  
ac = left_join(dplyr::select(monitored_trips , Trip.ID, Angler.Count) %>% distinct(),
              dplyr::select(trips, Trip.ID, Angler.Count) %>% distinct(), by = "Trip.ID")
# error/success rate
(dim(filter(ac, !Angler.Count.x %in% Angler.Count.y))[1]/dim(ac)[1])*100
(dim(filter(ac, Angler.Count.x %in% Angler.Count.y))[1]/dim(ac)[1])*100

### check catch kept
bait = left_join(dplyr::select(monitored_trips , Trip.ID, Species.Grade, Disposition, Quantity, Unit, Count, Size.Class) %>% distinct() %>% rename(Species = Species.Grade),
               dplyr::select(trips, Trip.ID, Species, Disposition, Quantity, Unit, Count, Size.Class) %>% 
                 distinct(), by = c("Trip.ID","Species", "Disposition")) %>% 
  filter(Disposition %in% "Kept for bait")

wp = left_join(dplyr::select(monitored_trips , Trip.ID, Species.Grade, Disposition, Quantity, Unit, Count, Size.Class) %>% distinct() %>% 
                 rename(Species = Species.Grade) %>% filter(Species %in% "WHITE PERCH"),
                  dplyr::select(trips, Trip.ID, Species, Disposition, Quantity, Unit, Count, Size.Class) %>% 
                    distinct() %>% filter(Species %in% "WHITE PERCH"), 
               by = c("Trip.ID","Species", "Disposition")) %>% 
  filter(!Disposition %in% c("Released","Kept for bait"))
# check for different disposition
#trips[trips$Trip.ID %in% 731349,]
#trips[trips$Trip.ID %in% 744326,]
#trips[trips$Trip.ID %in% 744714,]

bc = left_join(dplyr::select(monitored_trips , Trip.ID, Species.Grade, Disposition, Quantity, Unit, Count, Size.Class) %>% distinct() %>% 
                 rename(Species = Species.Grade) %>% filter(Species %in% "BLUE CATFISH"),
               dplyr::select(trips, Trip.ID, Species, Disposition, Quantity, Unit, Count, Size.Class) %>% 
                 distinct() %>% filter(Species %in% "BLUE CATFISH"), 
               by = c("Trip.ID","Species", "Disposition")) %>% 
  filter(!Disposition %in% c("Released","Kept for bait"))

sb = left_join(dplyr::select(monitored_trips , Trip.ID, Species.Grade, Disposition, Quantity, Unit, Count, Size.Class) %>% distinct() %>% 
                 rename(Species = Species.Grade) %>% filter(Species %in% "STRIPED BASS"),
               dplyr::select(trips, Trip.ID, Species, Disposition, Quantity, Unit, Count, Size.Class) %>% 
                 distinct() %>% filter(Species %in% "STRIPED BASS"), 
               by = c("Trip.ID","Species", "Disposition")) %>% 
  filter(!Disposition %in% c("Released","Kept for bait"))

sbR = left_join(dplyr::select(monitored_trips , Trip.ID, Species.Grade, Disposition, Quantity, Unit, Count, Size.Class) %>% distinct() %>% 
                 rename(Species = Species.Grade) %>% filter(Species %in% "STRIPED BASS"),
               dplyr::select(trips, Trip.ID, Species, Disposition, Quantity, Unit, Count, Size.Class) %>% 
                 distinct() %>% filter(Species %in% "STRIPED BASS"), 
               by = c("Trip.ID","Species", "Disposition")) %>% 
  filter(Disposition %in% "Released")

# alter kept and kept fileted into one disposition
monitored_trips2 = monitored_trips %>% dplyr::recode(Disposition, "Kept - fileted" ="Kept")
# -------------- #
