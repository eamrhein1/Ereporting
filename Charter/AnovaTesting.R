# load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# load data
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RM <- rbind(RMdat2020, RMdat2021)
# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
trips <- rbind(dat2020, dat2021)

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
#SPOT KEPT FOR BAIT
#Filter for Kept for Bait only (Onboard)
obKB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

ob_tripKB = trips %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(ob_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), lwd = 2, col="blue")
# there is a lot of overlap but there is a slight difference between the means

# format data
test.set = left_join(dplyr::select(obKB, TripID, Count), dplyr::select(ob_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Spot kept for bait on onboard trips (2020)###

# load data
#loaddata
# ------------- #
# ------------- #

RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")

# -------------------- #
# load data
# -------------------- #

trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
####SPOT KEPT FOR BAIT###
#Filter for Kept for Bait only (Onboard in 2020)
obKB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

ob_tripKB = trips %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(ob_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), lwd = 2, col="blue")
# there is a lot of overlap but there is a slight difference between the means

# format data
test.set = left_join(dplyr::select(obKB, TripID, Count), dplyr::select(ob_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)


###Spot kept for bait on onboard trips (2021)###
#loaddata
# ------------- #
# ------------- #
RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")

# -------------------- #
# load data
# -------------------- #
trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
#SPOT KEPT FOR BAIT
#Filter for Kept for Bait only (Onboard)
obKB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

ob_tripKB = trips %>%
  filter(TripID %in% obKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(ob_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obKB$Count, na.rm=T), sd = sd(obKB$Count, na.rm=T)), lwd = 2, col="blue")
# there is a lot of overlap but there is a slight difference between the means

# format data
test.set = left_join(dplyr::select(obKB, TripID, Count), dplyr::select(ob_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)


###Spot Kept for Bait during RM trips (Entire Pilot)###
# load data
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RM <- rbind(RMdat2020, RMdat2021)
# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
trips <- rbind(dat2020, dat2021)

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
#SPOT KEPT FOR BAIT
#Filter for Kept for Bait only (RM)
RMKB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

RM_tripKB = trips %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(RM_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripKB$Count), sd = sd(RM_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), lwd = 2, col="blue")

# format data
test.set = left_join(dplyr::select(RMKB, TripID, Count), dplyr::select(RM_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)


###Spot Kept for Bait during RM trips (2020)###
# load data
#loaddata
# ------------- #
# ------------- #

RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")

# -------------------- #
# load data
# -------------------- #

trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")


#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
#SPOT KEPT FOR BAIT
#Filter for Kept for Bait only (RM)
RMKB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

RM_tripKB = trips %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(RM_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripKB$Count), sd = sd(RM_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), lwd = 2, col="blue")

# format data
test.set = left_join(dplyr::select(RMKB, TripID, Count), dplyr::select(RM_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Spot Kept for Bait during RM trips (2021)###
# load data
#loaddata
# ------------- #
# ------------- #
RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")


# -------------------- #
# load data
# -------------------- #
trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")



#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

# ------------- #
#SPOT KEPT FOR BAIT
#Filter for Kept for Bait only (RM)
RMKB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Kept for bait",
         `SpeciesGrade` %in% "SPOT")

RM_tripKB = trips %>%
  filter(TripID %in% RMKB$TripID,
         Disposition %in% "Kept for bait",
         Species %in% "SPOT")

# check for normal distribution
ggplot(RM_tripKB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripKB$Count), sd = sd(ob_tripKB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripKB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripKB$Count), sd = sd(RM_tripKB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obKB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMKB$Count, na.rm=T), sd = sd(RMKB$Count, na.rm=T)), lwd = 2, col="blue")

# format data
test.set = left_join(dplyr::select(RMKB, TripID, Count), dplyr::select(RM_tripKB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)






###Striped Bass Released (ONBOARD) Entire Pilot###
# ------------- #
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RM <- rbind(RMdat2020, RMdat2021)
# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
trips <- rbind(dat2020, dat2021)

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Onboard)
obSB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

ob_tripSB = trips %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(ob_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(obSB, TripID, Count), dplyr::select(ob_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Striped Bass Released (ROVING MONITORS) Entire Pilot###
# ------------- #
#loaddata
# ------------- #
# ------------- #
RMdat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")
RMdat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")
RM <- rbind(RMdat2020, RMdat2021)
# -------------------- #
# load data
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
trips <- rbind(dat2020, dat2021)

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Roving Monitor)
RMSB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

RM_tripSB = trips %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(RM_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = RM_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = RMSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(RMSB, TripID, Count), dplyr::select(RM_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Striped Bass Released (ROVING MONITORS) 2020###
# ------------- #
#loaddata
# ------------- #
# ------------- #
RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")

# -------------------- #
# load data
# -------------------- #

trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")


#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Roving Monitor)
RMSB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

RM_tripSB = trips %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(RM_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = RM_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = RMSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(RMSB, TripID, Count), dplyr::select(RM_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Striped Bass Released (ONBOARD) 2020###
# ------------- #
#loaddata
# ------------- #
# ------------- #

RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522_2.csv")

# -------------------- #
# load data
# -------------------- #
trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")


#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Onboard)
obSB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

ob_tripSB = trips %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(ob_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(obSB, TripID, Count), dplyr::select(ob_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

###Striped Bass Released (ROVING MONITORS) 2021###
# ------------- #
#loaddata
# ------------- #
# ------------- #
RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")

# -------------------- #
# load data
# -------------------- #
trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")


#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Roving Monitor)
RMSB = RM %>% 
  filter(Onboard %in% "N",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

RM_tripSB = trips %>%
  filter(TripID %in% RMSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(RM_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = RM_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RM_tripSB$Count), sd = sd(RM_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = RMSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(RMSB$Count, na.rm=T), sd = sd(RMSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(RMSB, TripID, Count), dplyr::select(RM_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)


###Striped Bass Released Onboard Only (2021)####
# ------------- #
#loaddata
# ------------- #
# ------------- #
RM = read_csv("C:/Users/Eric Amrhein/Downloads/CharterMonitorReports_021522.csv")


# load data
# -------------------- #
trips = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")

#Fix Names
names(trips) = gsub(" ","", names(trips))
names(trips) = gsub("#","", names(trips))
names(trips) <- gsub("\\.", "", names(trips))
names(RM) = gsub(" ","", names(RM))
names(RM) = gsub("#","", names(RM))
names(RM) <- gsub("\\.", "", names(RM))
names(RM) <- gsub("/", "", names(RM))

# filter trips to TripID in RM
trips = filter(trips, TripID %in% RM$TripID)

# filter to only the last report
# filter to last trip reported

# filter to last trip reported
dat = trips %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

#Filter for Released SB only (Onboard)
obSB = RM %>% 
  filter(Onboard %in% "Y",
         Result %in% c("MONITORED","MONITORED (on paper)"),
         Disposition %in% "Released",
         `SpeciesGrade` %in% "STRIPED BASS")

ob_tripSB = trips %>%
  filter(TripID %in% obSB$TripID,
         Disposition %in% "Released",
         Species %in% "BASS, STRIPED")

# check for normal distribution
ggplot(ob_tripSB, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange") +
  ggtitle("Watermen")

ggplot(RM, aes(x = Count)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), col="blue") +
  ggtitle("Roving Monitors")

# look at the patterns of the distributions
# alpha makes it opaque and boundary centers the bar on 0.5 instead of 0 to jitter the plots
ggplot() +
  geom_histogram(data = ob_tripSB, aes(x=Count, y = ..density..), fill="orange", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(ob_tripSB$Count), sd = sd(ob_tripSB$Count)), col="orange", lwd=2) +
  geom_histogram(data = obSB, aes(x=Count, y = ..density..), boundary=0.5, fill ="blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(obSB$Count, na.rm=T), sd = sd(obSB$Count, na.rm=T)), lwd = 2, col="blue")


# format data
test.set = left_join(dplyr::select(obSB, TripID, Count), dplyr::select(ob_tripSB, TripID, Count), by="TripID") %>%
  filter(!is.na(Count.x), !is.na(Count.y))
names(test.set) = c("TripID", "RM_Count", "W_Count")

#Run ANOVA
test <- aov(RM_Count ~ W_Count, data = test.set)
summary(test)

t.test(test.set$RM_Count, test.set$W_Count, paired = TRUE)

