# ----------------- #
# divide priority lists by regions
# ----------------- #

# ----------------- #
# load packages
library(dplyr)
library(readxl)
# ----------------- #

# ----------------- #
# directories
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/PriorityLists/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/PriorityLists/output/"
# ----------------- #

# ----------------- #
# load data
crab <- read_excel(paste(dir.in, "ECrabPriority Oct-Dec.xlsx", sep = ""))
fish <- read_excel(paste(dir.in, "EFishPriority Oct- Dec.xlsx", sep = ""))
fish = fish[, 1:7] #loaded false columns, clip

# load regions
zips = read.csv("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/R1split/new_regions_r1split_3and4combo.csv")
# ----------------- #

# ----------------- #
# filter
r1acrab = crab %>% filter()
r1bcrab = crab %>% filter()
r2crab = crab %>% filter()
r3crab = crab %>% filter()
r4crab = crab %>% filter()
r5crab = crab %>% filter()
r6crab = crab %>% filter()

r1afish = fish %>% filter()
r1bfish = fish %>% filter()
r2fish = fish %>% filter()
r3fish = fish %>% filter()
r4fish = fish %>% filter()
r5fish = fish %>% filter()
r6fish = fish %>% filter()
# ----------------- #

# ----------------- #
# output lists
write.csv(paste(dir.out, "r1acrab.csv", sep = ""))
write.csv(paste(dir.out, "r1bcrab.csv", sep = ""))
write.csv(paste(dir.out, "r2crab.csv", sep = ""))
write.csv(paste(dir.out, "r3crab.csv", sep = ""))
write.csv(paste(dir.out, "r4crab.csv", sep = ""))
write.csv(paste(dir.out, "r5crab.csv", sep = ""))
write.csv(paste(dir.out, "r6crab.csv", sep = ""))

write.csv(paste(dir.out, "r1afish.csv", sep = ""))
write.csv(paste(dir.out, "r1bfish.csv", sep = ""))
write.csv(paste(dir.out, "r2fish.csv", sep = ""))
write.csv(paste(dir.out, "r3fish.csv", sep = ""))
write.csv(paste(dir.out, "r4fish.csv", sep = ""))
write.csv(paste(dir.out, "r5fish.csv", sep = ""))
write.csv(paste(dir.out, "r6fish.csv", sep = ""))
# ----------------- #
