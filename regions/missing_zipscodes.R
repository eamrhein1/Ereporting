# check missing zip codes and assign region where needed

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
library(readxl)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
new_regions = read.csv(paste(dir.out,"R1split/new_regions_r1split_3and4combo.csv",sep=""))
Aug2019_WM <- read_excel(paste(dir.in, "Aug2019_RM_WM.xlsx", sep = ""), sheet=2)
# -------------------- #

# -------------------- #
# test
# -------------------- #
EH_zips = sort(unique(Aug2019_WM$`EH Zip`))
missing_zips = as.data.frame(sort(unique(EH_zips[!EH_zips %in% new_regions$zipcode])))
names(missing_zips) = "zipcode"
missing_zips = missing_zips %>% filter(!zipcode %in% 0) 
missing_zips$region = c("NA","1a","2","5","5","6","3 and 4","NA","NA","NA")
#first is in Delaware, second and third from last are in VA, last is unknown
# -------------------- # 

