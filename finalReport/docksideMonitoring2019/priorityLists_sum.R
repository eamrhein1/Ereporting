# -------------------- #
# composed of __ % high, ___% medium, ___% low priority watermen.
# -------------------- #
BCP_OctDec <- read_excel(paste(dir.in2,"ECrabPriority Oct-Dec.xlsx", sep="")) %>% 
  rename(FisherName = Name, Monitoring = Priority) %>% 
  dplyr::select(License, Monitoring, FisherName) %>% 
  mutate(startmonth = 10, 
         endmonth = 12, 
         Fishery = "Blue Crab",
         DNRid = NA)
FFP_OctDec <- read_excel(paste(dir.in2,"EFishPriority Oct- Dec.xlsx", sep="")) %>% 
  rename(FisherName = Name, Monitoring = Priority) %>% 
  dplyr::select(License, Monitoring, FisherName) %>% 
  mutate(startmonth = 10, 
         endmonth = 12, 
         Fishery = "Finfish",
         DNRid = NA)
# R1P1 <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sep=""), sheet1) %>% 
#   dplyr::select(DNRid, Monitoring) %>% mutate(startMo = 5, endMo = 6, Fishery = "Finfish")
# R1P2 <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sep=""), sheet2) %>% 
#   dplyr::select(DNRid, Monitoring) %>% mutate(startMo = 7, endMo = 9, Fishery = "Finfish")
# R1P3 <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sep=""), sheet3) %>% 
#   dplyr::select(DNRid, Monitoring) %>% mutate(startMo = 4, endMo = 6, Fishery = "Blue Crab")
# R1P4 <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region1_MaySept.xlsx", sep=""), sheet4) %>% 
#   dplyr::select(DNRid, Monitoring) %>% mutate(startMo = 7, endMo = 9, Fishery = "Blue Crab")
# 

R1P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 1)
R1P_FF_Jul_Sep  <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 2)
R1P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 3)
R1P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 4)

R2P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 1)
R2P_FF_Jul_Sep  <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 2)
R2P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 3)
R2P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region2_MaySept.xlsx", sep=""), sheet = 4)

R3P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region3_MaySept.xlsx", sep=""), sheet = 1)
R3P_FF_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region3_MaySept.xlsx", sep=""), sheet = 2)
R3P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region3_MaySept.xlsx", sep=""), sheet = 3)
R3P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region3_MaySept.xlsx", sep=""), sheet = 4)

R4P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region4_MaySept.xlsx", sep=""), sheet = 1)
R4P_FF_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region4_MaySept.xlsx", sep=""), sheet = 2)
R4P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region4_MaySept.xlsx", sep=""), sheet = 3)
R4P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region4_MaySept.xlsx", sep=""), sheet = 4)

R5P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region5_MaySept.xlsx", sep=""), sheet = 1)
R5P_FF_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region5_MaySept.xlsx", sep=""), sheet = 2)
R5P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region5_MaySept.xlsx", sep=""), sheet = 3)
R5P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region5_MaySept.xlsx", sep=""), sheet = 4)

R6P_FF_May_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region6_MaySept.xlsx", sep=""), sheet = 1)
R6P_FF_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region6_MaySept.xlsx", sep=""), sheet = 2)
R6P_BC_Apr_Jun <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region6_MaySept.xlsx", sep=""), sheet = 3)
R6P_BC_Jul_Sep <- read_excel(paste(dir.in3,"Roving_Monitor_Priority_All_Lists_Region6_MaySept.xlsx", sep=""), sheet = 4)

plist = bind_rows(BCP_OctDec,
                  FFP_OctDec,
                  R1P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R1P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R1P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R1P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"),
                  R2P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R2P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R2P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R2P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"),
                  R3P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R3P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R3P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R3P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"),
                  R4P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R4P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R4P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R4P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"),
                  R5P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R5P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R5P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R5P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"),
                  R6P_FF_May_Jun %>% mutate(region = 1, startmonth = 5, endmonth = 6, Fishery = "Finfish"),
                  R6P_FF_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Finfish"),
                  R6P_BC_Apr_Jun %>% mutate(region = 1, startmonth = 4, endmonth = 6, Fishery = "Blue Crab"),
                  R6P_BC_Jul_Sep %>% mutate(region = 1, startmonth = 7, endmonth = 9, Fishery = "Blue Crab"))
rm(R1P_FF_May_Jun,R1P_FF_Jul_Sep,R1P_BC_Apr_Jun,R1P_BC_Jul_Sep,
   R2P_FF_May_Jun,R2P_FF_Jul_Sep,R2P_BC_Apr_Jun,R2P_BC_Jul_Sep,
   R3P_FF_May_Jun,R3P_FF_Jul_Sep,R3P_BC_Apr_Jun,R3P_BC_Jul_Sep,
   R4P_FF_May_Jun,R4P_FF_Jul_Sep,R4P_BC_Apr_Jun,R4P_BC_Jul_Sep,
   R5P_FF_May_Jun,R5P_FF_Jul_Sep,R5P_BC_Apr_Jun,R5P_BC_Jul_Sep,
   R6P_FF_May_Jun,R6P_FF_Jul_Sep,R6P_BC_Apr_Jun,R6P_BC_Jul_Sep,
   BCP_OctDec,
   FFP_OctDec)

# add DNR ID for those with license
ind = dplyr::select(WM, DNRID, License) %>% distinct()
plist = left_join(plist, ind, by = "License") %>% 
  mutate(DNRID = ifelse(!is.na(DNRid), DNRid, DNRID)) %>%
  dplyr::select(-DNRid, -License, -Date, -Region, -region, -FisherName) %>% 
  distinct() %>% 
  filter(!is.na(DNRID))

# ---------- #
# join with WM and RM tables
# ---------- #
WM2 = WM %>% dplyr::select(TripID, DNRID, Date, SH, EH, Fishery) %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  mutate(mo = month(Date)) %>% 
  dplyr::select(-SH, -EH, -lastH) %>% 
  distinct() 

# cycle through month ranges and fishery
WM2_BC = WM2 %>% filter(Fishery %in% "Blue Crab")
plist_BC = plist %>% filter(Fishery %in% "Blue Crab") 

WM2_FF = WM2 %>% filter(Fishery %in% "Finfish")
plist_FF = plist %>% filter(Fishery %in% "Finfish") 

# ---------- #

# ---------- #
# summarize
# ---------- #
# ---------- #


