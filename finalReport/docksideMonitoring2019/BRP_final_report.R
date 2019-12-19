# figure out who was actually following best reporting practices throuhg a loose ranking system

# this is run as part of final_report_analyses and is not meant to be run alone
# to load data you lines 1-51 in final_report_analyses_2019.R

# BRP outline
# 1. SH 1 hour earlier than EH
# 2. last SH at least half an hour earlier than Landing Time
# 3. last SH address = EH address
# 4. RM report success on first try +1 point
# 5. On Jen's call list -1 point

# import Jen's data
brp_calls <- read_excel("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/Documentation/Final Report/Outreach_Call_Details.xlsx")
names(brp_calls) = gsub(" ", "", names(brp_calls), fixed = TRUE)
brp_calls = dplyr::select(brp_calls, DNRID, LicenseNumber, WatermanName) %>% distinct() %>% 
  mutate(called = -1, License = as.numeric(LicenseNumber))

# BRP_SumTbl = WM %>% dplyr::select(WatermenName, License) %>% distinct() %>% 
#   left_join(., dplyr::select(brp_calls, License, called), by = "License")
BRP_SumTbl = WM %>% dplyr::select(WatermenName, DNRID) %>% 
  left_join(., dplyr::select(brp_calls, DNRID, called), by = "DNRID") %>% distinct()  
  
# sort those that were monitored on their first try
# x = RM %>% dplyr::select(TripID, Result) %>% distinct() %>% 
#   left_join(., dplyr::select(WM, TripID, DNRID, Date), by = "TripID") %>% 
#   mutate(mo = month(Date)) %>%
#   group_by(DNRID, mo) %>% 
#   summarise(n=n()) %>% arrange(-n) %>%
#   mutate(mo2 = mo, 
#          mo2 = replace(mo2, mo %in% 2, "February"),
#          mo2 = replace(mo2, mo %in% 3, "March"),
#          mo2 = replace(mo2, mo %in% 6, "June"),
#          mo2 = replace(mo2, mo %in% 7, "July"),
#          mo2 = replace(mo2, mo %in% 8, "August"),
#          mo2 = replace(mo2, mo %in% 9, "September"),
#          mo2 = replace(mo2, mo %in% 10, "October"),
#          mo2 = replace(mo2, mo %in% 11, "November"),
#          mo2 = replace(mo2, mo %in% 12, "December"))
# ggplot() + geom_boxplot(data = filter(x, !is.na(mo)), aes(x = reorder(mo2,mo), y = n)) + 
#   labs(x = "Month", y = "Number of Times an Attempt was made to Monitor an Individual per month")


brp_fsuccess = RM %>% dplyr::select(TripID, Result) %>% 
  left_join(., dplyr::select(WM, TripID, DNRID, Date), by = "TripID") %>% 
  distinct() %>% 
  mutate(jday = format(Date, "%j"),
         success = ifelse(Result %in% c("MONITORED","MONITORED (on paper)"), "yes", "no")) %>% 
  group_by(DNRID) %>% arrange(jday) %>%
  summarise(fsuccess = first(success)) %>%
  filter(fsuccess %in% "yes") %>%
  mutate(fsuccess = 1)
BRP_SumTbl = left_join(BRP_SumTbl, brp_fsuccess, by = "DNRID")

# SH 1 hour earlier than EH
brp_SH = WM %>% dplyr::select(TripID, SH, EH, SHSubmittedTime, EHSubmittedTime, DNRID) %>% 
  distinct() %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  mutate(timediff = difftime(as.POSIXlt(EHSubmittedTime), as.POSIXlt(SHSubmittedTime)) /60 /60) %>%
  filter(timediff < 1.000001 & timediff > (-0.0001)) %>%
  mutate(SH_error = -1) %>% dplyr::select(DNRID, SH_error) %>% distinct()
BRP_SumTbl = left_join(BRP_SumTbl, brp_SH, by = "DNRID")

# last SH at least half an hour earlier than Landing Time
brp_SH2 = WM %>% dplyr::select(TripID, SH, EH, SHSubmittedTime, SHLandingTime, DNRID) %>% 
  distinct() %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  mutate(timediff = difftime(as.POSIXlt(SHLandingTime), as.POSIXlt(SHSubmittedTime)) /60) %>%
  filter(timediff < 30.000001 & timediff > (-0.0001)) %>%
  mutate(SH_error2 = -1) %>% dplyr::select(DNRID, SH_error2) %>% distinct()
BRP_SumTbl = left_join(BRP_SumTbl, brp_SH2, by = "DNRID")

# last SH address = EH address
brp_address = WM %>% dplyr::select(TripID, SH, EH, SHAddress, EHAddress, DNRID) %>% 
  distinct() %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  filter(!SHAddress %in% EHAddress) %>% 
  mutate(Address_error = -1) %>% dplyr::select(DNRID, Address_error) %>%
  distinct()
BRP_SumTbl = left_join(BRP_SumTbl, brp_address, by = "DNRID")

BRP_SumTbl[is.na(BRP_SumTbl)] <- 0

mutate(BRP_SumTbl, score = called + fsuccess + SH_error + SH_error2 + Address_error) %>%
  group_by(score) %>% summarise(n=n())

  

