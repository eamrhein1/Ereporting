# Table 3. Trip Summary for Roving Monitors January to December 2019 for time blocks they were working

#RM = RM %>% mutate(hr = hour(Time))

#import Ryan's work table (edited by Carly)
RM_Schedules <- read_excel("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Pilot Projects/Roving Monitor Pilot/Documentation/Final Report/RM Schedules - CT.xlsx")
#RM_Schedules$EndHour[RM_Schedules$twohrShift %in% "Y"] = RM_Schedules$StartHour[RM_Schedules$twohrShift %in% "Y"] + 2
RM_Schedules = mutate(RM_Schedules, 
                      StartHour = as.POSIXct(paste(paste(Date, StartHour, sep = " "), "00:00", sep=":"), format = "%Y-%m-%d %H:%M:%S", tz = "EST"),
                      EndHour = as.POSIXct(paste(paste(Date, EndHour, sep = " "), "00:00", sep=":"), format = "%Y-%m-%d %H:%M:%S", tz = "EST")) %>% 
  filter(!twohrShift %in% "Y") %>%
  mutate(Region = replace(Region, Region %in% "1B", "1b"),
         Region = replace(Region, Region %in% "1A", "1a"))

# find landing times that occurred in the time block
r1split = read.csv("//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/FACTSdata/output/R1split/new_regions_r1split_3and4combo.csv")
r1split = r1split %>% filter(region %in% c("1a","1b"))

finalLT = WM %>% dplyr::select(TripID, SH, EH, EHLandingTime, Date, region, EHZip) %>% 
  distinct() %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  mutate(time = sapply(strsplit(as.character(EHLandingTime), " "), tail, 1),
         landingtime = as.POSIXct(paste(Date, time, sep = " "), format = "%Y-%m-%d %H:%M:%S")) %>%
  dplyr::select(-SH,-EH,-lastH,-time,-EHLandingTime) %>%
  rename(zipcode = EHZip) %>%
  left_join(., filter(r1split, region %in% "1a") %>% mutate(region = "1a") %>% rename(r1a = region), by = "zipcode") %>% 
  left_join(., filter(r1split, region %in% "1b") %>% mutate(region = "1b") %>% rename(r1b = region), by = "zipcode") 
#rm(r1split)

# correct for r1 split
finalLT$region[finalLT$region %in% 1 & 
                 finalLT$Date >= min(RM_Schedules$Date[RM_Schedules$Region %in% "1b"]) & 
                 finalLT$Date <= max(RM_Schedules$Date[RM_Schedules$Region %in% "1b"]) &
                 finalLT$r1b %in% "1b"] = "1b"
finalLT$region[finalLT$region %in% 1 & 
                 finalLT$Date >= min(RM_Schedules$Date[RM_Schedules$Region %in% "1a"]) & 
                 finalLT$Date <= max(RM_Schedules$Date[RM_Schedules$Region %in% "1a"]) &
                 finalLT$r1a %in% "1a"] = "1a"

in.time.block <- function(x){
  y = finalLT %>% filter(region %in% x$Region,
                         landingtime > x$StartHour,
                         landingtime < x$EndHour) %>%
    summarise(n=n())
  return(y$n)
}

RM_Schedules$Ntrips = NA
for(a in 1:dim(RM_Schedules)[1]){
  RM_Schedules$Ntrips[a] = in.time.block(RM_Schedules[a,])
}

# correct RM data for r1split
RM3 = RM %>%  
  left_join(., dplyr::select(WM, EHZip, TripID) %>% rename(zipcode = EHZip), by = "TripID") %>%
  left_join(., filter(r1split, region %in% "1a") %>% mutate(region = "1a") %>% rename(r1a = region), by = "zipcode") %>% 
  left_join(., filter(r1split, region %in% "1b") %>% mutate(region = "1b") %>% rename(r1b = region), by = "zipcode") 
RM3$region[RM3$region %in% 1 & 
             RM3$Date >= min(RM_Schedules$Date[RM_Schedules$Region %in% "1b"]) & 
             RM3$Date <= max(RM_Schedules$Date[RM_Schedules$Region %in% "1b"]) &
             RM3$r1b %in% "1b"] = "1b"
RM3$region[RM3$region %in% 1 & 
             RM3$Date >= min(RM_Schedules$Date[RM_Schedules$Region %in% "1a"]) & 
             RM3$Date <= max(RM_Schedules$Date[RM_Schedules$Region %in% "1a"]) &
             RM3$r1a %in% "1a"] = "1a"


# create table
tripSummary = as.data.frame(matrix(data = NA, ncol=7, nrow=7))
names(tripSummary) = c("Regions","AvailTrips","AttemptedTrips","SuccessfulTrips","AvailWM","AttemptedWM","SuccessfulWM")
tripSummary$Regions = c("1","1a","1b","2","5","6","Total")

x = RM_Schedules %>% group_by(Region) %>% summarise(n=sum(Ntrips))
tripSummary$AvailTrips[1:6] = x$n

xx = RM3 %>% dplyr::select(region, TripID) %>% distinct() %>% 
  group_by(region) %>% summarise(n=n()) %>% filter(!region %in% c(3,4,NA))
tripSummary$AttemptedTrips[1:6] = paste(formatC((xx$n/x$n)*100,digits = 4),"% (n = ",xx$n,")", sep = "")

xxx = RM3 %>% dplyr::select(region, TripID, Result) %>% distinct() %>% 
  filter(Result %in% c("MONITORED", "MONITORED (on paper)")) %>% 
  group_by(region) %>% summarise(n=n()) %>% filter(!region %in% c(3,4,NA))
tripSummary$SuccessfulTrips[1:6] = paste(formatC((xxx$n/x$n)*100,digits = 4),"% (n = ",xxx$n,")", sep = "")

tripSummary[tripSummary$Regions %in% "Total",2:7] = c(prettyNum(sum(RM_Schedules$Ntrips), big.mark = ","), 
                                                      paste(formatC((sum(xx$n)/sum(RM_Schedules$Ntrips))*100, digits = 4), "% (n = ",prettyNum(sum(xx$n), big.mark = ","), ")", sep = ""),
                                                      paste(formatC((sum(xxx$n)/sum(RM_Schedules$Ntrips))*100, digits = 4), "% (n = ",prettyNum(sum(xxx$n), big.mark = ","), ")", sep = ""),
                                                      NA,
                                                      NA,
                                                      NA) 

xTable =  htmlTable(tripSummary, rnames = FALSE,
                    caption="Table 3. Trip Summary for Roving Monitors January to December 2019 within Time Blocks",
                    header =  c("Region",
                                "Total Available Trips",	
                                "Attempted Trips",	
                                "Successful Trips Monitored",	
                                "Num. of Available Watermen",	
                                "Num. of Ind. Watermen Attempted to be Monitored",
                                "Num. of Ind. Watermen Successfully Monitored"),
                    n.rgroup = c(6,1),
                    align = "lc",
                    align.header = "lccc",
                    css.cell = rbind(rep("font-size: 1.1em; padding-right: 0.6em", 
                                         times=7), matrix("font-size: 1.1em; padding-right: 0.6em", ncol=7, nrow=7)),
                    css.table = "margin-top: 1em; margin-bottom: 1em; table-layout: fixed; width: 1000px",
                    total = "tspanner",
                    css.total = c("border-top: 1px solid grey; font-weight: 900"),
                    n.tspanner = c(nrow(tripSummary)))
xTable 

write.table(xTable, 
            file=paste(dir.out, "Table3_TripSumInTBs.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

rm(RM3, TripSummary)
