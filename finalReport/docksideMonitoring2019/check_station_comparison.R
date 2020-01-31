# read data from final_report_analyses_2019.R

require(gridExtra)
library(readxl)


# load check station data
# data from https://jira.fisheryfacts.com/browse/FACTSMD-711?jql=
CS <- read_excel("U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/RMChkStns.xlsx")
CS2 <- read_excel("U:/O365_NEW_ Operations/Sustainable Fisheries/E-Reporting/Data/FACTSdata/rawdata/ChkStns3Trips.xlsx")
names(CS2) = names(CS)

CS = rbind (CS, CS2) %>% rename(TripID = TRIP_NO)

# compare RM SB catch to check stations
RM_sub = RM %>% filter(TripID %in% CS$TripID, SpeciesGrade %in% "STRIPED BASS") %>% 
  group_by(TripID) %>%  
  mutate(lastH = ifelse(MonitorReportNum %in% max(MonitorReportNum), "yes","no")) %>%
  filter(lastH %in% "yes") %>% 
  summarise(Count = sum(Count), 
            Weight = sum(Quantity)) %>%
  mutate(Count = ifelse(TripID %in% 665950, 150, Count),
         Count = ifelse(TripID %in% 666057, 144, Count))

RM_com = RM %>% filter(TripID %in% CS$TripID, SpeciesGrade %in% "STRIPED BASS") %>% 
  group_by(TripID) %>%  
  mutate(lastH = ifelse(MonitorReportNum %in% max(MonitorReportNum), "yes","no")) %>%
  filter(lastH %in% "yes") %>% 
  dplyr::select(Comments)

RM_CS = left_join(RM_sub, CS, by = "TripID") %>% 
  mutate(countDiff = Count.x - as.numeric(Count.y),
         weightDiff = Weight.x - as.numeric(Weight.y)) %>% 
  filter(!TripID %in% 665356) #already offloaded

plot1 <- ggplot() + geom_histogram(data = RM_CS, aes(x = countDiff, stat = "count")) + 
  theme_bw() + theme(text = element_text(size = 20))+
  labs(x = "Difference in count", y = "Number of Occurrences")
plot2 <- ggplot() + geom_histogram(data = RM_CS, aes(x = weightDiff, stat = "count")) + 
  theme_bw()+ theme(text = element_text(size = 20)) + 
  labs(x = "Difference in weight (lbs)", y = "Number of Occurrences")

grid.arrange(plot1, plot2, ncol=2)
ggsave(paste(dir.out, "RM_CS.png", sep=""), arrangeGrob(plot1, plot2, ncol = 2))


# compare WM SB catch to check stations
WM_sub = WM %>% filter(TripID %in% CS$TripID, SpeciesGrade %in% "STRIPED BASS") %>%
  rename(Weight = Quantity)

WM_CS = left_join(WM_sub, CS, by = "TripID")  %>% 
  group_by(TripID) %>%  
  mutate(lastH = ifelse(EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% 
  mutate(countDiff = Count.x - as.numeric(Count.y),
         weightDiff = Weight.x - as.numeric(Weight.y))

plot1 <- ggplot() + geom_histogram(data = WM_CS, aes(x = countDiff, stat = "count")) + 
  theme_bw() + theme(text = element_text(size = 20))+
  labs(x = "Difference in count", y = "Number of Occurrences")
plot2 <- ggplot() + geom_histogram(data = WM_CS, aes(x = weightDiff, stat = "count")) + 
  theme_bw()+ theme(text = element_text(size = 20)) + 
  labs(x = "Difference in weight (lbs)", y = "Number of Occurrences")

grid.arrange(plot1, plot2, ncol=2)
ggsave(paste(dir.out, "WM_CS.png", sep=""), arrangeGrob(plot1, plot2, ncol = 2))

# compare all three
RM_WM_CS = left_join(RM_CS, dplyr::select(WM_CS, TripID, Count.x, Weight.x), by = "TripID") %>% 
  rename(RMcount = Count.x.x,
         WMcount = Count.x.y,
         CScount = Count.y,
         RMweight = Weight.x.x,
         WMweight = Weight.x.y,
         CSweight = Weight.y,) %>% 
  dplyr::select(-countDiff, -weightDiff)

