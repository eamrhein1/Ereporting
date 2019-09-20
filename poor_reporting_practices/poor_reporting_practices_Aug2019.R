# -------------------- # 
# this script is to determine who is using poor reporting practices
# and if any group is more likely than another
#
# bins:
#   1.	Time in the Program
#     1.	Brand New (say, less than a month)
#     2.	Less than a year
#     3.	Greater than one year
#   2.	Training
#     1.	Trained in Person (class or individual appointment)
#     2.	Expo - This is a sub group to consider, where new users are trained one-on-one in a condensed format at the FACTS booth in OC.
#     3.	Mentored
#     4.	Online Training
# -------------------- # 

# -------------------- # 
# load packages
# -------------------- # 
require(dplyr)
require(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)
# -------------------- # 

# -------------------- #
# set directories
# -------------------- # 
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/FACTSdata/output/poor_reporting_practices/"
# -------------------- # 

# -------------------- #
# load data
# -------------------- # 
# load fishing data
RM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 1)
WM <- read_excel(paste(dir.in,"Aug2019_RM_WM.xlsx", sep=""), sheet = 2)
permits = read_excel(paste(dir.in,"2019 Permits to Date_Kaycee.xlsx",sep=""))
WM_2018 <- read_excel(paste(dir.in, "WatermenData041819.xlsx", sep=""), sheet = 1)

# take spaces out of names
names(RM) = gsub(" ", "", names(RM), fixed = TRUE)
names(WM) = gsub(" ", "", names(WM), fixed = TRUE)
names(WM_2018) = gsub(" ", "", names(WM_2018), fixed = TRUE)
permits = mutate(permits, wm = toupper(Name))

# rename
WM_2018 = WM_2018 %>% rename(Date = TripDate, WatermenName = FisherName)

# needs to be changed in the data
RM = RM %>% mutate(AssignedMonitor = replace(AssignedMonitor, TripID %in% c(565820, 569269, 569582, 574640, 
                                                                            578963, 569640, 569665, 579730,
                                                                            566638, 584714, 584748, 584813, 
                                                                            588244), "Becky Rusteberg K"),
                   AssignedMonitor = replace(AssignedMonitor, TripID %in% c(582379, 582924, 583278, 585968), "Steve Harris Womack"))

r = RM %>% group_by(TripID) %>% summarise(date = first(AssignedMonitor),
                                          result = first(Result))
w = WM %>% group_by(TripID) %>% summarise(date = first(Date), wm = first(WatermenName))
rw = left_join(r,w,by="TripID")
names(rw) = c("tripid","rm","type","date","wm")
rm(r,w)
rw = rw %>% mutate(date = as.Date(date), mo = month(date)) 

permits = permits %>% dplyr::select(wm, Comments) %>% 
  mutate(training_type = "In person training",
         training_type = replace(training_type, Comments %in% c("Expo","Expo - Paper","Expo - renewal"),"Expo"),
         training_type = replace(training_type, Comments %in% c("Mentor","Mentor - Carol Dobson","Mentor - Gary S.",
                                              "Mentor - Need HRV since April contact" ,"Mentor - Richie Duff",
                                              "Mentor - S. Retallick","Mentor - Sherry Wilson",
                                              "Mentor -Tony Vicari","Mentoring"), "Mentoring"),
         training_type = replace(training_type, Comments %in% c("Mentored with Troy Weakley"), "Mentored"),
         training_type = replace(training_type, Comments %in% c("Online Training"),"Online Training"),
         training_type = replace(training_type, Comments %in% c("Dundalk","Dundalk Training",
                                              "Dundalk Training - Permitting UI glitch" ), "In person training"))
# -------------------- #

# -------------------- #
# analysis
# -------------------- #
#   1.	Time in the Program
#     1.	Brand New (say, less than a month)
#     2.	Less than a year
#     3.	Greater than one year
w = rbind(dplyr::select(WM, TripID, Date, WatermenName), dplyr::select(WM_2018, TripID, Date, WatermenName)) %>%
  group_by(TripID) %>% 
  summarise(date = first(Date), 
            wm = first(WatermenName)) %>% 
  mutate(mo = month(date)) %>% 
  group_by(wm) %>% summarise(max_dt = max(date), min_dt = min(date), 
                             max_mo = max(mo), min_mo = min(mo)) %>%
  mutate(mo_diff = difftime(max_dt, min_dt, units = "days"),
         grouping = NA, 
         grouping = replace(grouping, mo_diff < 31, "less than a month"),
         grouping = replace(grouping, mo_diff < 366 & mo_diff > 30, "greater than a month and less than a year"),
         grouping = replace(grouping, mo_diff > 365, "greater than 1 year"))
  
p = ggplot() + geom_histogram(data = w, aes(x = grouping), stat = "count") + 
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  ggtitle("All reports")
p 
ggsave(paste(dir.out, "Time_in_program_all_reports.png", sep=""), p)

not_mon_age = rw %>% left_join(.,w, by = "wm") %>% 
  filter(!type %in% c("MONITORED","MONITORED (on paper)"),
         !is.na(grouping))

p = ggplot() + geom_histogram(data = not_mon_age, aes(x = grouping), stat = "count") + 
  theme_bw() +
  theme(text = element_text(size = 20))+ 
  ggtitle("Not monitored reports only")
p
ggsave(paste(dir.out, "Time_in_program_not_monitored_reports.png", sep=""), p)

#percent by grouping
ww = w %>% group_by(grouping) %>% 
  summarise(n=n()) %>% 
  left_join(., not_mon_age %>% group_by(grouping) %>% 
              summarise(n=n()), by="grouping") %>% 
  mutate(perc = (n.y/n.x)*100)

p = ggplot() + geom_histogram(data = ww, aes(x = grouping, y = perc), stat = "identity") + 
  theme_bw() +
  theme(text = element_text(size = 20))+ 
  ggtitle("Percent of reports not monitored (not monitored/all) per group")+
  labs(y="Percent")
p
ggsave(paste(dir.out, "Time_in_program_not_monitored_reports_perc.png", sep=""), p)


#   2.	Training
#     1.	Trained in Person (class or individual appointment)
#     2.	Expo - This is a sub group to consider, where new users are trained one-on-one in a condensed format at the FACTS booth in OC.
#     3.	Mentored
#     4.	Online Training
t = rw %>% left_join(.,permits, by = "wm") %>% 
  filter(!type %in% c("MONITORED","MONITORED (on paper)")) %>%
  mutate(training_type = replace(training_type, is.na(training_type),"In person training")) %>% 
  filter(!is.na(wm)) %>%
  group_by(wm) %>%
  summarise(training_type = first(training_type)) %>%
  group_by(training_type) %>% 
  summarise(n=n())
         # reord = NA,
         # reord = replace(reord, training_type %in% "In person training", 1),
         # reord = replace(reord, training_type %in% "Online Training", 2),
         # reord = replace(reord, training_type %in% "Expo", 3),
         # reord = replace(reord, training_type %in% "Mentoring", 4))

p = ggplot(data = t, aes(x = reorder(training_type, n), y = n)) + 
  geom_histogram(stat="identity") + 
  geom_label(aes(label = n), size =10) +
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  labs(x = "Training Type", y = "Count") + 
  ggtitle("Unique watermen who occurred in not monitored reports Jan.-Aug.")
p
ggsave(paste(dir.out, "TrainingType.png", sep=""), p)
# -------------------- #
