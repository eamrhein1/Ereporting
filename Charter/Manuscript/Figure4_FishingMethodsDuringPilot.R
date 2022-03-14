##Plot for fishing method##
# load packages
# -------------------- #
library(dplyr)
library(ggplot2)
library(readr)
# -------------------- #
dat2021 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522.csv")
dat2020 = read_csv("C:/Users/Eric Amrhein/Downloads/CharterTrips_021522_2.csv")
#------------------ #
datpilot <- rbind(dat2020, dat2021)

# -------------------- #
# load data
# -------------------- #

# -------------------- #


# -------------------- #
# manipulate data
# -------------------- #
# filter so that the trip data is only last report
names(datpilot) = gsub(" ","", names(datpilot))
names(datpilot) = gsub("#","", names(datpilot))

datpilot = datpilot %>%
  group_by(TripID) %>%
  mutate(lastSH = ifelse(SH == max(SH),"yes","no"),
         lastEH = ifelse(EH == max(EH),"yes","no")) %>%
  filter(lastSH %in% "yes" & lastEH %in% "yes") %>%
  dplyr::select(-lastSH, -lastEH) %>%
  mutate(Disposition = replace(Disposition, Disposition %in% c("Kept - fileted"),"Kept")) %>%
  group_by(TripID, Disposition, Species) %>%
  mutate(Count = sum(Count),
         Quantity = sum(Quantity)) # add kept and kept filleted numbers together

  dat_sum = datpilot %>% group_by(GearMethod, TripID) %>%
  summarise(ct = sum(Count))
  
  # check if live lining was most popular fishing method#
  datpilot %>% filter(GearMethod %in% "Live Lining") %>%
    group_by(GearMethod) %>% 
    summarise(n=n())
  
  #Check if trolling was most popular fishing method#
  datpilot %>% filter(GearMethod %in% "Trolling") %>%
    group_by(GearMethod) %>% 
    summarise(n=n())
  
  #Check if bottom fishing was most popular fishing method#
  datpilot %>% filter(GearMethod %in% "Bottom Fishing") %>%
    group_by(GearMethod) %>% 
    summarise(n=n())
  
  # plot
  # -------------------- #
  p = ggplot() + 
    geom_bar(data = dat_sum, aes(x=GearMethod), stat="count") + 
    theme_bw() + 
    theme(text = element_text(size = 20))+
    labs(x="Fishing Method", y="Number of Trips") 
  p
  
  # export
  ggsave("C/Figure4_Fishing_Method.png",p)
  # -------------------- # 
