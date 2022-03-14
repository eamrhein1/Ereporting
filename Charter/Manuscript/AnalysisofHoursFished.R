 ##Number of Hours Fished##
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
  
  #Add time column 
  datpilot$Triptime <- difftime(datpilot$EHLandingTime, datpilot$SHSubmittedTime, tz="EST", units = "hours")
  
  
 # min(datpilot$Triptime)
 # max(datpilot$Triptime)
  median(datpilot$Triptime)
 # mean(datpilot$Triptime)
 # sd(datpilot$Triptime)
