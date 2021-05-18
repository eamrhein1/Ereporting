# --------------- #
# This script is to evaluate call wait times for the helpline
# The Call Detail Report from TeleRep covers from 01 January 2019 until Aug. 14, 2019.
# --------------- #

# --------------- #
# load packages
# --------------- #
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(chron)
# --------------- #

# --------------- #
# set directories
# --------------- #
dir.in = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/TeleRepData/rawdata/"
dir.out = "//orp-dc01/Users/ORP Operations/Fisheries Program/E-Reporting/4.0 Pilot projects/Data/TeleRepData/output/"
# --------------- #

# --------------- #
# load data
# --------------- #
BC <- read_excel(paste(dir.in, "BlueCrab980_Aug2019.xls", sep=""), skip = 3, col_names = TRUE)

# fix data
BC = BC[,!grepl("[...]",names(BC))] # remove rows without names (e.g. ...7)
BC = BC[complete.cases(BC), ] # remove rows will all NAs
BC = BC[!BC$DateTime %in% "DateTime",] # remove extra header rows
BC = BC %>% 
  mutate(DateTime = as.POSIXct(as.numeric(BC$DateTime) * (60*60*24), origin="1899-12-30", tz="GMT"),# convert excel date time
         Ring = as.numeric(Ring),
         Hold = as.numeric(Hold),
         time = chron(times = format(DateTime, format="%H:%M:%S")),
         date = as.POSIXct(format(DateTime, format="%m/%d/%Y"), format="%m/%d/%Y"),
         Dur = as.numeric(Dur),
         Talk = as.numeric(Talk))
            
# percents
BC_perc = BC %>% dplyr::select(DateTime, date, time, Hold) %>%
  mutate(time_block = NA,
         time_block = replace(time_block, Hold %in% 0, "0 min."),
         time_block = replace(time_block, Hold >0 & Hold <=1, ">0 min. to 1 min."),
         time_block = replace(time_block, Hold >1 & Hold <=2, ">1 min. to 2 min."),
         time_block = replace(time_block, Hold >2 & Hold <=3, ">2 min. to 3 min."),
         time_block = replace(time_block, Hold > 3, "greater than 3 min.")) %>% 
  group_by(time_block) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),
         reord = NA,
         reord = replace(reord, time_block %in% "0 min.", 1),
         reord = replace(reord, time_block %in% ">0 min. to 1 min.", 2),
         reord = replace(reord, time_block %in% ">1 min. to 2 min.", 3),
         reord = replace(reord, time_block %in% ">2 min. to 3 min.", 4),
         reord = replace(reord, time_block %in% "greater than 3 min.", 5))
# --------------- #

# --------------- #
# plots
# --------------- #
# hold perc. freq.
p = ggplot() + 
  geom_histogram(data = BC_perc, aes(x = reorder(time_block, reord), y= freq), stat = "identity") + 
  geom_text(data = BC_perc, aes(x = reorder(time_block, reord), y= freq, label = signif(freq*100, digits = 3)), size = 5, vjust = -0.8) + 
  theme_bw() + 
  labs(x = "time bin", y = "Relative Frequency") + 
  theme(text = element_text(size = 15)) 
p
ggsave(paste(dir.out,"Hold_freq.png",sep = ""), p)

# Ring
p = ggplot() + geom_point(data = BC, aes(x = DateTime, y = Ring)) +
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Date", y = "Ring (minutes)")+
  ggtitle("Ring Time")
p
ggsave(paste(dir.out,"Rind_by_date.png",sep = ""), p)

# Ring + time of day
p = ggplot() + geom_point(data = BC, aes(x = time, y = Ring)) +
  geom_smooth(method='lm')+
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Time", y = "Ring (minutes)")+
  ggtitle("Ring Time by time of day")
p
ggsave(paste(dir.out,"Ring_by_time.png",sep = ""), p)

# Hold
p = ggplot() + geom_point(data = BC, aes(x = DateTime, y = Hold)) +
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Date", y = "Hold (minutes)")+
  ggtitle("Hold Time")
p
ggsave(paste(dir.out,"Hold_by_date.png",sep = ""), p)

# Hold + time of day
p = ggplot() + geom_point(data = BC, aes(x = time, y = Hold)) +
  geom_smooth(method='lm')+
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Time", y = "Hold (minutes)")+
  ggtitle("Hold Time by time of day")
p
ggsave(paste(dir.out,"Hold_by_time.png",sep = ""), p)

# hold by hour
p = ggplot() + 
  geom_boxplot(data = BC, aes(x = hours(time), y = Hold, fill = as.character(hours(time)), group = hours(time))) +
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(x = "Hour", y = "Hold time (minutes)")+
  ggtitle("Hold time by hour")
p
ggsave(paste(dir.out,"Hold_by_hour.png",sep = ""), p)


# Duration
p = ggplot() + 
  geom_point(data = BC, aes(x = DateTime, y = Dur)) +
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Date", y = "Duration (minutes)")+
  ggtitle("Duration of calls")
p
ggsave(paste(dir.out,"Duration_by_date.png",sep = ""), p)

# Duration + time of day
p = ggplot() + geom_point(data = BC, aes(x = time, y = Dur)) +
  geom_smooth(method='lm')+
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Time", y = "Duration (minutes)")+
  ggtitle("Duration of call by time of day")
p
ggsave(paste(dir.out,"Duration_by_time.png",sep = ""), p)

# duration by hour
p = ggplot() + 
  geom_boxplot(data = BC, aes(x = hours(time), y = Dur, fill = as.character(hours(time)), group = hours(time))) +
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(x = "Hour", y = "Duration (minutes)")+
  ggtitle("Duration of call by hour")
p
ggsave(paste(dir.out,"Duration_by_hour.png",sep = ""), p)


# Talk time
p = ggplot() + 
  geom_point(data = BC, aes(x = DateTime, y = Talk)) +
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Date", y = "Talk time (minutes)")+
  ggtitle("Talk time on calls")
p
ggsave(paste(dir.out,"TalkTime_by_date.png",sep = ""), p)

# talk time by hours
p = ggplot() + 
  geom_boxplot(data = BC, aes(x = hours(time), y = Talk, fill = as.character(hours(time)), group = hours(time))) +
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(x = "Hour", y = "Talk time (minutes)")+
  ggtitle("Talk time on calls")
p
ggsave(paste(dir.out,"TalkTime_by_hour.png",sep = ""), p)

# talk time + time of day
p = ggplot() + geom_point(data = BC, aes(x = time, y = Talk)) +
  geom_smooth(method='lm')+
  theme_bw()+
  theme(text = element_text(size = 20)) +
  labs(x = "Time", y = "Talk time (minutes)")+
  ggtitle("Talk time on calls by time of day")
p
ggsave(paste(dir.out,"TalkTime_by_time.png",sep = ""), p)

# comparitive box plot
BC2 = BC %>% dplyr::select(Ring, Hold, Talk, Dur) %>% 
  rename(Duration = Dur) %>% 
  tidyr::gather() %>% 
  mutate(ordering = ifelse(key %in% "Ring", 1, ifelse(key %in% "Hold", 2, ifelse(key %in% "Talk",3,4)))) 
  
p = ggplot() + 
  geom_boxplot(data = BC2, aes(x = reorder(key, ordering), y = value, fill = key))+
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.position = "none") + 
  labs(x = "Type", y = "Time (minutes)")
p
# --------------- #
