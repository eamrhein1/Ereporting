library(readr)
library(ggplot2)
library(dplyr)

dat <- read_csv("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/VerizonData/Analyze phone charges052021.csv", skip=13)
dat = as.data.frame(dat)
names(dat) = gsub(" ", "", names(dat))

dat = dat %>% 
  dplyr::select(Billdate, Minutes) %>%
  dplyr::filter(!is.na(Billdate), !Billdate %in% "05/01/2021") %>%
  mutate(month = sapply(strsplit(Billdate,"/"), head, 1),
         mo = month,
         mo = replace(mo, month %in% "01", "Jan."),
         mo = replace(mo, month %in% "02", "Feb."),
         mo = replace(mo, month %in% "03", "Mar."),
         mo = replace(mo, month %in% "04", "Apr."),
         mo = replace(mo, month %in% "05", "May"),
         mo = replace(mo, month %in% "06", "Jun."),
         mo = replace(mo, month %in% "07", "Jul."),
         mo = replace(mo, month %in% "08", "Aug."),
         mo = replace(mo, month %in% "09", "Sep."),
         mo = replace(mo, month %in% "10", "Oct."),
         mo = replace(mo, month %in% "11", "Nov."),
         mo = replace(mo, month %in% "12", "Dec.")) %>%
  arrange(month)

p = ggplot() + 
  geom_bar(dat=dat, aes(x=month, y=Minutes), stat="identity") + 
  geom_text(dat=dat, aes(x=month, y=Minutes+50, label=Minutes)) + 
  theme_bw() +
  scale_x_discrete(labels = dat$mo) +
  theme(text = element_text(size=20)) +
  labs(x="Month",y="Total Minutes",title="Helpline Minutes by Month \n(May-Dec. 2020, Jan.-Apr. 2021)")
p
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Sustainable Fisheries/E-Reporting/Data/VerizonData/HelplineAnnualMinutes2021.png",p)
