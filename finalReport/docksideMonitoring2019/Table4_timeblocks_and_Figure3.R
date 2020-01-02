# ------------------------ #
# landing hours in time blocks
# 7 to 13
# 11 to 17
# 15 to 21
# ------------------------ #
tmp = WM %>% dplyr::select(TripID, SH, EH, EHLandingTime, region, Date, Fishery) %>%
  distinct() %>% 
  group_by(TripID) %>%
  mutate(lastH = ifelse(SH %in% max(SH) & EH %in% max(EH), "yes","no")) %>%
  filter(lastH %in% "yes") %>% ungroup() %>%
  mutate(hr = hour(EHLandingTime),
         mo = month(Date)) 

hrs = group_by(tmp, hr) %>% 
  summarise(n=n()) %>% 
  arrange(hr) %>% ungroup()

tbs = as.data.frame(as.matrix(rbind(hrs %>% filter(hr %in% c(7:13)) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(11:17)) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(15:21)) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(7:21)) %>% summarize(sum(n)),
                                    hrs %>% filter(!hr %in% c(7:21)) %>% summarize(sum(n)))))
names(tbs) = c("n")
tbs$perc = NA
tbs$perc[1:4] = formatC((tbs$n[1:4]/length(unique(WM$TripID))*100), digits = 4)
tbs$perc[5] = formatC((tbs$n[5]/length(unique(WM$TripID))*100), digits = 3)

hrs = group_by(tmp, hr, Fishery) %>% 
  summarise(n=n()) %>% 
  arrange(hr) %>% ungroup()

tbs_BC = as.data.frame(as.matrix(rbind(hrs %>% filter(hr %in% c(7:13) & Fishery %in% c("Blue Crab")) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(11:17) & Fishery %in% c("Blue Crab")) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(15:21) & Fishery %in% c("Blue Crab")) %>% summarize(sum(n)),
                                    hrs %>% filter(hr %in% c(7:21) & Fishery %in% c("Blue Crab")) %>% summarize(sum(n)),
                                    hrs %>% filter(!hr %in% c(7:21) & Fishery %in% c("Blue Crab")) %>% summarize(sum(n)))))
names(tbs_BC) = c("n")
tbs_BC$perc = NA
tbs_BC$perc[1:4] = formatC((tbs_BC$n[1:4]/length(unique(WM$TripID[WM$Fishery %in% "Blue Crab"]))*100), digits = 4)
tbs_BC$perc[5] = formatC((tbs_BC$n[5]/length(unique(WM$TripID[WM$Fishery %in% "Blue Crab"]))*100), digits = 3)

tbs_FF = as.data.frame(as.matrix(rbind(hrs %>% filter(hr %in% c(7:13) & Fishery %in% c("Finfish")) %>% summarize(sum(n)),
                                       hrs %>% filter(hr %in% c(11:17) & Fishery %in% c("Finfish")) %>% summarize(sum(n)),
                                       hrs %>% filter(hr %in% c(15:21) & Fishery %in% c("Finfish")) %>% summarize(sum(n)),
                                       hrs %>% filter(hr %in% c(7:21) & Fishery %in% c("Finfish")) %>% summarize(sum(n)),
                                       hrs %>% filter(!hr %in% c(7:21) & Fishery %in% c("Finfish")) %>% summarize(sum(n)))))
names(tbs_FF) = c("n")
tbs_FF$perc = NA
tbs_FF$perc[1:4] = formatC((tbs_FF$n[1:4]/length(unique(WM$TripID[WM$Fishery %in% "Finfish"]))*100), digits = 4)
tbs_FF$perc[5] = formatC((tbs_FF$n[5]/length(unique(WM$TripID[WM$Fishery %in% "Finfish"]))*100), digits = 3)


tripSum = as.data.frame(as.matrix(cbind(c("7 a.m. - 1 p.m.","11 a.m. - 5 p.m.","3 - 9 p.m.","in block","out of block"),
                                        paste(tbs$perc, "% (n = ", prettyNum(tbs$n, big.mark = ","), ")", sep = ""),
                                        paste(tbs_BC$perc, "% (n = ", prettyNum(tbs_BC$n, big.mark = ","),")", sep = ""),
                                        paste(tbs_FF$perc, "% (n = ", prettyNum(tbs_FF$n, big.mark = ","),")", sep = ""))))
names(tripSum) = c("block", "TotalPerc","BCPrec","FFPrec")


xTable = htmlTable(tripSum, rnames = FALSE,
                   caption="Table 4. Percent of trips from January to December in 2019 by time block",
                   header =  c("Time block",
                               "All Trips",
                               "Blue Crab Trips",
                               "Finfish Trips"),
                   rgroup = c("By block",
                              "Summary"),
                   n.rgroup = c(3,2),
                   align = "lc",
                   align.header = "lccc",
                   css.cell = rbind(rep("font-size: 1.3em; padding-right: 0.5em", 
                                        times=4), matrix("font-size: 1.2em; padding-right: 0.5em", ncol=4, nrow=5)),
                   css.rgroup = "font-weight: 800; font-size: 1.2em;", 
                   css.rgroup.sep = "",
                   css.table = "margin-top: 1em; margin-bottom: 1em; table-layout: fixed; width: 800px;")
xTable 

write.table(xTable, 
            file=paste(dir.out, "TimeBlock2019.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

rm(tbs, tbs_BC, tbs_FF)

# ---- #
# by region
# --- #

# --- #
# by month
# --- #

# --- #
# by region and month
# --- #
hrs = group_by(tmp, hr, region, mo, Fishery) %>% 
  summarise(n=n()) %>% 
  arrange(hr) %>% ungroup() %>% 
  mutate(timeBlock = NA,
         timeBlock = ifelse(hr %in% c(7:13), "morning", timeBlock),
         timeBlock = ifelse(hr %in% c(11:17), "afternoon", timeBlock),
         timeBlock = ifelse(hr %in% c(15:21), "night", timeBlock))#,
         #hline_min = ifelse(timeBlock %in% "morning", 7, ifelse(timeBlock %in% "afternoon", 11, 15)),
         #hline_max = ifelse(timeBlock %in% "morning", 13, ifelse(timeBlock %in% "afternoon", 17, 21)))


x=hrs %>% filter(Fishery %in% "Blue Crab", !region %in% "undefined", !is.na(timeBlock)) #%>% 
  #group_by(timeBlock, mo, region) %>% summarise(n=sum(n))
xx=hrs %>% filter(Fishery %in% "Finfish", !region %in% "undefined", !is.na(timeBlock))  

# ggplot()+
#   #geom_point(data = x, aes(y=timeBlock, x = mo, size = n, col = Fishery, shape = Fishery))+
#   geom_point(data = xx, aes(y=timeBlock, x = mo, size = n, col = Fishery, shape = Fishery))+
#   facet_wrap(~region)+
#   theme_bw() + 
#   theme(text = element_text(size = 20))
# 
# ggplot()+
#   geom_jitter(data = x, aes(y=region, x = mo, size = n, col = timeBlock, shape = timeBlock, group = timeBlock), width = 0.25, height = 0.25)+
#   theme_bw() + 
#   theme(text = element_text(size = 20))
# 
# ggplot()+
#   geom_hline(yintercept=7, col = "red") +
#   geom_hline(yintercept=13, col="red") +
#   geom_hline(yintercept=11, col = "gold") +
#   geom_hline(yintercept=17, col="gold") +
#   geom_hline(yintercept=15, col = "blue") +
#   geom_hline(yintercept=21, col="blue") +
#   #geom_ribbon(aes(ymin = 7, ymax = 13), fill = "red")+
#   geom_point(data = x, aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   geom_point(data = xx, aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   scale_color_manual(values = c("black","white"))+
#   facet_wrap(~region)+
#   #theme_bw() + 
#   theme(text = element_text(size = 20))
# 
# ggplot()+
#   geom_ribbon(data = filter(hrs, timeBlock %in% "morning") %>% dplyr::select(hline_min, hline_max) %>% distinct(), aes(ymin = hline_min, ymax = hline_max, x=seq(1:12)), fill = "red", alpha = 0.3)+
#   geom_ribbon(data = filter(hrs, timeBlock %in% "afternoon"), aes(ymin = hline_min, ymax = hline_max, x=seq(1:12)), fill = "yellow", alpha = 0.3)+
#   geom_ribbon(data = filter(hrs, timeBlock %in% "night"), aes(ymin = hline_min, ymax = hline_max, x=seq(1:12)), fill = "blue", alpha = 0.3) + 
#   geom_point(data = hrs %>% filter(Fishery %in% "Blue Crab", !region %in% "undefined", !is.na(timeBlock)), aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   geom_point(data = hrs %>% filter(Fishery %in% "Finfish", !region %in% "undefined", !is.na(timeBlock)) , aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   #scale_color_manual(values = c("black","white"))+
#   facet_wrap(~region)+
#   #theme_bw() + 
#   theme(text = element_text(size = 20))
# 
# ggplot()+
#   geom_ribbon(aes(ymin = 7.5, ymax = 13.5, x=seq(0.5:12.5)), fill = "gold", alpha = 0.2)+
#   geom_ribbon(aes(ymin = 11.5, ymax = 17.5, x=seq(0.5:12.5)), fill = "red", alpha = 0.2)+
#   geom_ribbon(aes(ymin = 15.5, ymax = 21.5, x=seq(0.5:12.5)), fill = "blue", alpha = 0.2) + 
#   geom_point(data = x, aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   geom_point(data = xx, aes(y=hr, x = mo, size = n, col = Fishery, shape = Fishery))+
#   scale_color_manual(values = c("black","white"))+
#   #facet_wrap(~region)+
#   theme_bw() + 
#   theme(text = element_text(size = 20))
# 
# ggplot()+
#   geom_point(data = x, aes(y=hr, x = mo, size = n, fill = Fishery, shape = Fishery), col = "black")+
#   geom_point(data = xx, aes(y=hr, x = mo, size = n, fill = Fishery, shape = Fishery), col = "black")+
#   scale_fill_manual(values = c("black","white"))+
#   facet_wrap(~region)+
#   #theme_bw() + 
#   theme(text = element_text(size = 20))


boo = as.data.frame(rbind(cbind(rep(0.1, 6), seq(1,6), rep(7,6)),
                          cbind(rep(0.3, 6), seq(1,6), rep(11,3)),
                          cbind(rep(0.5, 6), seq(1,6), rep(15,6)),
                          cbind(rep(0.1, 6), seq(1,6), rep(13,6)),
                          cbind(rep(0.3, 6), seq(1,6), rep(17,3)),
                          cbind(rep(0.5, 6), seq(1,6), rep(21,6))))
names(boo) = c("x","region","y")

ggplot()+
  geom_line(data = boo[boo$x %in% 0.1,], aes(x = x, y = y), col = "lightgrey", size = 3) +
  geom_line(data = boo[boo$x %in% 0.3,], aes(x = x, y = y), col = "grey70", size = 3) +
  geom_line(data = boo[boo$x %in% 0.5,], aes(x = x, y = y), col = "grey50", size = 3) +
  geom_point(data = x, aes(y=hr, x = mo, size = n, shape = Fishery, fill = Fishery), col = "steelblue1")+
  geom_point(data = xx, aes(y=hr, x = mo, size = n, shape = Fishery, fill = Fishery), col = "black")+
  scale_shape_manual(values = c(21, 25))+
  scale_fill_manual(values = c("lightskyblue", "black"))+
  facet_wrap(~region, ncol = 2, scales = "free")+
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.text.x=element_text(angle=45),
        panel.grid.minor = element_line(colour = "white")) + 
  labs(x = "Month", y = "Hour") + 
  xlim(0, 12.1)+ 
  scale_x_continuous(labels = c("0" = "",
                                "1" = "Jan.",
                                "2" = "Feb.",
                                "3" = "Mar.",
                                "4" = "Apr.",
                                "5" = "May",
                                "6" = "Jun.",
                                "7" = "Jul.",
                                "8" = "Aug.",
                                "9" = "Sep.",
                                "10" ="Oct.",
                                "11" = "Nov.",
                                "12" = "Dec."),
                   breaks = c(0:12),
                   limits = c(0,12))


