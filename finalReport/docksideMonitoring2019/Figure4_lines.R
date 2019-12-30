# Figure 4. Trips per month per region

tmp = WM %>% mutate(mo = month(Date)) %>% dplyr::select(mo, TripID, region) %>% 
  filter(!region %in% "undefined") %>%
  group_by(mo, region) %>% 
  summarise(n = n()) %>% ungroup() %>%
  mutate(mo_char = as.character(mo), 
         mo_char = replace(mo_char, mo %in% 1, "Jan."),
         mo_char = replace(mo_char, mo %in% 2, "Feb."),
         mo_char = replace(mo_char, mo %in% 3, "Mar."),
         mo_char = replace(mo_char, mo %in% 4, "Apr."),
         mo_char = replace(mo_char, mo %in% 5, "May"),
         mo_char = replace(mo_char, mo %in% 6, "Jun."),
         mo_char = replace(mo_char, mo %in% 7, "Jul."),
         mo_char = replace(mo_char, mo %in% 8, "Aug."),
         mo_char = replace(mo_char, mo %in% 9, "Sep."),
         mo_char = replace(mo_char, mo %in% 10, "Oct."),
         mo_char = replace(mo_char, mo %in% 11, "Nov."),
         mo_char = replace(mo_char, mo %in% 12, "Dec."))

p = ggplot() + 
  geom_point(data = tmp, aes(x = mo, y = n, col = region, shape = region), size = 5)+
  geom_line(data = tmp, aes(x = mo, y = n, col = region, group = region), size = 2)+
  scale_color_manual(values=c("lightgrey","#999999", "gold","#E69F00", "#56B4E9","cornflowerblue"))+
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Month", y = "Number of Trips") +
  scale_x_continuous(labels = c("1" = "Jan.","2" = "Feb.","3" = "Mar.","4" = "Apr.",
                              "5" = "May","6" = "Jun.","7" = "Jul.","8" = "Aug.",
                              "9" = "Sep.","10" ="Oct.","11" = "Nov.","12" = "Dec."),
                   breaks = c(1:12), 
                   limits = c(1,12))
p

ggsave(paste(dir.out, "Figure4_tripsPerMonthPerRegion.png", sep=""),p)
