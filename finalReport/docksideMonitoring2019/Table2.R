# Table 2. Trip Summary for Roving Monitors January to December 2019

# create table
tripSummary = as.data.frame(matrix(data = NA, ncol=7, nrow=7))
names(tripSummary) = c("Regions","AvailTrips","AttemptedTrips","SuccessfulTrips","AvailWM","AttemptedWM","SuccessfulWM")
tripSummary$Regions = c("1","2","3","4","5","6","Total")
tripSummary[tripSummary$Regions %in% "Total",2:7] = c(prettyNum(length(unique(WM$TripID)), big.mark = ","), 
                                                      paste(formatC(length(unique(RM$TripID))/length(unique(WM$TripID))*100, digits = 3), "% (n = ", length(unique(RM$TripID)), ")", sep=""),
                                                      paste(formatC((length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED")]))/length(unique(WM$TripID)))*100, digits=3), 
                                                            "% (n = ", length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED")])), ")", sep=""),
                                                      length(unique(WM$WatermenName)), 
                                                      paste(formatC((length(unique(RM$DNRID))/length(unique(WM$DNRID)))*100, digits=4), 
                                                            "% (n = ", length(unique(RM$DNRID)), ")", sep=""),
                                                      paste(formatC((length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED")]))/length(unique(WM$DNRID)))*100, digits=4), 
                                                            "% (n = ",length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED")])), ")", sep="")) 

for(n in c(1:6)){
  tripSummary$AvailTrips[n] = prettyNum(length(unique(WM$TripID[WM$region %in% n])), big.mark = ",")
  tripSummary$AttemptedTrips[n] = paste(formatC(length(unique(RM$TripID[RM$region %in% n]))/length(unique(WM$TripID[WM$region %in% n]))*100, digits = 3), "% (n = ", length(unique(RM$TripID[RM$region %in% n])), ")", sep="")
  tripSummary$SuccessfulTrips[n] = paste(formatC((length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED") &RM$region %in% n]))/length(unique(WM$TripID[WM$region %in% n])))*100, digits=3), 
                                         "% (n = ", length(unique(RM$TripID[RM$Result %in% c("MONITORED (on paper)","MONITORED") &RM$region %in% n])), ")", sep="")
  tripSummary$AvailWM[n] = length(unique(WM$DNRID[WM$region %in% n]))
  tripSummary$AttemptedWM[n] = paste(formatC((length(unique(RM$DNRID[RM$region %in% n]))/length(unique(WM$DNRID[WM$region %in% n])))*100, digits=4), 
                                     "% (n = ", length(unique(RM$DNRID[RM$region %in% n])), ")", sep="")
  tripSummary$SuccessfulWM[n] = paste(formatC((length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED") & RM$region %in% n]))/length(unique(WM$DNRID[WM$region %in% n])))*100, digits=4), 
                                      "% (n = ",length(unique(RM$DNRID[RM$Result %in% c("MONITORED (on paper)","MONITORED") & RM$region %in% n])), ")", sep="") 
}
rm(n)

xTable =  htmlTable(tripSummary, rnames = FALSE,
                    caption="Table 2. Trip Summary for Roving Monitors January to December 2019",
                    header =  c("Region",
                                "Total Available Trips",	
                                "Attempted Trips Monitored",	
                                "Successful Trips Monitored",	
                                "Number of Available Watermen",	
                                "Number of Individual Watermen Attempted to be Monitored",
                                "Number of Individual Watermen Successfully Monitored"),
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
            file=paste(dir.out, "Table2.html",sep=""), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)
