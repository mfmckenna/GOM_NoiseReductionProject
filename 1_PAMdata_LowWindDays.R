# wind speed days for GoMex project

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)
library(ggplot2)
#for map
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)

#-----------------------------------------------------------------------------------------
#MAP OF PAM SITES and NOAA BUOYS
#--------------------------------------------------------------------------------
#world map 
wrkDir = "C:\\Users\\mckenna6\\Google Drive\\ActiveProjects\\NFWF_GOM_workingDrive\\data\\PAM\\"
#wrkDir = "G:\\My Drive\\ActiveProjects\\NFWF_GOM_workingDrive\\data\\PAM\\" # desktop"
setwd(wrkDir)
dataIn   = read.csv(paste0(wrkDir, "\\GoM_PAMsites_Compiled.csv"))
dataIn= dataIn[1:41,1:7] #remove NA rows
dataIn = dataIn[dataIn$Use == "Y",]
dataWind = read.csv(paste0(wrkDir, "\\GoM_NOAAbuoy.csv"))
dataWind= dataWind[1:12,1:5] #remove NA rows

theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")
class(world)
AKprog = 3467 # c(-4596612.39 -2250856.49) , c( 2,024,122.30 4,364,571.46)
WGS84proj = 4326
sites <- st_as_sf(data.frame( latitude = dataIn$Latitude, longitude = dataIn$Longitude),
                  coords = c("longitude", "latitude"), crs = WGS84proj, 
                  agr = "constant")
sitesWind <- st_as_sf(data.frame( latitude = dataWind$Latitude, longitude = dataWind$Longitude),
                      coords = c("longitude", "latitude"), crs = WGS84proj, 
                      agr = "constant")

#MAP WITH GOM with PAM stations
ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  #geom_sf(data = sites, size = 4, shape = 16, fill = "red") +
  geom_sf     (data = sites,     aes(fill = 'red', color = as.factor(dataIn$Use )), size = 5, shape = 16) +
  geom_sf_text(data = sites, aes(label = dataIn$SiteID), colour = "black") +
  geom_sf     (data = sitesWind, aes(fill = 'yellow'), size = 2, shape = 24) +
  geom_sf_text(data = sitesWind, aes(label = dataWind$Buoy), colour = "black")+
  geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
            fill = NA, colour = "black", size = 1.5) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)+
  scale_fill_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA)) 


#-----------------------------------------------------------------------------------------
# WIND DATA to Select days for each site
#-----------------------------------------------------------------------------------------
dirWd = wrkDir #choose.dir() #paste0(tDir,"abioticFiles\\")
setwd(dirWd)
outDir = paste0(wrkDir, "SelectDays_bySite")
#-----------------------------------------------------------------------------------------
#SUMMARY OF SITES MATCHED WITH NOAA buoys: SelectDays_summary.xlsx
# OUTPUT: create a selected days file for each site: 
# file name: SITEID_BUOY_2019to2021.csv
# column names: "Day","avgWSPD","sdWSPD","nSamples","timeAbove5"
# 13 sites... AUG 2020 to AUG 2021


#-----------------------------------------------------------------------------------------
buoy = "42002" 
sites = c("AC", "GA")
#-----------------------------------------------------------------------------------------
buoy = "42003" 
sites = c("DT", "Y1B", "LC")
#-----------------------------------------------------------------------------------------
buoy = "42039" 
sites = c("MC", "DC", "Y1D")
#-----------------------------------------------------------------------------------------
buoy = "42055" 
sites = c("Y1C", "MR", "CE" )
#-----------------------------------------------------------------------------------------
buoy = "BURL1" 
sites = c("NO", "GC")
#-----------------------------------------------------------------------------------------

#CREATE A WAY TO LOOP
buoy  = c("42002", "42003", "42039", "42055","BURL1")
sites = list( c("AC", "GA"),c("DT", "Y1B", "LC"),c("MC", "DC", "Y1D"),c("Y1C", "MR", "CE" ),c("NO", "GC")) 

#-----------------------------------------------------------------------------------------
output = NULL #combines all sites together
outInfo = NULL

for (ii in 1:length(buoy)){
  WSPD = NULL
  filenames = intersect(list.files(dirWd,pattern = buoy[ii]), list.files(dirWd,pattern = "NOAA") )
  
  ###read and append data
  for (ff in 1: length (filenames)){
    tfile = paste0(dirWd, "\\", filenames[ff])
    tmp = fread(tfile)
    #check headers
    #cat( filenames[ff], colnames(tmp), "\n")
    WSPD = rbind(WSPD,tmp)
  }
  endR = nrow(WSPD)
  WSPD = as.data.frame(WSPD[2:endR,]) #remove first row
  WSPD$DateC    =  ( paste( WSPD$`#YY`, "-", WSPD$MM, "-", WSPD$DD, " ", WSPD$hh, ":",  WSPD$mm , sep = ""))
  WSPD$DateF = as.POSIXct(WSPD$DateC,"%Y-%m-%d %H:%M",tz ="GMT")
  WSPD$Mth =   month(WSPD$DateF)
  WSPD$WSPD  = as.numeric(as.character(WSPD$WSPD  )) #some days with NAs which( is.na(WSPD$WSPD)) = 9
  WSPD$WSPD[WSPD$WSPD == 99] = NA #need to remove 99!!!
 
  ###format and aggregate by day
  WSPD$Day   = as.Date(WSPD$DateF, format = "%Y-%m-%d")
  WSPD$HR    = strftime(WSPD$DateF, format="%H") 
  WSPD$DayHR = as.POSIXct( paste0(WSPD$Day," ", WSPD$HR,":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  uday = unique(WSPD$Day)
  WSPDday = NULL
  cutOff = 5.1 # counts the samples above this value 5.1 = 10 knots
  for (dd in 1:length(uday))
  {
    tmp = WSPD[WSPD$Day== uday[dd],] 
    tmp = tmp[complete.cases(tmp),]
    WSPDday = rbind(WSPDday, c(as.character(uday[dd]), mean(tmp$WSPD, na.action = na.omit), sd(tmp$WSPD), nrow(tmp), nrow(tmp[tmp$WSPD>cutOff, ])/nrow(tmp) ) )
    
  }
  WSPDday = as.data.frame(WSPDday)
  colnames(WSPDday) = c("Day","avgWSPD","sdWSPD","nSamples","timeAbove5")
  WSPDday$Day = as.Date(WSPDday$Day)
  WSPDday$avgWSPD = as.numeric(as.character(WSPDday$avgWSPD))
  WSPDday$sdWSPD = as.numeric(as.character(WSPDday$sdWSPD))
  WSPDday$nSamples = as.numeric(as.character(WSPDday$nSamples))
  WSPDday$timeAbove5 = as.numeric(as.character(WSPDday$timeAbove5))
  WSPDday$Mth = month(WSPDday$Day)
  #remove NA days
  WSPDday = WSPDday[complete.cases(WSPDday),]
  
  ###data check plot
  # ggplot(WSPDday, aes(Day, avgWSPD, color = as.factor(Mth)) )+
  #   geom_errorbar(aes(ymin=avgWSPD-sdWSPD, ymax=avgWSPD+sdWSPD), width=.1) +
  #   geom_point() +
  #   xlab("")+
  #   ylab("Wind Speed (mps) ") +
  #   theme_minimal()+
  #   ggtitle(paste("Check- WIND (",buoy, ")"))
  
 
  ###select days mean less than 10 knots (5.14444 m/s) and SD less than 5 knots
  selectDaysSD = WSPDday[ WSPDday$avgWSPD <=5.14444 & WSPDday$sdWSPD <= 2.6 ,]
  #select days mean less than 10 knots (5.14444 m/s) and less than 10% of samples >10 knots (use these days)
  selectDaysTA  = WSPDday[ WSPDday$avgWSPD <=5.14444 & WSPDday$timeAbove5 <= .10 ,]
  selectDaysTAt = selectDaysTA[ selectDaysTA$Day >= as.Date("2020-08-01") ,]
  selectDaysTAt = selectDaysTAt[ selectDaysTAt$Day < as.Date("2021-09-01") ,]
  sitesS = sites[[ii]]
  for (ss in 1:length(sitesS) ){
    output = rbind(output, cbind(sitesS[ss], selectDaysTAt) )
    outInfo =  rbind( outInfo, (c(sitesS[ss], buoy[ii], nrow(selectDaysTAt)) ) )
  }
  
  
  
}


colnames(output)[1] = "site"
output$Day1 = as.Date(output$Day)
ggplot(output, aes(Day1,site, fill=as.numeric(as.character(avgWSPD)) ) ) +
  geom_tile() +
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle(paste0("selected days for GoMex PAM stations- average windspeed [m/s] \n Total days = ", nrow(output) ) )

Sys.Date()
write.csv(output,  paste0(outDir, "\\", Sys.Date(), "SelectDaysALL_ALLSites_Aug2020toAug2021.csv"))
write.csv(outInfo, paste0(outDir, "\\", Sys.Date(), "SelectDaysInfo_ALLSites_Aug2020toAug2021.csv"))

#truncate dates-- remove consecutive days
uSites = ( unique(output$site) )
output2 = NULL
outputInfo2 = NULL

for (ss in 1:length(uSites)){
  tmp = output[ output$site == uSites[ss], ]
  tmp$Diff = c(NA,diff(tmp$Day) )
  
  tmp = tmp[ tmp$Diff != 1, ]
  tmp = tmp[complete.cases(tmp), ]
  
  if ( uSites[ss] == "Y1D") {
    tmp = tmp[tmp$Day1 < as.Date("2020-12-01") , ]
  }
  
  #truncate dates-- remove Y1D data after Dec 1 2020 because not good data
  output2 = rbind( output2,tmp)
  outputInfo2 = rbind(outputInfo2, c(as.character(tmp$site[1]),nrow(tmp)) )
}




Sys.Date()
write.csv(output2,  paste0(outDir, "\\", Sys.Date(), "SelectDays2_ALLSites_Aug2020toAug2021.csv"))
write.csv(outputInfo2, paste0(outDir, "\\", Sys.Date(), "SelectDaysInfo2_ALLSites_Aug2020toAug2021.csv"))

ggplot(output2, aes(Day1,site, fill=as.numeric(as.character(avgWSPD)) ) ) +
  geom_tile() +
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle(paste0("selected days for GoMex PAM stations- average windspeed [m/s] \n Total days = ", nrow(output2) ) )
