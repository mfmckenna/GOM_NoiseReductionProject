## GoMex Project:  AIS traffic
# summarize AIS traffic within a specified polygon around a PAM station

#PURPOSE: generate predictive variables for PAM model
# AND list of vessel for JJ
# currently works for single site- DC

# loops through single day of AIS data for all U.S.(trimmed using 2a_trimAIStoGoMex.R) 
# finds points within buffer (listening Space) around PAM station
# and summarizes by size category.

# NEXT STEPs: 
#- modify this code to bring in MMSI labels from JJ to categorize the ships

#-----------------------------------------------------
rm(list = ls())


# libraries
#-----------------------------------------------------
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
library(tigris)
library(tidyverse)
library (geosphere)
library(ggsn)
library(rgdal)
library(sf)

# set up
#-----------------------------------------------------
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\"
out.Dir = paste0(wrkDir, "data\\PAM")
WGS84proj = 4326
AIS.dir <- paste0("H:\\AIS_MarineCad\\data\\GoMexRegion")
site = "DC"
type = "all" # choose to run on all days or "low" wind days only
sf_use_s2(FALSE) #avoids error with st_intersects

# INPUT DATA: PAM sites
#-----------------------------------------------------
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_Compiled.csv"))
dataPAM   = dataPAM[1:41,1:7] #remove NA rows
dataPAM   = dataPAM[dataPAM$Use == "Y",]

# PAM site info
dataPAM   = dataPAM[dataPAM$SiteID == site,]
sites = st_as_sf(data.frame( latitude = dataPAM$Latitude, longitude = dataPAM$Longitude ),
                 coords = c("longitude", "latitude"), crs = WGS84proj, 
                 agr = "constant") 
pois = st_transform(sites, crs = 7801)
pt_buffer = st_buffer(pois,dist = 15000,) # just make 10 km buffer/polygon
buff = st_transform(pt_buffer, crs = 4326)

# INPUT DATA: Listening Spaces-- specific to this site
#-----------------------------------------------------
shpInLV = st_read(paste0(wrkDir, paste0("data\\ListeningArea\\PilotCalculations\\", site, "_Aug_LargeVessel_64.shp") ))
ggplot() + 
  geom_sf(data = shpInLV) + 
  ggtitle("Listening Range") + 
  coord_sf()
shpInLV = shpInLV %>% st_set_crs(4326)
shpInLV = st_as_sf(shpInLV)

shpInMV = st_read(paste0(wrkDir, paste0("data\\ListeningArea\\PilotCalculations\\", site, "_Aug_MediumVessel_74.shp")) )
shpInMV = shpInMV %>% st_set_crs(4326)
shpInMV = st_as_sf(shpInMV)

shpInSE = st_read(paste0(wrkDir, paste0("data\\ListeningArea\\PilotCalculations\\", site, "_Aug_Seismic_81.shp")) )
shpInSE = shpInSE %>% st_set_crs(4326)
shpInSE = st_as_sf(shpInSE)

# INPUT DATA: other layers files
#-----------------------------------------------------
RIGS = sf::st_read(dsn =  paste0(wrkDir, "data\\PlatformData\\platform.gdb"), layer = "platform")
WIND = sf::st_read(dsn =  paste0(wrkDir, "data\\GOM_CallAreaTo400m.gdb"), layer = "GOM_CallAreaTo400m")

# INPUT DATA: AIS daily files
#----------------------------------------------------
#all possible days
dys = read.csv(paste0( wrkDir, "data\\PAM\\SelectDays_bySite\\2022-02-14SelectDays2_ALLSites_Aug2020toAug2021.csv" ))

#OR truncate to only low wind days
if (type != "all") { 
  dys = dys$Day[dys$site == site]
  #dys = gsub("-","_",dys)
} else {
  dys = dys$Day
  #dys = gsub("-","_",dys)
  
}

# LOOP THROUGH AIS DAYS with Data and summarize around each PAM point
#-----------------------------------------------------
List.ships = NULL
outputDaySite = NULL
for (ff in 1:length(dys)) { 
  
  cat("Day ", dys[ff], " (", ff," of ",length(dys), ")", as.character( dataPAM$SiteName ) ,"\n")
  
  
  dysFiles  = list.files(path = AIS.dir, pattern= dys[ff] , recursive=TRUE, full.names = TRUE) 
  if ( length(dysFiles) != 0 ){
    load(dysFiles) 
    
    #INTERSECT DATA IN Listening Space
    # only points within Large vessel polygon
    tst = lengths(st_intersects(df2t, shpInLV)) > 0 
    
    df2t = df2t[tst,]
    df2t$day = as.Date( df2t$timestamp )
    df2t$hr  = hour( df2t$timestamp )
    df2t$Length = as.numeric(as.character(df2t$Length))
    # separate data by size groups... eventually make this types (cargo/tanker, service, other)
    lg = df2t[df2t$Length >= 100,]
    uships_LV_large  = length( unique(lg$MMSI) )
    SOG_LV_large = mean( lg$SOG, na.rm = T)
    other = df2t[df2t$Length < 100,]
    uships_LV_other  = length( unique(other$MMSI) )
    SOG_LV_other = mean( other$SOG, na.rm = T)
    
    # UNIQUE SHIPS IN LISTENING SPACE-- large
    List.ships = rbind(List.ships, unique(df2t$MMSI) ) #keeps a list of vessels for JJ to search.. but not all days in this period
    
    # only points within medium vessel polygon
    tst = lengths(st_intersects(df2t, shpInMV)) > 0 
    df2t = df2t[tst,]
    df2t$day = as.Date( df2t$timestamp )
    df2t$hr  = hour( df2t$timestamp )
    df2t$Length = as.numeric(as.character(df2t$Length))
    # separate data by size groups... eventually make this types (cargo/tanker, service, other)
    lg = df2t[df2t$Length >= 100,]  
    uships_MV_large  = length( unique(lg$MMSI) )
    SOG_MV_large = mean( lg$SOG, na.rm = T)
    other = df2t[df2t$Length < 100,]
    uships_MV_other  = length( unique(other$MMSI) )
    SOG_MV_other = mean( other$SOG, na.rm = T)
    
    RIGS2 = st_transform(RIGS, crs = 4326)
    tstRIG = lengths(st_intersects(RIGS2, buff)) > 0
    RIGS_10km = nrow( RIGS2[tstRIG,] )
    
    # OUTPUTS
    vsum = cbind(dataPAM$SiteID, dataPAM$SiteName, as.character(dys[ff]),  
                 uships_LV_large, SOG_LV_large, uships_LV_other, SOG_LV_other,
                 uships_MV_large, SOG_MV_large, uships_MV_other, SOG_MV_other,
                 RIGS_10km)
    
    colnames(vsum)[3] = "Day"
    
    outputDaySite = rbind( outputDaySite, vsum)
  }
} 

save(outputDaySite, file = paste0(out.Dir, "\\", site, "_NoiseActivityIndex_",type, ".RData") )
colnames(List.ships)= dys
write.csv(List.ships, file = paste0(out.Dir, "\\", site, "_UniqueShips_",type, ".csv") )
List.ships2 = NULL
for (ii in 1:dim(List.ships)[2]){
  tmp = ( List.ships[ii,])
  names(tmp)= NULL
  unlist(tmp)
  List.ships2 =  c( List.ships2,  tmp)
}

List.ships3 = unique( List.ships2 )
write.csv(List.ships3, file = paste0(out.Dir, "\\", site, "_UniqueShips3_",type, ".csv") )
