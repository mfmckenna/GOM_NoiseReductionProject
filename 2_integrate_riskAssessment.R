# PURPOSE: summarize by site and species ####
# in all listening spaces (not seismic b/c too big)
#1) SPECIES PRESENCE 
#2) NOISE ACTIVITY
#3) Vulnerability
# Output monthly files by site/species


rm(list=ls())

# libraries #### 
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)
library(rgdal)
library(lubridate)

# set up #### 
WGS84proj = 4326
plt = "on"
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")

# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 
AIS.dir =   paste0("H:\\AIS_MarineCad\\data\\GoMexRegion\\GoMexRegion_trim")

# DATA LAYER INPUTS ####
## analysis area  ####
region = sf::st_read(dsn =  paste0(wrkDir, "data\\region\\ROI.dbf"), layer = "ROI")

## bathymetry  ####
dataBATH   = list.files( path = paste0(wrkDir, "data\\bathymetry\\"), pattern = ".shp", recursive=TRUE, full.names = T )
tmp = st_read(dataBATH)
tmp = tmp %>% st_set_crs(4326)
dataBATH = st_as_sf(tmp)

## vulnerability ####
zone1 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone1v2_EPA.dbf"), layer = "Zone1v2_EPA")
zone2 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone2v2_All.dbf"), layer = "Zone2v2_All")
zone3 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone3v2_WPA.dbf"), layer = "Zone3v2_WPA")
zone4 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone4v2_EPA2.dbf"), layer = "Zone4v2_EPA2")
zone5 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone5clip.dbf"), layer = "zone5clip")
zone6 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone6clip.dbf"), layer = "zone6clip")
zone7 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone7_Clip.dbf"), layer = "Zone7_clip")
filesVul = list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), pattern = ".csv", recursive=TRUE, full.names = T )

## PAM sites  ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) 
#this version has updated lat/lons & summary from 2_integrate_PAMsites.R
sites = st_as_sf(data.frame( latitude = dataPAM$Latitude, longitude = dataPAM$Longitude ),
                 coords = c("longitude", "latitude"), crs = WGS84proj, 
                 agr = "constant") 

## 20 km buffer ####
pois = st_transform(sites, crs = 7801)
pt_buffer = st_buffer(pois,dist = 20000,) 
buff = st_transform(pt_buffer, crs = 4326)

## offshore Oil ####
RIGS = sf::st_read(dsn =  paste0(wrkDir, "data\\gom_platforms\\gom_platforms.dbf"), layer = "gom_platforms")
RIGS2 = st_transform(RIGS, crs = 4326)
RIGSYes = RIGS2[ RIGS2$Status == "IN SERVICE", ]
RIGSNo = RIGS2[ RIGS2$Status != "IN SERVICE", ]
ActiveLease = sf::st_read(dsn =  paste0(wrkDir, "data\\actlease\\al_20220801.dbf"), layer = "al_20220801")
ActiveLease = st_transform(ActiveLease, crs = 4326)

## offshore wind ####
WIND = sf::st_read(dsn =  paste0(wrkDir, "data\\GOM_CallAreaTo400m.gdb"), layer = "GOM_CallAreaTo400m")
WIND2 = st_transform(WIND, crs = 4326)
WEA1 = sf::st_read(dsn =  paste0(wrkDir, "data\\WEA_option_I_M_shapes_w_metadata\\Option_I.dbf"), layer = "Option_I")
WEA2 = sf::st_read(dsn =  paste0(wrkDir, "data\\WEA_option_I_M_shapes_w_metadata\\Option_M.dbf"), layer = "Option_M")

## seismic days ####
dataSeismic   = read.csv(paste0(wrkDir, "data\\Seismic\\NFWF seismic dates1.csv")) 
dataSeismic$Date = as.Date( dataSeismic$ï..Date, format = "%d-%b-%y")
dataSeismic$Mth = month( dataSeismic$Date, label = TRUE, abbr = FALSE)
idx = !is.na(dataSeismic$Lat)
dataSeismicT = dataSeismic[idx,]
seismicLocations = st_as_sf(data.frame( latitude = dataSeismicT$Lat, longitude = dataSeismicT$Lon, label = dataSeismicT$Mth ),
                            coords = c("longitude", "latitude"), crs = WGS84proj, 
                            agr = "constant") 


# BY MONTH PROCESSING ####
# ? ####
# NEED TO RE-RUN once all vulnerability and seismic are ready!

#set up loop with files to grab based on naming- listening spaces are calculated for each month
mthLS = c("August","August","August","October","October","October","Feburary","Feburary","Feburary","May","May","May") # for listening range data 
mthSe= c("July","August","September","October","November","December","January","Feburary","March","April","May","June") # seismic surveys days
yrmthSp = c(202107, 202008,202009,202010,202011,202012,202101,202102,202103,202104,202105,202106)       # for species data
mthAIS = c("2021-07", "2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06") # for AIS data

#worked for testing August 2020
# mthIn = "August" # for listening range data and seismic surveys days
# yrmth = 202008   # for species data
# AIS_pattern = "2020-08" # for AIS data


for (m in 3:3){
  # GET MONTHLY LAYERS ####
  ## correct labels ####
  mthIn = mthLS[m] # for listening range data 
  mthS =  mthSe[m] #seismic surveys days
  yrmth = yrmthSp[m]   # for species data
  AIS_pattern = mthAIS[m] # for AIS data
  
  ## AIS types  ####
  AIStypes = read.csv( list.files( path = paste0(wrkDir, "data\\AIS\\VesselTypes" ), pattern = AIS_pattern, recursive=TRUE, full.names = T ) )
  ## listening areas ####
  lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\", mthIn), 
                        pattern = ".shp", recursive=TRUE, full.names = T )
  namAll = NULL
  dataSpace = NULL 
  for (ii in 1: length( lsFiles )){
    source =  sapply(strsplit(basename(lsFiles[ii]), "_"), "[[", 3)
    namAll[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
    if (source != "Seismic"){
      
      tmp = st_read(lsFiles[ii])
      tmp$Name =  namAll[ii]
      tmp = tmp %>% st_set_crs(4326)
      tmp = st_as_sf(tmp)
      #assign(namAll[ii],tmp) 
      dataSpace = rbind(dataSpace, tmp)
    }
  }
  
  ## vulnerability files ####
  dataVul = read.csv ( list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), 
                                   pattern = paste0(mthS, ".csv"), recursive=TRUE, full.names = T ) )
  
  # SPECIES DATA ####
  # loop through each species to get values in all listening spaces
  # ?? do I need to divide all species values by 40 to get density in each cell, then sum?
  spFiles = list.files(path = paste0(wrkDir, "data\\MM density data\\"), pattern = "v2.shp", recursive=T,  full.names=T)
  SpeciesOut = NULL
  for (dd in 1:length(spFiles)) { 
    
    ### get species data- month ####
    pm = st_read( spFiles[dd] ) %>% st_transform(crs = WGS84proj)
    Specieslabel = sapply(strsplit(basename(spFiles[dd]), "_"), "[[", 1)
    mth =  (as.numeric( substr(yrmth, 5,6))*3) #gets matching month for species data
    denData =  as.data.frame(( pm[,mth ] ))
    denLayerName = colnames(denData)[1]
    mthData = denData[,1] #get gets the actual density values
    mthName = colnames( pm[,mth] )[1]
    
    #to keep track and make sure they match!
    cat("Processing...", yrmth, " for ", Specieslabel, "-", mthName, "\n")
    
    # summary in each LR ####
    for (ss in 1:dim(dataSpace)[1]) 
    {
      
      #names to save out
      x = strsplit(dataSpace$Name[ss], "_")
      site = sapply( x, "[", 1 )  
      mth2 = sapply( x, "[", 2 )  
      source = sapply( x, "[", 3 )  
      SNR = sapply( x, "[", 4 )  
      #intersect with each species layer for a given month
      tst = lengths(st_intersects(pm, dataSpace$geometry[ss] )) > 0
      pm2 = pm[tst,]
      tmp =  as.data.frame(( pm2[,mth] ))
      
      # vulnerability score ####
      tmpZone = dataPAM$VulerabilityZone[dataPAM$SiteID == site ]
      tmpZone1 = dataVul[dataVul$Zone == tmpZone,]
      tmpScore = ( tmpZone1$Total.vulnerability[tmpZone1$Species.ID == Specieslabel ] )
      
      SpeciesOut = rbind(SpeciesOut, c(site, mth2, source, SNR, dataSpace$Name[ss], Specieslabel ,   
                                       sum( tmp[1], na.rm=T), #total for Listening area
                                       sum( mthData, na.rm=T), #total for species
                                       sum( tmp[1], na.rm=T)/sum(mthData, na.rm=T),  #percent of total species
                                       tmpScore) )
      
      
      rm(x, site, mth2, source, SNR, tst, pm2, tmp, tmpZone, tmpZone1, tmpScore)
      
    } # end listening range loop
    
  } #end species loop
  SpeciesOut = as.data.frame(SpeciesOut)
  colnames(SpeciesOut) = c("Site","Month", "Source", "SNR", "ListeningSpace", "SpeciesLabel", "Density_LS", "Density_total", "PropInLS", "VulScore")
  SpeciesOut$Density_LS = as.numeric( as.character( SpeciesOut$Density_LS ))
  
  # species check plot
  # unique( SpeciesOut$SpeciesLabel )
  # ggplot(SpeciesOut, aes(Site, Density_LS, color = SpeciesLabel)) +
  #   geom_point()+
  #   facet_wrap(~Source)
  
  ### SAVE species results ####
  save(SpeciesOut, file = paste0(wrkDir,  "data\\RiskAssessment\\SpOut_", yrmth, ".RData") ) 
  
  ##  NOISE ACTIVITY  ####
  # intersect with AIS layer for a given month
  dysFiles  =  list.files(path = AIS.dir, pattern = AIS_pattern, recursive=TRUE, full.names = TRUE) 
  
  NoiseOut = NULL
  for (dd in 1:length(dysFiles)) 
  {
    
    load(dysFiles[dd]) 
    AISday = as.Date( df2t2$timestamp[1] )
    
    #get summary of vessel data in each LS
    for (ss in 1:dim(dataSpace)[1]){
      
      # CHECK PLOT
      # ggplot(data = world) +
      #   geom_sf( ) +
      #   geom_sf(data = df2t2, color = "blue",size = .1) + #AIS data for the day!
      #   geom_sf(data = sites, color = "red") +
      #   geom_sf(data = buff, color  = "gray", fill = "gray", alpha = .3) +
      #   geom_text (data  = dataPAM, aes(x=Longitude , y=Latitude +.3,  label = SiteID  ), size = 3)  +
      #   geom_sf(data = dataSpace$geometry[ss], color = "black", alpha = .1) + 
      #   ggtitle(dataSpace$Name[ss]) +
      #   coord_sf(xlim = c(-102.15, -80), ylim = c(16, 33), expand = FALSE) 
      
      #listening space details
      x = strsplit(dataSpace$Name[ss], "_")
      site = sapply( x, "[", 1 )  
      mth2 = sapply( x, "[", 2 )  
      source = sapply( x, "[", 3 )  
      SNR = sapply( x, "[", 4 ) 
      
      ### intersect AIS ####
      tst    = lengths(st_intersects(df2t2, dataSpace$geometry[ss] )) > 0
      df2t3  = df2t2[tst,]
      AISpts = nrow(df2t3)
      
      if  (AISpts == 0 ){
        
        # no vessels in the buffer- fill in zeros
        NoiseOut = rbind(NoiseOut, c(as.character(AISday[1]), site, mth2, source, SNR, dataSpace$Name[ss], AISpts,
                                     NA, NA, NA, NA,
                                     NA, NA, NA,NA, NA, NA) )
      } else {
        # separate data by size groups (eventually make this types: cargo/tanker, service, other)
        lg = df2t3[df2t3$Length >= 100,]
        uships_LV_large  = length( unique(lg$MMSI) )
        SOG_LV_large     = mean( lg$SOG, na.rm = T)
        other = df2t3[df2t3$Length < 100,]
        uships_LV_other  = length( unique(other$MMSI) )
        SOG_LV_other     = mean( other$SOG, na.rm = T)
        
        AISships = unique( df2t3$MMSI )
        AISdets = NULL
        for (tt in 1:length(AISships)){
          AISdets = rbind(AISdets, AIStypes[AIStypes$MMSI == AISships[tt], ])
        }
        
        #separate by vessel types: CARGO, TABKER, FISHING, PASSANGER, TUG, OTHER
        # https://faq.spire.com/determining-ais-ship-type
        cargo  = sum( AISdets$SHIP_AND_CARGO_TYPE >=70 & AISdets$SHIP_AND_CARGO_TYPE <=79)
        tanker = sum( AISdets$SHIP_AND_CARGO_TYPE >=80 & AISdets$SHIP_AND_CARGO_TYPE <=89)
        fish = sum( AISdets$SHIP_AND_CARGO_TYPE == 30 )
        passenger = sum( AISdets$SHIP_AND_CARGO_TYPE >=60 & AISdets$SHIP_AND_CARGO_TYPE <=69)
        tug =  sum( AISdets$SHIP_AND_CARGO_TYPE == c(31,32,52) )
        other = nrow(AISdets) - (cargo + tanker + fish + passenger + tug)
        
        NoiseOut = rbind(NoiseOut, c(as.character(AISday[1]), site, mth2, source, SNR, dataSpace$Name[ss], AISpts,
                                     uships_LV_large, SOG_LV_large, uships_LV_other, SOG_LV_other,
                                     cargo, tanker, fish, passenger, tug, other) )
        
        
      } #end if loop
      
      
      
    } # end listening space loop
    
  } #end AIS daily files in month loop
  
  NoiseOut = as.data.frame(NoiseOut)
  colnames(NoiseOut) = c("Day","Site", "mth", "Source", "SNR", "ListeningSpace", "TotalVesselsPts", "LargeVessels", "LargeSpeed", 
                         "OtherVessels", "OtherSpeed", "cargo", "tanker", "fish", "passenger", "tug", "other")
  
  ## intersect Seismic ####
  dataSeismicMth = dataSeismic[dataSeismic$Mth == mthS,] #trim data to just this month
  nDys = unique( dataSeismicMth$Date)
  for (d in 1:length(nDys)){
    #sum the activities
    tmp = dataSeismicMth[ dataSeismicMth$Date == nDys[d],]
    #cat(as.character(nDys[d]), sum(tmp$Active), "\n")
    
    #select rows of NoiseOut to add value includes all sites and ls
    NoiseOut$Seismic[ NoiseOut$Day == nDys[d]] = sum(tmp$Active)
    
  }
  
  # SAVE noise activity ####
  save(NoiseOut,   file = paste0(wrkDir,  "data\\RiskAssessment\\NaOut_daily_", yrmth, ".RData") ) 
  
  # SUMMARIZE by site x listening area
  uSts = unique(NoiseOut$Site)
  NoiseOut_MTH = NULL
  for (s in 1:length(uSts)){
    
    # unique site
    tmp = NoiseOut[ NoiseOut$Site == uSts[s], ]
    uLs = unique(tmp$ListeningSpace)
    
    for (u in 1:length(uLs)){
      # unique listening space
      tmp2 = tmp[ tmp$ListeningSpace == uLs[u], ]
      
      cols.num = c(8,9,10,11,12,13,14,15,16,17)
      tmp2[cols.num] <- sapply(tmp2[cols.num],as.numeric) 
      
      d[is.na(d)] <- 0
      
      mAct = colMeans(tmp2[sapply(tmp2, is.numeric)], na.rm = T)        
      sAct = colSums(tmp2[sapply(tmp2, is.numeric)], na.rm = T)
      
      NoiseOut_MTH = rbind(NoiseOut_MTH, c(tmp2$mth[1], tmp2$Site[1], tmp2$Source[1], tmp2$SNR[1], tmp2$ListeningSpace[1],
                                           mAct[c(2,4,11)], sAct[c(1,3,5:11)])  )
      
    }
    
  }
  colnames(NoiseOut_MTH) = c("mth","Site", "Source", "SNR", "ListeningSpace", 
                             "mean_LargeSpeed", "mean_OtherSpeed", "mean_SeismicDay", 
                             "total_LargeVessel", "total_OtherVessel", "total_cargo", "total_tanker", "total_fish", "total_passenger", "total_tug", "total_other",
                             "total_Seismic")
  save(NoiseOut_MTH,   file = paste0(wrkDir,  "data\\RiskAssessment\\NaOut_mthly_", yrmth, ".RData") )
  
  
}