# PURPOSE: summarize unique vessels in DC listening area

rm(list=ls())

# libraries #### 
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)
library(rgdal)
library(lubridate)

# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 
AIS.dir =   "E:\\AIS\\AIS_MarCad\\data\\GoMexRegion\\"

# set up #### 
WGS84proj = 4326
plt = "on"
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")
#set up loop with files to grab based on naming- listening spaces are calculated for each month
mthLS = c("August","August","August","November","November","November","February","February","February","May","May","May") # for listening range data 
mthSe = c("July","August","September","October","November","December","January","Feburary","March","April","May","June") # seismic surveys days
yrmthSp = c(202107,202008,202009,202010,202011,202012,202101,202102,202103,202104,202105,202106)       # for species data
mthAIS = c("2021-07", "2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06") # for AIS data
AISsummary = NULL

# DATA LAYER INPUTS ####
## analysis area  ####
region = sf::st_read(dsn =  paste0(wrkDir, "data\\region\\ROI.dbf"), layer = "ROI")
## PAM sites  ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) 
#this version has updated lat/lons & summary from 2_integrate_PAMsites.R
sites = st_as_sf(data.frame( latitude = dataPAM$Latitude, longitude = dataPAM$Longitude ),
                 coords = c("longitude", "latitude"), crs = WGS84proj, 
                agr = "constant") 

for (m in 1:12){
  # GET MONTHLY LAYERS ####
  
  ## correct labels ####
  mthIn = mthLS[m]     # for listening range data 
  mthS =  mthSe[m]     # seismic surveys days
  yrmth = yrmthSp[m]   # for species data
  AIS_pattern = mthAIS[m] # for AIS data
  
  ## AIS types  ####
  AIStypes = read.csv( list.files( path = paste0(wrkDir, "data\\AIS\\VesselTypes" ), pattern = AIS_pattern, recursive=TRUE, full.names = T ) )
  
  ## listening areas ####
  lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\"), pattern = ".shp", recursive=TRUE, full.names = T )
  lsFiles = lsFiles[grepl("MediumVessel_74", lsFiles)]
  lsFiles = lsFiles[grepl(mthIn, lsFiles)]
  lsFiles =  ( lsFiles[grepl("DC", lsFiles)] ) 
  
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
  
  ##  NOISE ACTIVITY  ####
  # intersect with AIS layer for a given month
  dysFiles  =  list.files(path = AIS.dir, pattern = AIS_pattern, recursive=FALSE, full.names = TRUE) 
  sf_use_s2(FALSE)
  NoiseOut = NULL
  
  for (dd in 1:length(dysFiles)) {
    
    load(dysFiles[dd]) 
    AISday = as.Date( df2t$timestamp[1] )
    
    #listening space details
    x = strsplit(dataSpace$Name, "_")
    site = sapply( x, "[", 1 )  
    mth2 = sapply( x, "[", 2 )  
    source = sapply( x, "[", 3 )  
    SNR = sapply( x, "[", 4 ) 
    
    ### intersect AIS ####
    tst    = lengths(st_intersects(df2t, dataSpace$geometry )) > 0
    df2t3  = df2t[tst,]
    AISpts = nrow(df2t3)
    
    if  (AISpts != 0 ){
      ush =  (unique(df2t3$MMSI) )
      for (ss in 1:length( ush) ) {
        tmpSh = df2t3[df2t3$MMSI == ush[ss],] 
        tmpSh = tmpSh[1,]
        
        #error with some headings not having the same name
        # assume first day is alway correct and then just apply that heading to all other days!
        if(dd > 2) {colnames(tmpSh) = colnames(NoiseOut) }
        
        NoiseOut = rbind(NoiseOut, as.data.frame(tmpSh) ) 
        
        #as.data.frame( colnames(NoiseOut) )
        #as.data.frame ( colnames(tmpSh) )
      }
    } #end if loop
    
    
  } #end AIS daily files in month loop
  
  NoiseOut = as.data.frame(NoiseOut) # save this out/ trim to type fishing
  NoiseOutFish = NoiseOut [NoiseOut$VesselType >= 30 & NoiseOut$VesselType < 40 , 1:15]
  #write.csv( NoiseOutFish, (paste0(wrkDir, "AISFish_DC_", AIS_pattern, ".csv")),row.names=FALSE )
  
  AISsummary = rbind( AISsummary, c(AIS_pattern, length(unique(NoiseOutFish$MMSI) ), length( NoiseOut$MMSI) ) ) 
  
} #end month loop

AISsummary 