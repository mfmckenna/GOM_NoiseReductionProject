# plotting listening ranges on map -- chekc output from TM
# GOM sites
rm(list=ls())

#ONLY WORKS FOR AUGUST DATA!!!
mthIn = "August" #all sites 1 month-- just to make initial plot #only processes one month 

# SET UP  ####

# libraries #### 

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)
library(rgdal)

# set up #### 
WGS84proj = 4326
plt = "on"
sf_use_s2(FALSE) #avoids error with st_intersects
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")


# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 

# DATA LAYERS ####
## analysis area  ####
region = sf::st_read(dsn =  paste0(wrkDir, "data\\region\\ROI.dbf"), layer = "ROI")

## bathymetry  ####
dataBATH   = lsFiles = list.files( path = paste0(wrkDir, "data\\bathymetry\\"), pattern = ".shp", recursive=TRUE, full.names = T )
tmp = st_read(dataBATH)
tmp = tmp %>% st_set_crs(4326)
dataBATH = st_as_sf(tmp)

## PAM sites  ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) #this version has updated lat/lons
#dataPAM   = dataPAM[1:10,1:7] #remove NA rows
sites = st_as_sf(data.frame( latitude = dataPAM$Latitude, longitude = dataPAM$Longitude ),
                 coords = c("longitude", "latitude"), crs = WGS84proj, 
                 agr = "constant") 

## 20 km buffer ####
pois = st_transform(sites, crs = 7801)
pt_buffer = st_buffer(pois,dist = 20000,) # just make 10 km buffer/polygon
buff = st_transform(pt_buffer, crs = 4326)

## vulnerability ####
zone1 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone1v2_EPA.dbf"), layer = "Zone1v2_EPA")
zone2 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone2v2_All.dbf"), layer = "Zone2v2_All")
zone3 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone3v2_WPA.dbf"), layer = "Zone3v2_WPA")
zone4 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone4v2_EPA2.dbf"), layer = "Zone4v2_EPA2")
zone5 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone5clip.dbf"), layer = "zone5clip")
zone6 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone6clip.dbf"), layer = "zone6clip")
zone7 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone7_Clip.dbf"), layer = "Zone7_clip")

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

## AIS daily files ####
AIS.dir <- "H:\\AIS_MarineCad\\data\\GoMexRegion"
fileList <- list.files(path = AIS.dir, pattern="AIS_", recursive=FALSE, full.names = T)
load( fileList[1] )
df2 = st_as_sf(x = df2t,  coords = c("LON", "LAT"), crs = WGS84proj)
DY = unique(as.Date(df2t$timestamp))

# LISTENING SPACES  ####
lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\", mthIn), pattern = ".shp", recursive=TRUE, full.names = T )
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
    assign(namAll[ii],tmp) 
    dataSpace = rbind(dataSpace, tmp)
  }
}
as.data.frame(namAll)

## Large Vessel ####
lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\", mthIn), pattern = "64.shp", recursive=TRUE, full.names = T )
namLV = NULL
for (ii in 1: length( lsFiles )){
  namLV[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp$Name =  namLV[ii]
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namLV[ii],tmp)
  
}
as.data.frame(namLV)

## Medium Vessel ####
lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\", mthIn), pattern = "74.shp", recursive=TRUE, full.names = T )
namMV = NULL
for (ii in 1: length( lsFiles )){
  namMV[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp$Name =  namMV[ii]
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namMV[ii],tmp)
  
}
as.data.frame(namMV)

## Seismic ####
lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\"), pattern = "81.shp", recursive=TRUE, full.names = T )
namSE = NULL
for (ii in 1: length( lsFiles )){
  namSE[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namSE[ii],tmp)
  
}
as.data.frame(namSE)
as.data.frame(namLV)  

## Low-frequency  ####
ggplot(data = world) +
  geom_sf( ) +
  #geom_sf(data = df2, color = "blue",   size = .2, alpha = .5)  +
  geom_sf(data = region, alpha=0.1) + 
  geom_sf(data = dataBATH, alpha=0.2) + 
  #LargeVessel Listening Spaces- LF
  geom_sf(data = AC_Aug_LargeVessel_64, color = "orange", alpha=.1) + 
  geom_sf(data = DC_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = DT_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = GA_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = GC_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = LC_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = Y1B_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = Y1D_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = MC_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  geom_sf(data = NO_Aug_LargeVessel_64, color = "orange", alpha=.1) +
  #Medium Vessel Listening Spaces-LF
  geom_sf(data = AC_Aug_MediumVessel_74, color = "blue", alpha=.1) + 
  geom_sf(data = DC_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = DT_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = GA_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = GC_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = LC_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = Y1B_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = Y1D_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = MC_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  geom_sf(data = NO_Aug_MediumVessel_74, color = "blue", alpha=.1) +
  #set up
  geom_sf(data = sites, color = "black", size = 1)  +
  geom_text(data  = dataPAM, aes(x=Longitude , y=Latitude +.3,  label = SiteID  ), size = 3)  +
  ggtitle("Listening Spaces- low frequency (to 100 nm at 0.1 km resolution) \n
          blue = medium vessel 165 dB rms @ 1kHz |  orange = large vessel 185 dB rms @ 125Hz | yellow = seismic 245 dB rms @ 63Hz") + 
  coord_sf(xlim = c(-102.15, -80), ylim = c(22, 33), expand = FALSE) 

# #Seismic Listening Spaces-LF
# geom_sf(data = AC_Aug_Seismic_81, color = "yellow", alpha=.1) + 
#   geom_sf(data = DC_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = DT_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = GA_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = GC_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = LC_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = Y1B_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = Y1D_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = MC_Aug_Seismic_81, color = "yellow", alpha=.1) +
#   geom_sf(data = NO_Aug_Seismic_81, color = "yellow", alpha=.1) +

# High-frequency calculations   ####
lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\"), pattern = "130.shp", recursive=TRUE, full.names = T )
namLVH = NULL
for (ii in 1: length( lsFiles )){
  namLVH[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namLVH[ii],tmp)
  
}
as.data.frame(namLVH)

lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\"), 
                      pattern = "105.shp", recursive=TRUE, full.names = T )
namMVH = NULL
for (ii in 2: length( lsFiles )){
  namMVH[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namMVH[ii],tmp)
  
}
as.data.frame(namMVH)

lsFiles = list.files( path = paste0(wrkDir, "data\\ListeningArea\\"), 
                      pattern = "135.shp", recursive=TRUE, full.names = T )
namSEH = NULL
for (ii in 1: length( lsFiles )){
  namSEH[ii] <- gsub(".shp", "", basename( lsFiles [ii]) )
  tmp = st_read(lsFiles[ii])
  tmp = tmp %>% st_set_crs(4326)
  tmp = st_as_sf(tmp)
  assign(namSEH[ii],tmp)
  
}
as.data.frame(namSEH)

## high-frequency
ggplot(data = world) +
  geom_sf( ) +
  geom_sf(data = region, alpha=0.1) + 
  geom_sf(data = dataBATH, alpha=0.2) + 
  geom_sf(data = sites, color = "black", size = 1)  +
  #Seismic Listening Spaces-HF
  geom_sf(data = AC_Aug_Seismic_135, color = "red", alpha=.1) + 
  geom_sf(data = DC_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = DT_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = GA_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = GC_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = LC_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = Y1B_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = Y1D_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = MC_Aug_Seismic_135, color = "red", alpha=.1) +
  geom_sf(data = NO_Aug_Seismic_135, color = "red", alpha=.1) +
  #LargeVessel Listening Spaces- HF
  geom_sf(data = AC_Aug_LargeVessel_130, color = "orange", alpha=.1) + 
  geom_sf(data = DC_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = DT_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = GA_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = GC_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = LC_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = Y1B_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = Y1D_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = MC_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  geom_sf(data = NO_Aug_LargeVessel_130, color = "orange", alpha=.1) +
  #Medium Vessel Listening Spaces-HF
  geom_sf(data = AC_Aug_MediumVessel_105, color = "blue", alpha=.1) + 
  geom_sf(data = DC_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = DT_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = GA_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = GC_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = LC_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = Y1B_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = Y1D_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = MC_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  geom_sf(data = NO_Aug_MediumVessel_105, color = "blue", alpha=.1) +
  #set up
  geom_sf(data = buff, color = "black", size = .5 ,alpha=.1)  +
  geom_text (data  = dataPAM, aes(x=Longitude , y=Latitude +.3,  label = SiteID  ), size = 3)  +
  ggtitle("Listening Spaces- high frequency (to 100 nm at 0.1 km resolution) \n
          blue = medium vessel 165 dB rms @ 1kHz |  orange = large vessel 185 dB rms @ 125Hz | red = seismic 245 dB rms @ 63Hz") + 
  #coord_sf(xlim = c(-85, -84), ylim = c(25.4, 25.8), expand = FALSE)
  #coord_sf(xlim = c(-84.65, -84.6), ylim = c(25.5, 25.55), expand = FALSE)
  #ggspatial::annotation_scale(location = 'tl')
  coord_sf(xlim = c(-102.15, -80), ylim = c(22, 33), expand = FALSE)
