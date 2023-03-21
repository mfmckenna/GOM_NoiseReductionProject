# Extracting noise activity around each PAM site-- 20 km buffer

#outputs ####
# 1) map with all data layers
# copy to https://docs.google.com/spreadsheets/d/1j-pGWWX0Nj-MpZ9UUCqg9VQHhoAwqtw7nYA0UDJzgdc/edit#gid=604240668
 
# inputs ####
# activity: lease areas, rigs, wind energy
# other info: vulnerability zone, PAM levels

# libraries #### 
rm(list=ls())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)
library(rgdal)
library(mgcv)

# set up #### 
WGS84proj = 4326
plt = "on"
sf_use_s2(FALSE) #avoids error with st_intersects
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")

# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 

# ANALYSIS AREA   ####
region = sf::st_read(dsn =  paste0(wrkDir, "data\\region\\ROI.dbf"), layer = "ROI")

## bathymetry  ####
dataBATH   = lsFiles = list.files( path = paste0(wrkDir, "data\\bathymetry\\"), pattern = ".shp", recursive=TRUE, full.names = T )
tmp = st_read(dataBATH)
tmp = tmp %>% st_set_crs(4326)
dataBATH = st_as_sf(tmp)

## PAM sites  ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) #this version has updated lat/lons
dataPAM   = dataPAM[1:10,1:7] #remove NA rows
sites = st_as_sf(data.frame( latitude = dataPAM$Latitude, longitude = dataPAM$Longitude ),
                 coords = c("longitude", "latitude"), crs = WGS84proj, 
                 agr = "constant") 

## 20 km buffer ####
pois = st_transform(sites, crs = 7801)
pt_buffer = st_buffer(pois,dist = 20000,) # just make 10 km buffer/polygon
buff = st_transform(pt_buffer, crs = 4326)

## Vulnerability ####
zone1 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone1v2_EPA.dbf"), layer = "Zone1v2_EPA")
zone2 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone2v2_All.dbf"), layer = "Zone2v2_All")
zone3 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone3v2_WPA.dbf"), layer = "Zone3v2_WPA")
zone4 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone4v2_EPA2.dbf"), layer = "Zone4v2_EPA2")
zone5 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone5clip.dbf"), layer = "zone5clip")
zone6 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\zone6clip.dbf"), layer = "zone6clip")
zone7 = sf::st_read(dsn =  paste0(wrkDir, "data\\EWG_GOMEX\\Zone7_Clip.dbf"), layer = "Zone7_clip")

## offshore Oil  ####
RIGS = sf::st_read(dsn =  paste0(wrkDir, "data\\gom_platforms\\gom_platforms.dbf"), layer = "gom_platforms")
RIGS2 = st_transform(RIGS, crs = 4326)
RIGSYes = RIGS2[ RIGS2$Status == "IN SERVICE", ]
RIGSNo = RIGS2[ RIGS2$Status != "IN SERVICE", ]
ActiveLease = sf::st_read(dsn =  paste0(wrkDir, "data\\actlease\\al_20220801.dbf"), layer = "al_20220801")
ActiveLease = st_transform(ActiveLease, crs = 4326)

## offshore WIND ####
WIND = sf::st_read(dsn =  paste0(wrkDir, "data\\GOM_CallAreaTo400m.gdb"), layer = "GOM_CallAreaTo400m")
WIND2 = st_transform(WIND, crs = 4326)
WEA1 = sf::st_read(dsn =  paste0(wrkDir, "data\\WEA_option_I_M_shapes_w_metadata\\Option_I.dbf"), layer = "Option_I")
WEA2 = sf::st_read(dsn =  paste0(wrkDir, "data\\WEA_option_I_M_shapes_w_metadata\\Option_M.dbf"), layer = "Option_M")

## seismic days ####
dataSeismic   = read.csv(paste0(wrkDir, "data\\Seismic\\NFWF seismic dates.csv")) 
dataSeismic$Date = as.Date( dataSeismic$ï..Date, format = "%d-%b-%y")
dataSeismic$Mth = month( dataSeismic$Date)
idx = !is.na(dataSeismic$Lat)
dataSeismicT = dataSeismic[idx,]
seismicLocations = st_as_sf(data.frame( latitude = dataSeismicT$Lat, longitude = dataSeismicT$Lon, label = dataSeismicT$Mth ),
                            coords = c("longitude", "latitude"), crs = WGS84proj, 
                            agr = "constant") 

# OUTPUT: MAP ####
# ? ####
# add legend to the maps
ggplot(data = world) +
  geom_sf( ) +
  
  # vulnerability zones
  geom_sf(data = zone1, alpha=0.1, color = "black") + 
  geom_sf(data = zone2, alpha=0.1, color = "black") + 
  geom_sf(data = zone3, alpha=0.1, color = "black") + 
  geom_sf(data = zone4, alpha=0.1, color = "black") + 
  geom_sf(data = zone5, alpha=0.1, color = "black") + 
  geom_sf(data = zone6, alpha=0.1, color = "black") + 
  geom_sf(data = zone7, alpha=0.1, color = "black") + 
  geom_sf(data = region, alpha=0.1, color = "magenta") + # study area , aes(lwd = .3)+
  
  geom_sf(data = dataBATH, alpha=0.2) + #bathymetry

  #Offshore infrastructure
  geom_sf(data = RIGSYes, alpha = .3, color = "black") + # aes(color = factor(Status))
  geom_sf(data = ActiveLease, alpha=0.1, color = "gray" ) + 
  geom_sf(data = WEA1, alpha=0.1, color = "orange") + 
  #geom_sf(data = WIND2, alpha=0.1, color = "green") + 
  geom_sf(data = WEA2, alpha=0.1, color = "orange") + 
  geom_sf(data = seismicLocations, alpha = 1, color = "red") + 
  
   #PAM sites
  geom_sf(data = sites, color = "blue") +
  geom_sf(data = buff, color  = "blue", fill = "white", alpha = .3) +
  geom_text (data  = dataPAM, aes(x=Longitude , y=Latitude +.3,  label = SiteID  ), size = 3,color = "blue")  +
  
  #scale_color_identity(labels = c(yellow = "Yellow Label"), guide = "legend") +
  coord_sf(xlim = c(-100, -80), ylim = c(22, 30.5), expand = FALSE)

# OUTPUT: number of rigs within 10 km of each site ####
#copy results of operators separately
for (ss in 1:dim(buff)[1]){
  
  tstRIG = lengths(st_intersects(RIGSYes, buff$geometry[ss])) > 0
  if ( sum( tstRIG ) >=1 ){
    
     cat( dataPAM$SiteID[ss],  sum( tstRIG ), RIGSYes$Operator[tstRIG] ,RIGSYes$Status[tstRIG], "\n" )
   
  }
  
  dataPAM$operational_RIGSRIGS[ss] = sum( tstRIG )
  
}
for (ss in 1:dim(buff)[1]){
  
  tstRIG = lengths(st_intersects(RIGSNo, buff$geometry[ss])) > 0
  if ( sum( tstRIG ) >=1 ){
    
    cat( dataPAM$SiteID[ss],  sum( tstRIG ), RIGSNo$Operator[tstRIG] ,RIGSNo$Status[tstRIG], "\n" )
    
  }
  
  dataPAM$nonOperational_RIGS[ss] = sum( tstRIG )
  
}
dataPAM

# OUTPUT: lease areas in PAM buffer ####
st_crs(ActiveLease) == st_crs(buff$geometry[ss])
for (ss in 1:dim(buff)[1]){
  
 #length( intersect(ActiveLease, buff$geometry[ss] ) )
  
  tstRIG = lengths(st_intersects(ActiveLease, buff$geometry[ss])) > 0
  cat( dataPAM$SiteID[ss],  ( sum(tstRIG ) ), "\n" )
  
 dataPAM$Lease[ss] = sum( tstRIG )
  
}

# SPLs ####
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\"

PAM.Dir   = paste0(wrkDir, "data\\PAM")
PAMFilesMedian  = list.files(path = PAM.Dir, pattern= "MFmedian" , recursive=TRUE, full.names = TRUE)
medSPL = as.data.frame( read.csv(PAMFilesMedian ) )
dim(medSPL)
length( medSPL$AC ) # days in analysis
Fq = as.character( seq(from = 20, to = 4000, by = 10) )
colnames( medSPL)  = c("site", "date", Fq)
medSPL$date = as.Date( medSPL$date , format = " %d-%b-%Y")
medSPL2 = medSPL[medSPL$site != "CE" , ]
medSPL2 = medSPL2[medSPL2$site != "MR" , ]
medSPL2 = medSPL2[medSPL2$site != "Y1C" , ]

medSPLm = reshape:: melt (medSPL2, id.vars = c("date","site"),  measure.vars = colnames(medSPL)[4:ncol(medSPL)-1] )
colnames( medSPLm)  = c("date", "site", "Fq","SPL")
medSPLm$Fq = as.numeric( as.character(medSPLm$Fq) )

# OUTPUT: plot the spectra for each date- for each site 
ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
  geom_line(alpha = .2 ) + 
  scale_x_log10() +
  geom_vline(xintercept=120, linetype="dashed", color = "red")+
  geom_vline(xintercept=1000, linetype="dashed", color = "red")+
  facet_wrap(~as.factor(site) ) +  
  ylab("daily median SPL (10 Hz bands)")+ xlab("Frequency (Hz)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# OUTPUT: plot the median in specidic FQ for all days at each date- for each site 
PAM.Dir   = paste0(wrkDir, "data\\PAM")
PAMFilesMedian  = list.files(path = PAM.Dir, pattern= "MFpctile" , recursive=TRUE, full.names = TRUE)
pctSPL = as.data.frame( read.csv(PAMFilesMedian ) )
Fq = as.character( seq(from = 20, to = 4000, by = 10) )
colnames( pctSPL)  = c("site", "date", "120Hz_25","120Hz_50","120Hz_75","1kHz_25","1kHz_50","1kHz_75")
pctSPL$date = as.Date( pctSPL$date , format = " %d-%b-%Y")

pctSPL2 = pctSPL[pctSPL$site != "CE" , ]
pctSPL2 = pctSPL2[pctSPL2$site != "MR" , ]
pctSPL2 = pctSPL2[pctSPL2$site != "Y1C" , ]

ggplot(pctSPL2, aes(x = site, y=`120Hz_50` ) ) +
  geom_boxplot( ) + 
  ylab("daily median SPL (120-130 Hz)")+ xlab("")+
  theme_minimal() 

ggplot(pctSPL2, aes(x = site, y=`1kHz_50` ) ) +
  geom_boxplot( ) + 
  ylab("daily median SPL (1 - 1.01 kHz)")+ xlab("")+
  theme_minimal() 

# OUTPUT: median values per site #### 
uSite = unique(dataPAM$SiteID)
siteSPL = NULL
for (ss in 1:length(uSite)){
  dataPAM$SPL_120kHz_50[ss]  =  mean( pctSPL2$`120Hz_50`[pctSPL2$site == uSite[ss]] )
  dataPAM$SPL_1kHz_50[ss]    =  mean( pctSPL2$`1kHz_50` [pctSPL2$site == uSite[ss]] )
}
dataPAM
