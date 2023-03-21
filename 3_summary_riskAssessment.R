# GENERATE RISK ASSESSMENT RESULTS FOR EACH Month-Site-Species

rm(list=ls())

# libraries #### 
library(lubridate)
library(dplyr)
library(rnaturalearth)
library(sf)

# set up params #### 
# for mapping
WGS84proj = 4326
plt = "on"
sf_use_s2(FALSE) #avoids error with st_intersects
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")

mthName=NULL
mthName$Sp  = c(202107, 202008,202009,202010,202011,202012,202101,202102,202103,202104,202105,202106)
mthName$Lr  = c("August","August","August","October","October","October","Feburary","Feburary","Feburary","May","May","May") # for listening range data 
mthName$Sem  = c("July","August","September","October","November","December","January","Feburary","March","April","May","June") # seismic surveys days
mthName$AIS  = c("2021-07", "2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06") # for AIS data
mthName = as.data.frame(mthName) # unique file naming by month

# directories #### 
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\" 
AIS.dir =   paste0("H:\\AIS_MarineCad\\data\\GoMexRegion\\GoMexRegion_trim")

# Input data ####
## PAM ####
dataPAM   = read.csv(paste0(wrkDir, "data\\PAM\\GoM_PAMsites_CompiledV2.csv")) 

## species ####
dataSpecies   = list.files( path = paste0(wrkDir, "data\\RiskAssessment\\"), pattern = "SpOut_noVul", recursive=TRUE, full.names = T )
#load(dataSpecies[1])

## noise activity ####
dataNoise  = list.files( path = paste0(wrkDir, "data\\RiskAssessment\\"), pattern = "NaOut_daily", recursive=TRUE, full.names = T )
#as.data.frame( load(dataNoise[1]) )

#  vulnerability ####
filesVul = list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), pattern = ".csv", recursive=TRUE, full.names = T )

#  seismic days ####
dataSem = read.csv( list.files( path = paste0(wrkDir, "data\\Seismic\\"), pattern = "dates.csv", recursive=TRUE, full.names = T ) )
dataSem$Date = as.Date(dataSem$ï..Date,format="%d-%b-%y")
dataSem$mth = month( dataSem$Date )

## spatial layers ####

# Monthly Processing Species ####
# Total density across all sites for a specific month-- per specie
# used to calculate relative density

totalSpecies = NULL
for ( ii in 1:length(dataSpecies) ) {
  
  load(dataSpecies[ii])
  SpeciesOut$ListeingSpaceName = paste(SpeciesOut$Source, SpeciesOut$SNR, sep = "_")
  
  tmp = SpeciesOut %>% group_by(ListeingSpaceName, SpeciesLabel) %>%
    summarize(totalDensity = sum(Density_LS) )
  
  tmp$Mth = ( sapply ( strsplit( sapply(strsplit(basename( dataSpecies[ii] ),"[.]"), `[`, 1)  , "_", ), `[[`, 3) )
  
  totalSpecies = rbind(totalSpecies, tmp)
  
}
rm(SpeciesOut,tmp,ii)

# Relative density-- loops through each month
monthSpecies = NULL

for ( ii in 1:length(dataSpecies) ) {
  load(dataSpecies[ii])
  mth = ( sapply ( strsplit( sapply(strsplit(basename( dataSpecies[ii] ),"[.]"), `[`, 1)  , "_", ), `[[`, 3) )
  totalSp = as.data.frame( totalSpecies[totalSpecies$Mth == mth,] )
  SpeciesOut$ListeingSpaceName = paste(SpeciesOut$Source, SpeciesOut$SNR, sep = "_")
  
  # add vulnerability
  imth = mthName$Sem[ mthName$Sp == mth]
  dataVul = read.csv ( list.files( path = paste0(wrkDir, "data\\EWG_GOMEX\\"), 
                                   pattern = paste0(imth, ".csv"), recursive=TRUE, full.names = T ) )
  
  #loop through each site (in given month) to divide by total
  uSite = unique(SpeciesOut$Site)
  for (ss in 1:length(uSite)){
    
    tmp = SpeciesOut[SpeciesOut$Site== uSite[ss],]
    
    #vulnerability zone 
    vzone = dataPAM$VulerabilityZone[ dataPAM$SiteID == tmp$Site[1]]
    vultmp = dataVul[dataVul$Zone == vzone,]
    
    #loops through each line and finds value from totalSpecies
    for (rr in 1:nrow(tmp)){
      tmp2 = as.data.frame( totalSpecies[ totalSpecies$ListeingSpaceName == tmp$ListeingSpaceName[rr],] ) # trim by LR name
      tmp2 = as.data.frame( tmp2[ tmp2$SpeciesLabel == tmp$SpeciesLabel[rr],] ) # trim by species
      tmp2 = as.data.frame( tmp2[ tmp2$Mth == mth ,]) # trim by month
      
      tmp$Density_LRtotal[rr] = tmp2$totalDensity
      tmp$mth = mth
      tmp$DensityRel[rr] = tmp$Density_LS[rr]/tmp2$totalDensity
      
      # vulnerability score
      tmp$vulScore[rr] =  vultmp$Total.vulnerability [ vultmp$Species.ID == tmp2$SpeciesLabel]
      tmp$vulRate[rr] =   vultmp$Rating [ vultmp$Species.ID == tmp2$SpeciesLabel]
      
      monthSpecies = rbind(monthSpecies, tmp[rr,] ) 
      rm( tmp2)
    }
    rm(tmp)
  }
}

# select specific listening range results dependent on species hearing-- 
monthSpecies$Lab  = paste(monthSpecies$ListeingSpaceName, monthSpecies$mth, sep = "_")
monthSpecies$Lab2 = paste(monthSpecies$Source, monthSpecies$SNR, sep = "_")
monthSpecies$Month = ( substr( monthSpecies$mth, start = 5,stop = nchar(monthSpecies$mth)) )
monthSpecies$FQ = "HF"
monthSpecies$FQ [monthSpecies$SpeciesLabel == "Rices"] = "LF"
monthSpecies$SNR = as.numeric( as.character( monthSpecies$SNR))
monthSpecies$LR_SNR[monthSpecies$SNR > 100]  = "HF_SNR"
monthSpecies$LR_SNR[monthSpecies$SNR < 100]  = "LF_SNR"

#remove rows for different species-- 2 LS for each species!
monthSpecies$keep = 0
monthSpecies$keep[monthSpecies$LR_SNR == "HF_SNR" & monthSpecies$FQ == "HF"] = 1
monthSpecies$keep[monthSpecies$LR_SNR == "LF_SNR" & monthSpecies$FQ == "LF"] = 1
monthSpeciesT = monthSpecies[monthSpecies$keep == 1, ]

# plot by species across sites for LR 
umonth = unique(monthSpeciesT$mth)
monthSpeciesT$Order = 1
uSpecies = unique(monthSpeciesT$SpeciesLabel)
uorder = c("i","f","a","b","c","g","k","h","j","d","e" ) 
ulabs =  c("OceanicAtlSpotted","OceanicBottlenose","Pantropical","SpinnerDolphin","StripedDolphin","Blackfish","Pilot","Rissos"
           ,"Beaked","Sperm","Rices" )
for (i in 1:length(uSpecies)){
  idxO = which(monthSpeciesT$SpeciesLabel == uSpecies[i])
  tmpName = paste0(uorder[i],uSpecies[i])
  monthSpeciesT$Order[idxO] = tmpName
}

ggplot(monthSpeciesT, aes(x = Site, y = DensityRel)) +
  geom_point(aes( shape = as.factor(Source), color = mth), size = 2)  +
  facet_wrap(~SpeciesLabel, nrow=4 )+
  theme_light()  + theme(axis.text.x = element_text(angle = 10))

monthSpeciesTLV = monthSpeciesT[monthSpeciesT$Source == "LargeVessel",]
monthSpeciesTLV$MonthName[monthSpeciesTLV$Month == "08"] = "08 August"
monthSpeciesTLV$MonthName[monthSpeciesTLV$Month == "07"] = "07 July"
monthSpeciesTLV$MonthName[monthSpeciesTLV$Month == "09"] = "09 September"

ggplot(monthSpeciesTLV, aes(x = Site, y = Order)) +
  geom_tile(aes( fill = DensityRel),color = "white",lwd = 1.5 ) +
  geom_text(aes(label=round(DensityRel*100) ) ) +
  facet_wrap(~MonthName, nrow=1 )+
  scale_y_discrete(labels = ulabs) +
  scale_fill_distiller(palette = "Blues",direction = 1, name="Relative Density",) +
  labs(title = "",       y = "", x = "")+
  theme(legend.position="top")

#title = "Summary of relative species density across site and month \n with vulnerability score",

# OUTPUT species summary ####
write.csv(monthSpeciesT,file = paste0(wrkDir, "data\\RiskAssessment\\Species_RelDensity.csv"))
# copy into "species denisty" tab: https://docs.google.com/spreadsheets/d/1j-pGWWX0Nj-MpZ9UUCqg9VQHhoAwqtw7nYA0UDJzgdc/edit#gid=209993613

# get monthly summary
out1 = as.data.frame( monthSpeciesTLV %>% group_by(Site, MonthName, Source) %>%
                        summarize(above10 = sum(DensityRel > .10),  #sum(DensityRel > .10) /length(unique(monthSpeciesT$SpeciesLabel)
                                  avgVul = round( mean(vulScore) )) )
out1$mthNum = ( substr( out1$mth, start = 5, stop = nchar(out1$mth)) )
write.csv(out1,file = paste0(wrkDir, "data\\RiskAssessment\\BySite_RelDensity.csv"))

monthSpeciesTLVSum = monthSpeciesTLV[monthSpeciesTLV$DensityRel > .10,]

# PLOT species summary ####
ggplot(out1, aes(x=Site,y=MonthName))+
  geom_tile(aes( fill = above10), color = "white",lwd = 1.5 )+
  geom_text(aes(label=avgVul)) +
  facet_wrap(~Source)+
  scale_fill_distiller(palette = "Blues",direction = 1, name="Number of species above 10% \n relative density",) +
  labs(  y = "", x = "")+
  theme(legend.position="top")
#title = "Summary of relative species density across site and month \n with average vulnerability score"

# just Rice and sperm high priority
monthSpeciesTLV_HP = monthSpeciesTLV[ monthSpeciesTLV$SpeciesLabel == "Rices" | monthSpeciesTLV$SpeciesLabel == "Sperm",]
ggplot(monthSpeciesTLV_HP, aes(x = Site, y = SpeciesLabel)) +
  geom_tile(aes( fill = DensityRel),color = "white",lwd = 1.5 ) +
  geom_text(aes(label=round(DensityRel*100) ) ) +
  facet_wrap(~MonthName, nrow=1 )+
  scale_fill_distiller(palette = "Blues",direction = 1, name="Relative Density",) +
  labs(title = "",       y = "", x = "")+
  theme(legend.position="top")

# Monthly Processing AIS ####

# get totals across all sites for a specific month
totalNoise = NULL
siteNoise = NULL
for ( ii in 1:length(dataNoise) ) {
  load(dataNoise[ii])
  NoiseOut = as.data.frame(NoiseOut)
  mth = ( sapply ( strsplit( sapply(strsplit(basename( dataNoise[ii] ),"[.]"), `[`, 1)  , "_", ), `[[`, 3) )
  NoiseOut$ListeingSpaceName = paste(NoiseOut$Source, NoiseOut$SNR, sep = "_")
  NoiseOut$LargeVessels = as.numeric(as.character(NoiseOut$LargeVessels ))
  NoiseOut = NoiseOut %>% mutate(across(.cols=7:17, .fns=as.numeric))
  
  tmp = as.data.frame( NoiseOut %>% group_by(ListeingSpaceName) %>%
                         summarize(totalLV    = sum(LargeVessels,na.rm = T),
                                   avgLVspeed = mean(LargeSpeed,na.rm = T),
                                   totalMV    = sum(OtherVessels,na.rm = T),
                                   totalCargo = sum(cargo,na.rm = T),
                                   totalTanker = sum(tanker,na.rm = T),
                                   totalFish = sum(fish,na.rm = T),
                                   totalPass = sum(passenger,na.rm = T), 
                                   totalTug = sum(other,na.rm = T),
                                   totalOther = sum(other,na.rm = T) ) )
  
  tmp2 = as.data.frame( NoiseOut %>% group_by(ListeingSpaceName, Site) %>%
                          summarize(totalLV    = sum(LargeVessels,na.rm = T),
                                    avgLVspeed = mean(LargeSpeed,na.rm = T),
                                    totalMV    = sum(OtherVessels,na.rm = T),
                                    totalCargo = sum(cargo,na.rm = T),
                                    totalTanker = sum(tanker,na.rm = T),
                                    totalFish = sum(fish,na.rm = T),
                                    totalPass = sum(passenger,na.rm = T), 
                                    totalTug = sum(other,na.rm = T),
                                    totalOther = sum(other,na.rm = T) ) )
  
  
  tmp$Mth = mth
  tmp2$Mth = mth
  totalNoise = rbind(totalNoise, tmp)
  siteNoise = rbind(siteNoise, tmp2)
  
}

# get proportion of traffic for each vessel type
for (ii in 1:nrow( siteNoise)){
  tmp = totalNoise[totalNoise$ListeingSpaceName ==  siteNoise$ListeingSpaceName[ii],]
  tmp = tmp[tmp$Mth ==  siteNoise$Mth[ii],]
  
  siteNoise[ii,13:(13+8)] = siteNoise[ii,c(3, 5:11) ]/tmp[c(2,4:10) ] #proportion of total
  siteNoise[ii,13+9]  =   siteNoise[ii,4] - tmp[3] #difference from average speed
}
as.data.frame(colnames(siteNoise))
siteNoise$mthNum = ( substr( siteNoise$Mth, start = 5, stop = nchar(siteNoise$Mth)) )

# large vessel in large vessel LR
siteNoiseT = siteNoise[ siteNoise$ListeingSpaceName == "LargeVessel_64",]
ggplot(siteNoiseT, aes(y=mthNum,x=Site))+
  geom_tile(aes( fill = totalLV.1),color = "white",lwd = 1.5 )+
  geom_text(aes(label=round( avgLVspeed.1) )) +
  #facet_wrap(~ListeingSpaceName)+
  scale_fill_distiller(palette = "Reds",direction = 1, name="Proportion of total vessels",) +
  labs(title = "Summary of relative number of large vessels \n with difference from mean speed",
       y = "", x = "")

# not large vessel in large vessel LR
unique(siteNoise$ListeingSpaceName)
siteNoiseT = siteNoise[ siteNoise$ListeingSpaceName == "MediumVessel_74",]
ggplot(siteNoiseT, aes(x=mthNum,y=Site))+
  geom_tile(aes( fill = totalLV.1),color = "white",lwd = 1.5 )+
  scale_fill_distiller(palette = "Reds",direction = 1, name="Proportion of total vessels",) +
  labs(title = "Summary of relative number of medium vessels (<100 m)",
       y = "", x = "")

siteNoiseT = siteNoise[ siteNoise$ListeingSpaceName == "MediumVessel_74" | siteNoise$ListeingSpaceName == "LargeVessel_64",] 
unique(siteNoiseT$ListeingSpaceName)
colnames(siteNoiseT)

siteNoiseTm = reshape2 :: melt(siteNoiseT, id.vars = c("Site", "ListeingSpaceName", "mthNum"), measure.vars =  colnames(siteNoiseT)[15:20] ) 
# need to remove listening space for each vessel category
# CARGO,TANKER use "LargeVessel_64"
unique(siteNoiseTm$variable)
siteNoiseTm$keep[ siteNoiseTm$variable == "totalCargo.1"  & siteNoiseTm$ListeingSpaceName == "LargeVessel_64"] = 1
siteNoiseTm$keep[ siteNoiseTm$variable == "totalTanker.1" & siteNoiseTm$ListeingSpaceName == "LargeVessel_64"] = 1
siteNoiseTm$keep[ siteNoiseTm$variable == "totalFish.1" & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 1
siteNoiseTm$keep[ siteNoiseTm$variable == "totalPass.1" & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 1
siteNoiseTm$keep[ siteNoiseTm$variable == "totalTug.1" & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 1
siteNoiseTm$keep[ siteNoiseTm$variable == "totalOther.1 " & siteNoiseTm$ListeingSpaceName == "MediumVessel_74"] = 1

siteNoiseTm2 = filter(siteNoiseTm, siteNoiseTm$keep == 1)
ggplot(siteNoiseTm2, aes(x=mthNum,y=Site))+
  geom_tile(aes( fill = value), color = "white",lwd = 1.5 )+
  facet_wrap(~variable)+
  scale_fill_distiller(palette = "Reds",direction = 1, name="Proportion of total vessels",) +
  labs(title = "Summary of relative number of medium vessels (<100 m)",
       y = "", x = "")

# get monthly summary-- count of vessel types >10% (like species)
outVes1 = as.data.frame( siteNoiseTm2 %>% group_by(Site, mthNum) %>%
                        summarize(above10 = sum(value > .10),  #sum(DensityRel > .10) /length(unique(monthSpeciesT$SpeciesLabel)
                                  avgVes = ( mean(value) )) )
ggplot(outVes1, aes(x=mthNum,y=Site))+
  geom_tile(aes( fill = above10), color = "white",lwd = 1.5 )+
  #geom_text(aes(label= round(avgVes*100) )) +
  scale_fill_distiller(palette = "Reds",direction = 1, name="Number of vessel types above \n 10% relative activity",) +
  labs(title = "Summary of vessel activity across site and month",
       y = "", x = "")
#copied values to here: https://docs.google.com/spreadsheets/d/1j-pGWWX0Nj-MpZ9UUCqg9VQHhoAwqtw7nYA0UDJzgdc/edit#gid=490156703
outVes1[ outVes1$mthNum == "08", ]  
outVes1[ outVes1$mthNum == "09", ]
outVes1[ outVes1$mthNum == "07", ]
#what vessels
siteNoiseTm2[siteNoiseTm2$mthNum == "08" & siteNoiseTm2$value >.1, ]
siteNoiseTm2[siteNoiseTm2$mthNum == "09" & siteNoiseTm2$value >.1, ]
siteNoiseTm2[siteNoiseTm2$mthNum == "08" & siteNoiseTm2$value >.1, ]

# ADD add seismic days to NoiseOut ####
dataSeismic   = read.csv(paste0(wrkDir, "data\\Seismic\\NFWF seismic dates.csv")) 
dataSeismic$Date = as.Date( dataSeismic$ï..Date, format = "%d-%b-%y")
dataSeismic$Mth = month( dataSeismic$Date, label = TRUE, abbr = FALSE)
idx = !is.na(dataSeismic$Lat)
dataSeismicT = dataSeismic[idx,]
seismicLocations = st_as_sf(data.frame( latitude = dataSeismicT$Lat, longitude = dataSeismicT$Lon, label = dataSeismicT$Mth ),
                            coords = c("longitude", "latitude"), crs = WGS84proj, 
                            agr = "constant") 
mthS = "August"
dataSeismicMth = dataSeismic[dataSeismic$Mth == mthS,] #trim data to just this month
nDys = unique( dataSeismicMth$Date)
sum(dataSeismicMth$Active)

for (d in 1:length(nDys)){
  #sum the activities
  tmp = dataSeismicMth[ dataSeismicMth$Date == nDys[d],]
  #cat(as.character(nDys[d]), sum(tmp$Active), "\n")
  
  #select rows of NoiseOut to add value includes all sites and ls
  NoiseOut$Seismic[ NoiseOut$Day == nDys[d]] = sum(tmp$Active)
  
}

# COMBINE results and score ####

## save out values ####














