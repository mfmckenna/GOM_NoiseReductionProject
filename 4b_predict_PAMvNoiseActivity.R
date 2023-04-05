## GoMex Project:  predicting SPL from Noise Activity Index

#updated the input from the PAM data
# ListenGoMex2020_AmbientSamples4COA_20220426.mat
# ExtractData.m

rm(list = ls())
library(mgcv)
library(ggplot2)
library(lubridate)
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\"

# INPUT DATA: PAM SPL values (response variable)
#-----------------------------------------------------
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

# plot the spectra for each date- for each site 
ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
  geom_line(alpha = .2 ) + 
  scale_x_log10() +
  geom_vline(xintercept=120, linetype="dashed", color = "red")+
  geom_vline(xintercept=1000, linetype="dashed", color = "red")+
  facet_wrap(~as.factor(site) ) +  
  ylab("daily median SPL (10 Hz bands)")+ xlab("Frequency (Hz)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Select specific data to compare...
dts = ( as.data.frame( unique( medSPLm$date ) ) )
medSPLm$mth = month( medSPLm$date )
yesS = medSPLm[ medSPLm$date  == as.Date("2020-09-04"), ] #seismic activity
noS = medSPLm[ medSPLm$date  == as.Date("2020-11-09"), ] #no seismic activity

unique(noS$site)
unique(yeS$site)

ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
  geom_line(alpha = .2 ) + 
  geom_line(data = noS, aes(x = Fq, y=SPL), color = "blue")  +
  geom_line(data = yesS, aes(x = Fq, y=SPL), color = "red")  +
  scale_x_log10() +
  geom_vline(xintercept=120, linetype="dashed", color = "gray")+
  geom_vline(xintercept=1000, linetype="dashed", color = "gray")+
  facet_wrap(~as.factor(site) ) +  
  ylab("daily median SPL (10 Hz bands)")+ xlab("Frequency (Hz)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



 # plot the median in specidic FQ for all days at each date- for each site 
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


# INPUT DATA: Human Activity Index  values (predictor variable)
#-----------------------------------------------------
NoiseFiles  = list.files(path = PAM.Dir, pattern= "DC_NoiseActivityIndex" , recursive=TRUE, full.names = TRUE)
NoiseIdx = NULL
for (ff in 1:length(NoiseFiles )){
  load(NoiseFiles[ff])
  
  NoiseIdx = rbind(NoiseIdx, outputDaySite) 
  rm(outputDaySite)
}
NoiseIdx = as.data.frame(NoiseIdx)

#some formatting
NoiseIdx = NoiseIdx %>% mutate_at(c(4:12), as.numeric)
colnames(NoiseIdx)[1] = "site"
colnames(NoiseIdx)[2] = "SiteName"
NoiseIdx$Day = as.Date( NoiseIdx$Day )
as.data.frame(colnames(NoiseIdx)) 

ggplot(NoiseIdx, aes(x = Day, y = uships_LV_large, size = SOG_LV_large) ) +
  geom_point()+
  facet_wrap(~site)+
  ylab("Large vessels in listening space")+
  theme_minimal()

# MERGE DATA
#-----------------------------------------------------
tmp = merge(PAM_LowFreq, NoiseIdx, by = c( "Day", "site") )


# INITIAL PLOTS: 
#-----------------------------------------------------
ggplot(tmp, aes(x =uships_LV_large , y = sound_50 , size =  SOG_LV_large))+
  geom_smooth(method = "loess")+
  geom_point(alpha = 0.8)+
  xlab("Unique Vessels (> 100 m)")+
  ylab("median SPL (125 Hz TOL)")+
 
  ylim(c(50,100))+
  labs(size='average SOG')+
  theme_minimal()

# MODEL BUILD: 125 Hz TOL median ~ large vessels + speed + medium vessels + speed + rigs + seismic 
#-----------------------------------------------------
options(na.action = "na.omit")
as.data.frame(colnames(tmp))

global.gam125 = gam(sound_50 ~ 
                       s(uships_LV_large) +  # large vessel in large LS
                       s(SOG_LV_large) + 
                      (uships_MV_other) ,    # large vessel speed in large LS
                     data=tmp , method="REML", select=TRUE, gamma=1.4,na.rm = TRUE)
                     

# 
plot(global.lme125)


