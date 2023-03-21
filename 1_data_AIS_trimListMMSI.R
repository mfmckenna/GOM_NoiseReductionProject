# PURPOSE: Use previously trimmed AIS files to  ####
#1) list of MMSI per month-- share with JJ
#2) trim even further to help with processing save for next steps

rm(list = ls())
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\"
sf_use_s2(FALSE) #avoids error with st_intersects
WGS84proj = 4326

# INPUT: TOTAL ANALYSIS AREA ####
region = sf::st_read(dsn =  paste0(wrkDir, "data\\region\\ROI.dbf"), layer = "ROI")

# INPUT: GoMex trimmed AIS FILES ####
AIS.dir  =   paste0("H:\\AIS_MarineCad\\data\\GoMexRegion")

# processing loop ####
mths = c("2020-08", "2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08" )

for (mm in 1:length(mths)) {
  
  filpart = paste0("AIS_GoMex_", mths[mm]) 
  dysFiles  =  list.files(path = AIS.dir, pattern = filpart, recursive=TRUE, full.names = TRUE) 
  
  MMSIout = NULL
  for (ff in 1:length(dysFiles) ) {
    
    cat("Processing...", mths[mm], ': ', ff, ' of ', length(dysFiles), " days", "\n") 
    
    load(dysFiles[ff]) 
    AISday = unique( as.Date( df2t$timestamp ) )
    
    ## intersect data with region ####
    tst = lengths(st_intersects(df2t, region$geometry )) > 0
    df2t2 = df2t[tst,]
    AISships = unique( df2t2$MMSI )
    #AISships = length( unique( df2t$MMSI ) )- significant reduction in unique vessels- yay!
    
    ## save MMMSI ####
    MMSIout = c(MMSIout, AISships)
    
    ## save new Rdata ####
    save(df2t2, file = paste0(AIS.dir, "\\GoMexRegion_trim\\AIS_GoMex_", AISday, ".RData") )
    
  }
  MMSIout2 = unique(MMSIout)
  
  #write.csv(MMSIout2, file = paste0(wrkDir,"data\\AIS\\" , filpart, "_MMMSIout.csv") )
}


