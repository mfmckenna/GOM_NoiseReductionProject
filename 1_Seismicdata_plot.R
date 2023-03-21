## GoMex Project:  seismic activity timing
rm(list = ls())
library(timevis)
wrkDir = "G:\\My Drive\\ActiveProjects\\COA\\NFWF_GOM_workingDrive\\data\\Seismic\\"

df = read.csv( paste0( wrkDir, "US GOM Seismic 2020-2021.csv") )
df$start   = as.Date( df$Start.Production, format = "%b %d %Y" )
df$end    =  as.Date( str_trim(df$End.Production),   format = "%b %d %Y" )
df$content = df$Type 
df$id = 1:nrow(df)
df[nrow(df)+1,] <- NA

#add study period 
df$start[nrow(df)] = as.Date( "Aug 1 2020" ,format = "%b %d %Y")
df$end[nrow(df)] = as.Date( "Aug 30 2021" ,format = "%b %d %Y")
df$content[nrow(df)] = "Study Period"
df$id [nrow(df)] = nrow(df)
timevis(df)


