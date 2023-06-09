#GoMex Project:

# unzip downloaded daily files from https://marinecadastre.gov/ais/
# runs on specific month of data and generates daily files for the entire region


rm(list = ls())

library(utils)
yrmth = "202108" #change this for each month
zipDir = paste0("H:\\AIS_MarineCad\\downloaded\\", yrmth)
uzipDir= paste0("H:\\AIS_MarineCad\\data\\", yrmth)

inFiles = list.files(zipDir, full.names = T)

for (ff in 1:length(inFiles)) {
  zipfile = inFiles[ff]
  unzip(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = uzipDir, unzip = "internal",
        setTimes = FALSE)
}


