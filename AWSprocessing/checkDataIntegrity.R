################################################################################
# Check integrity of date column of the station data
# 
# checkDataIntegrity.R
# 
#
# Author: Philip Kraaijenbrink
# Created:          2020/05/05
# Latest Revision:  2020/05/05
#
# mountainhydrology group UU |  www.mountainhydrology.org 
################################################################################

# Clear plots and workspace
if(!is.null(dev.list())) dev.off()
rm(list=ls())

## get files
dataroot <- './data'
infiles <- list.files(dataroot, patt='.csv$', full=T) 

## read CSV with meteodata
integrity <- do.call(rbind, lapply(infiles, function(fn){
  print(fn)
  meteo <- read.csv(fn, header=TRUE, sep=",", stringsAsFactors=F)
  meteo <- setNames(meteo, tolower(names(meteo)))
  meteo$datestamp <- as.Date(meteo$date, format='%Y-%m-%d')
  data.frame(filename=basename(fn),
             proper_date_order=sum(meteo$datestamp != sort(meteo$datestamp)) == 0,
             na_values_among_dates=sum(is.na(meteo$datestamp))>0)
}))
integrity
            
