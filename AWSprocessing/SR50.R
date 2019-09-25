################################################################################
# SR50 data cleaner function - create daily time series of snow depth
# 
# SR50.R
#
# ReadMe:
# Uses raw SR50 data from csv files (format:Date,Time,T,RH,SDQ,SD,WS columns necessary with these headers) and filters data to produce daily snow height vectors
#
# requires TSaggregate Code from the d2EB package (check at https://github.com/fidelsteiner)
# Output:
# .csv file with daily SD values
#
# Created:          2019/08/15
# Latest Revision:  2019/08/22
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht 
# Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 
################################################################################

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

# install necessary packages if not available yet via install.packages()
require(zoo)
require(lubridate)
require(data.table)

source('F:\\PHD\\Research\\EB_DCG\\DistributedEB\\Code\\subCodes\\TSaggregate.R')

SR50 <- function(pathin,z_sr50,pathout){
sr50raw <- read.csv(pathin,header = 1)
timest =  as.POSIXlt(paste(sr50raw$Date,sr50raw$Time),format="%m/%d/%Y  %H:%M:%S")   #time string

# Filter data flagged, far enough from the sensor and retrieved at RH<80%, as well as WS<5 m/s
filter <- which(sr50raw$SD>0.5&sr50raw$SDQ<250&sr50raw$RH<80&sr50raw$WS<5) # quality filter of all snow data
sr50raw[['sr50height']]<-NA*sr50raw$SD
sr50raw$sr50height[filter]<-sr50raw$SD[filter]

# smooth data, filtering away noise
smoothed<-supsmu(1:length(sr50raw$sr50height),na.fill(sr50raw$sr50height,"extend"),bass=5,span=0.004)

# Aggregate to daily values (mean, min and max)
SD_daily <- TSaggregate(z_sr50-smoothed$y,timest,24,2010,'mean')
SD_daily_max <- TSaggregate(z_sr50-smoothed$y,timest,24,2010,'max')[,2]
SD_daily_min <- TSaggregate(z_sr50-smoothed$y,timest,24,2010,'min')[,2]
SD_daily <- cbind(SD_daily,SD_daily_max,SD_daily_min)
colnames(SD_daily) <- c('time','mean','sd','max','min')

# save File in specified folder/filename
write.csv(SD_daily,file = pathout)
}