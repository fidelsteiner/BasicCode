################################################################################
# SR50 data cleaner function - create daily time series of snow depth
# Based on original code from Maxime Litt to quality check snow depth data
# 
# SR50.R
#
# ReadMe:
# Uses raw SR50 data from csv files (format:Date,Time,T,RH,SDQ,SD,WS columns necessary with these headers) and filters data to produce daily snow height vectors
#
# SD_reverse: 1: SD is the distance between sensor and ground, 0: SD is the actual depth of snow
# 
# requires TSaggregate Code from the d2EB package (check at https://github.com/fidelsteiner/BasicCode/tree/master/timeseriesAnalysis )
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

# Requires additional code: 'https://github.com/fidelsteiner/BasicCode/blob/master/timeseriesAnalysis/TSaggregate.R'
source('.../TSaggregate.R')

SR50 <- function(pathin,z_sr50,pathout,SD_reverse, mon_NoSnow){
sr50raw <- read.csv(pathin,header = 1)
timest =  as.POSIXlt(paste(sr50raw$Date,sr50raw$Time),format="%m/%d/%Y  %H:%M:%S")   #time string

# Filter data flagged, far enough from the sensor and retrieved at RH<80%, as well as WS<5 m/s
check=function(x) tryCatch(if(class(x) == 'logical') 1 else 1, error=function(e) 0) 
RH_exist <- is.null(sr50raw$RH)
WS_exist <- is.null(sr50raw$WS)
SDQ_exist <- is.null(sr50raw$SDQ)
T_exist <- is.null(sr50raw$T)

if(SD_reverse==0){
  sr50raw$SD <- z_sr50 - sr50raw$SD
  }  

if(RH_exist){sr50raw$RH <- sr50raw$SD*0}
sr50raw$RH[is.na(sr50raw$RH)] <- 0
if(WS_exist){sr50raw$WS <- sr50raw$SD*0}
sr50raw$WS[is.na(sr50raw$WS)] <- 0
if(SDQ_exist){sr50raw$SDQ <- sr50raw$SD*0}
sr50raw$SDQ[is.na(sr50raw$SDQ)] <- 0

filter <- which(sr50raw$SD>0.5&sr50raw$SDQ<250&sr50raw$RH<80&sr50raw$WS<5) # quality filter of all snow data
sr50raw[['sr50height']]<-NA*sr50raw$SD
sr50raw$sr50height[filter]<-sr50raw$SD[filter]

# smooth data, filtering away noise
smoothed<-supsmu(1:length(sr50raw$sr50height),na.fill(sr50raw$sr50height,"extend"),bass=5,span=0.004)

# Aggregate to daily values (mean, min and max)
actualSD <- z_sr50-smoothed$y
actualSD[actualSD <= 0] <- 0
#browser()
# Remove snow depth measurements in warm periods where it is likely vegetation
MonthID <- month(as.POSIXct(TSaggregate(sr50raw$SD,timest,24,2010,'mean')[,1],origin='1970-01-01'))
actualSD[which(!is.na(match(MonthID,mon_NoSnow)))] <- 0

SD_daily <- TSaggregate(actualSD,timest,24,2010,'mean')
SD_daily_max <- TSaggregate(actualSD,timest,24,2010,'max')[,2]
SD_daily_min <- TSaggregate(actualSD,timest,24,2010,'min')[,2]

SD_daily <- cbind(as.character(as.POSIXct(SD_daily[,1],origin='1970-01-01')),SD_daily,SD_daily_max,SD_daily_min)
colnames(SD_daily) <- c('datestring','datenumeric','mean','sd','max','min')

# save File in specified folder/filename
write.csv(SD_daily,file = pathout, row.names=FALSE)
}