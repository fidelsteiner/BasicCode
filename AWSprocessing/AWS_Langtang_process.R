################################################################################
# AWS Data Reader for Langtang catchment, aggregating 10 min to 60 min data
# 
# AWS_Langtang_process.R
#
# ReadMe: 
# (1) The original .txt file needs to be saved as a csv file, with only 1 hearder row which denotes the different variables
# (2) For incoming SW radiation only negative and NaN values are corrected; unrealistic high values have to be sorted separately
# (3) For outgoing SW radiation only negative, NaN and values higher than incoming are corrected
# (4) For RH, values<0 and >100 are set to 0 and 100
#
# Reads in the raw 10/60 min file from the AWS and makes the final AWS data file (in 60 min)
# Created:          2018/02/05
# Latest Revision:  2017/02/05
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht 
# Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 
################################################################################
# clear entire workspace (excl. packages)
rm(list = ls())
gc()

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

source("F:\\PhD\\BasicCode\\timeseriesAnalysis\\TSaggregate.R")

winddirmean <- function(ws,wd,timestr,timStep,timShift){
  u =-ws*sin(wd*pi/180)
  v =-ws*cos(wd*pi/180)
  
  umean <- TSaggregate(u,timestr,timStep,timShift,'mean')
  vmean <- TSaggregate(v,timestr,timStep,timShift,'mean')

  windvec <- umean
  windvec[,2] <- windvec[,2]*NA

  windvec[umean[,2]>0,2] <- (90-180/pi*atan(umean[umean[,2]>0,2]/vmean[umean[,2]>0,2])+180)
  windvec[umean[,2]<=0,2] <- (90-180/pi*atan(umean[umean[,2]<=0,2]/vmean[umean[,2]<=0,2])+180)
  windvec[,3] <- windvec[,3]*NA
  return(windvec)
}

# Location Kyanjing
path_kya <- 'F:\\PhD\\FieldWork\\ProcessingFieldData\\LangtangAutumn2019'
raw10minFile <- '2019_KyanjingAWS_RAW.csv'            # generally includes all climate data
raw60minFile <- '2019_KyanjingAWS_60min_RAW.csv'      # generally includes SR50 data

datRaw <- read.table(path_kya&'\\'&raw10minFile,header = T, sep = ",", dec = ".")

timestr <- as.POSIXct(datRaw$TIMESTAMP, format="%m/%d/%Y  %H:%M")

# Note that for Kyanjing the radiation components in the raw 10 min data are wrongly identified (up==down and down==up)
BVOLhourly <- TSaggregate(datRaw$BattV_Min,timestr,60,5,'mean')   # BVOL, Battery status [V] 
BCONhourly <- TSaggregate(datRaw$Bucket_RT,timestr,60,5,'mean')   # BCON, Bucket content [mm]
PVOLhourly <- TSaggregate(datRaw$Accumulated_RT_NRT_Tot,timestr,60,5,'sum') # PVOL, insttantaneous precipitation, mm
TAIRhourly <- TSaggregate(datRaw$AirTC_Avg,timestr,60,5,'mean')   # TAIR, mean air temperature, [degC]
datRaw$RH_Avg[datRaw$RH_Avg>100] <- 100
datRaw$RH_Avg[datRaw$RH_Avg<0] <- 0
RHhourly <- TSaggregate(datRaw$RH_Avg,timestr,60,5,'mean')   # RH, mean air rel humidity, [%]
TCNR4hourly <- TSaggregate(datRaw$CNR4TC_Avg,timestr,60,5,'mean')   # TCNR4, mean CNR4 temperature, [degC]

datRaw$SUp_Avg[datRaw$SUp_Avg<0]<-0
datRaw$SDn_Avg[datRaw$SDn_Avg<0]<-0
datRaw$SDn_Avg[datRaw$SDn_Avg>datRaw$SUp_Avg] <- datRaw$SUp_Avg[datRaw$SDn_Avg>datRaw$SUp_Avg]
KINChourly <- TSaggregate(datRaw$SUp_Avg,timestr,60,5,'mean')   # KINC, incoming SW radiation, [W m-2]
KUPWhourly <- TSaggregate(datRaw$SDn_Avg,timestr,60,5,'mean')   # KUPW, outgoing SW radiation, [W m-2]
LINChourly <- TSaggregate(datRaw$LUpCo_Avg,timestr,60,5,'mean')   # LINC, incoming LW radiation, [W m-2]
LUPWhourly <- TSaggregate(datRaw$LDnCo_Avg,timestr,60,5,'mean')   # LUPW, outgoing LW radiation, [W m-2]

PREShourly <- TSaggregate(datRaw$BP_mbar,timestr,60,5,'mean')   # PRES, air pressure, [mbar]

# Wind
datRaw$WS_ms_Avg[datRaw$WS_ms_Avg<0] <- 0
datRaw$WS_ms_Avg[datRaw$WS_ms_Avg>50] <- NA
WSPDhourly <- TSaggregate(datRaw$WS_ms_Avg,timestr,60,5,'mean')   # WSPD, wind speed, [m s-1]
WSPDmaxhourly <- TSaggregate(datRaw$WS_ms_Avg,timestr,60,5,'max')   # WSPDmax, max wind speed, [m s-1]

WINDDIRhourly <- winddirmean(datRaw$WS_ms_Avg,datRaw$WindDir_D1_WVT, timestr,60,5) # WINDDIR, hourly wind direction, [deg]

# SR50 Data
datRaw_SR50 <- read.table(path_kya&'\\'&raw60minFile,header = T, sep = ",", dec = ".")

#Quality check (Q needs to be between 152 and 200)
datRaw_SR50$SR50_HAS_cor.1.[which(datRaw_SR50$SR50_Q.1.<152|datRaw_SR50$SR50_Q.1.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.1.[which(datRaw_SR50$SR50_HAS_cor.1.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.2.[which(datRaw_SR50$SR50_Q.2.<152|datRaw_SR50$SR50_Q.2.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.2.[which(datRaw_SR50$SR50_HAS_cor.2.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.3.[which(datRaw_SR50$SR50_Q.3.<152|datRaw_SR50$SR50_Q.3.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.3.[which(datRaw_SR50$SR50_HAS_cor.3.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.4.[which(datRaw_SR50$SR50_Q.4.<152|datRaw_SR50$SR50_Q.4.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.4.[which(datRaw_SR50$SR50_HAS_cor.4.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.5.[which(datRaw_SR50$SR50_Q.5.<152|datRaw_SR50$SR50_Q.5.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.5.[which(datRaw_SR50$SR50_HAS_cor.5.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.6.[which(datRaw_SR50$SR50_Q.6.<152|datRaw_SR50$SR50_Q.6.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.6.[which(datRaw_SR50$SR50_HAS_cor.6.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.7.[which(datRaw_SR50$SR50_Q.7.<152|datRaw_SR50$SR50_Q.7.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.7.[which(datRaw_SR50$SR50_HAS_cor.7.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.8.[which(datRaw_SR50$SR50_Q.8.<152|datRaw_SR50$SR50_Q.8.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.8.[which(datRaw_SR50$SR50_HAS_cor.8.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.9.[which(datRaw_SR50$SR50_Q.9.<152|datRaw_SR50$SR50_Q.9.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.9.[which(datRaw_SR50$SR50_HAS_cor.9.==0)] <- NA
datRaw_SR50$SR50_HAS_cor.10.[which(datRaw_SR50$SR50_Q.10.<152|datRaw_SR50$SR50_Q.10.>210)] <- NA
datRaw_SR50$SR50_HAS_cor.10.[which(datRaw_SR50$SR50_HAS_cor.10.==0)] <- NA

allSR50 <- cbind(datRaw_SR50$SR50_HAS_cor.1.,datRaw_SR50$SR50_HAS_cor.2.,datRaw_SR50$SR50_HAS_cor.3.,datRaw_SR50$SR50_HAS_cor.4.,
      datRaw_SR50$SR50_HAS_cor.5.,datRaw_SR50$SR50_HAS_cor.6.,datRaw_SR50$SR50_HAS_cor.7.,datRaw_SR50$SR50_HAS_cor.8.,
      datRaw_SR50$SR50_HAS_cor.9.,datRaw_SR50$SR50_HAS_cor.10.)

SR50_Final <- rowMeans(allSR50,na.rm=T)
SR50_Final[is.nan(SR50_Final)]<-NA
timestrSR50 <- as.POSIXct(datRaw_SR50$TIMESTAMP, format="%m/%d/%Y  %H:%M")

SR50hourly <- TSaggregate(SR50_Final,timestrSR50,60,5,'mean')

matchAWSdata <- match(TAIRhourly[,1],SR50hourly[,1])


# Combine all Data
#TimeHourly <- as.POSIXct(TAIRhourly[,1],origin='1970-01-01')
Date <- as.Date(as.POSIXct(TAIRhourly[,1],origin='1970-01-01'))
Time <- strftime(as.POSIXct(TAIRhourly[,1],origin='1970-01-01'),format="%H:%M:%S")

expData <- data.frame(matrix(ncol = 19, nrow = length(Date)))
expData$DATE <- Date
expData$TIME <- Time
expData$BVOL <- BVOLhourly[,2]
expData$PVOL <- PVOLhourly[,2]
expData$BCON <- BCONhourly[,2]
expData$TAIR <- TAIRhourly[,2]
expData$RH <- RHhourly[,2]
expData$RHCOR <- RHhourly[,2]
expData$TCNR4 <- TCNR4hourly[,2]


write.csv(expData,file=path_kya&'//final.csv')