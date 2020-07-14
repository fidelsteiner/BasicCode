################################################################################
# TSaggregate - Aggregate time series data to different time steps
# 
# TSaggregate.R
#
# ReadMe: 
# Code is made for climate/hydro data that needs to be aggregated to coarse time steps.
# Aggregation to half-hourly, hourly and daily aggregation is currently available.
# Data needs to be provided in the following format
# TS - time series to be aggregated, vector()
# TimStr - time string in any time string format that includes year, month, day, hour and minute data, vector()
# timStep - single value, see below
# timShift - default, set to 0; only needed for the case that sub-hourly measurements within an hour need to be aggregated to another hour 
#           than the one specified in the time string. e.g. if data at 0:10, 0:20, 0:30 etc is aggregated to 1:00 rather than 0:00, the timeShift would be 5
#
# timStep:  30 - half hourly aggregation
#           60 - hourly aggregation
#           24 - daily aggregation
#
# aggrmode: mean, median, sum, max, min
#
# Created:          2017/02/05
# Latest Revision:  2020/11/17
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht 
# Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 
################################################################################

TSaggregate <- function(TS,TimStr,timStep,timShift,aggrmode){
  
  library(lubridate)
  library(data.table)
########### 
  # Aggregate to 30 min timesteps
  if(timStep == 30){
    minVal <- minute(TimStr)
    minVal[minVal<=30] <- 0
    minVal[minVal>=30] <- 30

    # Find unique lines
    timBind<-(cbind(year(TimStr),month(TimStr),lubridate::day(TimStr),hour(TimStr),minVal))
    timBinddf<-data.frame(timBind)
    df2<-unique(timBinddf)
    df2$ID <- 1:nrow(df2)
    timBinddf_unique<-merge(timBinddf,df2)
    DT <- data.table(timBinddf, key="V1,V2,V3,V4,minVal")
    DT[, Cluster_ID:=.GRP, by=key(DT)]
    DT<-as.data.frame(DT)

    # Aggregation of data
    timVec <- ISOdatetime(df2[,1],df2[,2],df2[,3],df2[,4],df2[,5],rep(0,dim(df2)[1]))
    valVec <- aggregate(TS,list(DT[,6]),aggrmode,na.rm=T)
    valVec_sd <- aggregate(TS,list(DT[,6]),sd,na.rm=T)
    valVec$x[is.nan(valVec$x)]<-NA
    TSaggregate <- cbind(timVec ,valVec[,2],valVec_sd[,2])
    
########### 
  # Aggregate to 60 min timesteps
  } 
###########  
  # Aggregate hourly timesteps
  else if(timStep == 60){
    minVal <- minute(TimStr)*0

    if(timShift>0){       # Version where hourly aggregates are different than simply aggregating to the full hour as given in the timestring
  # shift the time vector
#browser()
  TimStr <- TimStr + 1*timShift*10*60
  hourdata <- hour(TimStr)
  #hourdata <- hourdata[-(1:timShift)]

  daydata <- lubridate::day(TimStr)
  #daydata <- daydata[-(1:timShift)]
  
  yeardata <- year(TimStr)
  #yeardata <- yeardata[1:(length(yeardata)-timShift)]
  
  monthdata <- month(TimStr)
  #monthdata <- monthdata[1:(length(monthdata)-timShift)]
  
  #minVal <- minVal[1:(length(minVal)-timShift)]

  # Find unique lines
  timBind<-(cbind(yeardata,monthdata,daydata,hourdata,minVal))
  timBinddf<-data.frame(timBind)
  df2<-unique(timBinddf)
  df2$ID <- 1:nrow(df2)
  timBinddf_unique<-merge(timBinddf,df2)
  DT <- data.table(timBinddf, key="yeardata,monthdata,daydata,hourdata,minVal")
  DT[, Cluster_ID:=.GRP, by=key(DT)]
  DT<-as.data.frame(DT)

  # Aggregation of data
  timVec <- ISOdatetime(df2[,1],df2[,2],df2[,3],df2[,4],df2[,5],rep(0,dim(df2)[1]))
  valVec <- aggregate(TS,list(DT[,6]),aggrmode,na.rm=T)
  valVec_sd <- aggregate(TS,list(DT[,6]),sd,na.rm=T)
  valVec$x[is.nan(valVec$x)]<-NA
  TSaggregate <- cbind(timVec ,valVec[,2],valVec_sd[,2])  
  
} 
    else if(timShift==0){
    # Find unique lines
    timBind<-(cbind(year(TimStr),month(TimStr),lubridate::day(TimStr),hour(TimStr),minVal))
    timBinddf<-data.frame(timBind)
    df2<-unique(timBinddf)
    df2$ID <- 1:nrow(df2)
    timBinddf_unique<-merge(timBinddf,df2)
    DT <- data.table(timBinddf, key="V1,V2,V3,V4,minVal")
    DT[, Cluster_ID:=.GRP, by=key(DT)]
    DT<-as.data.frame(DT)
    
    # Aggregation of data
    timVec <- ISOdatetime(df2[,1],df2[,2],df2[,3],df2[,4],df2[,5],rep(0,dim(df2)[1]))
    valVec <- aggregate(TS,list(DT[,6]),aggrmode,na.rm=T)
    valVec_sd <- aggregate(TS,list(DT[,6]),sd,na.rm=T)
    valVec$x[is.nan(valVec$x)]<-NA
    TSaggregate <- cbind(timVec ,valVec[,2],valVec_sd[,2])
}
  }
###########     
  # Aggregate to 24 h timesteps
  else if(timStep == 24){
    minVal <- minute(TimStr)*0
    hourVal <- hour(TimStr)*0
    
    # Find unique lines
    timBind<-(cbind(year(TimStr),month(TimStr),lubridate::day(TimStr),hourVal,minVal))
    timBinddf<-data.frame(timBind)
    df2<-unique(timBinddf)
    df2$ID <- 1:nrow(df2)
    timBinddf_unique<-merge(timBinddf,df2)
    DT <- data.table(timBinddf, key="V1,V2,V3,hourVal,minVal")
    DT[, Cluster_ID:=.GRP, by=key(DT)]
    DT<-as.data.frame(DT)
    
    # Aggregation of data
    timVec <- ISOdatetime(df2[,1],df2[,2],df2[,3],df2[,4],df2[,5],rep(0,dim(df2)[1]))
    valVec <- aggregate(TS,list(DT[,6]),aggrmode,na.rm=T)
    valVec_sd <- aggregate(TS,list(DT[,6]),sd,na.rm=T)
    valVec$x[is.nan(valVec$x)]<-NA
    TSaggregate <- cbind(timVec ,valVec[,2],valVec_sd[,2])  
  }
###########

}