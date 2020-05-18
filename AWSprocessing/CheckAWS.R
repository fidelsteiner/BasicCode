################################################################################
# Check AWS Quality for Mountainhydrology Stations
# 
# CheckAWS.R
#
# ReadMe: 
#
# Consider the corresponding code AWS_Langtang_process.R for original processing
#
# Author: walter immerzeel
# Created:          2020/02/05
# Latest Revision:  2020/02/05
#
# mountainhydrology group UU |  www.mountainhydrology.org 
################################################################################

# Clear plots
if(!is.null(dev.list())) dev.off()
 
# Clean workspace
rm(list=ls())


## Set paths
base = "F://Dropbox//FW_WI_WI//CheckAWS//MicroMet//"    # Path for datafile to be checked
stat = "20200117_MM_6350"                               # datafile name (always has to be a .csv file)

# specify meteofile
meteof = paste(base, stat ,".csv",sep="")

## Read CSV with meteodata
meteo <- read.csv(file=meteof, header=TRUE, sep=",",stringsAsFactors=F)
meteo$datestamp <- as.POSIXct(meteo$DATE, format='%Y-%m-%d',tz = "Asia/Kathmandu",logical=TRUE)
meteo$checkdate <- is.na(meteo$datestamp)
date_check <- unique(subset(meteo,is.na(meteo$datestamp),select = c(DATE)))
date_check

# Output data with wrong date column
outf = paste(base, stat ,"_wrong_dates.csv",sep="")
write.csv(date_check,file = outf)

# Output plots with data visualization for all variables
vars = colnames(meteo)[3:(length(meteo)-2)]
dev.off() 
pdff = paste(base, stat ,"_plots.pdf",sep="")
pdf(file = pdff)  
par(mfrow = c(2,2))
for (var in vars)   
{ 
  print(var)
  plot(meteo$date,meteo[,var], main=var,xlab="date",ylab=var)
  
} 
dev.off() 