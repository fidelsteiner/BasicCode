################################################################################
# Read results from cosicorr and convert to velocity raster
# 
# cosiCorrReader.R
#
# ReadMe:
# 
# Created:          2018/01/12
# Latest Revision:  2018/11/29
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
projec<-'+proj=utm +zone=43N +datum=WGS84'
path_data <- 'F://PhD//Research//SurgingGlaciers//Shishper//PlanetData'
# Read Velocities for BuildUp Phase
velB_NS<-raster(path_data&'//1021_1202_VELTIFF.tif',band=2)
velB_EW<-raster(path_data&'//1021_1202_VELTIFF.tif',band=1)  
velB_SNR<-raster(path_data&'//1021_1202_VELTIFF.tif',band=3)
projection(velB_NS)<-projec
projection(velB_EW)<-projec
projection(velB_SNR)<-projec
velclipB_NS<-mask(velB_NS,glacier_p)
velclipB_EW<-mask(velB_EW,glacier_p)  
velclipB_SNR<-mask(velB_SNR,glacier_p) 

vel_bu <- sqrt(velB_EW^2+velB_NS^2) / 42 * 365;
vel_bu[velB_SNR<0.75]<-NA
writeRaster(vel_bu, path_data&'/BuildcentVel_December.tif', 'GTiff',overwrite=TRUE)