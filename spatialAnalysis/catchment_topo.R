################################################################################
# catchment_topo - Visualize surface properties per elevation bin in a catchment and subcatchments
# 
# catchment_topo.R
#
# ReadMe: Written for the Langtang catchment, applicable elsewhere
# 
# 
#
# Created:          2019/08/15
# Latest Revision:  2021/05/05
#
# Jakob F Steiner| ICIMOD | jakob.steiner@icimod.org | x-hydrolab.org 
################################################################################
# clear entire workspace (excl. packages)
rm(list = ls())
gc()

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

# install necessary packages if not available yet via install.packages()
library(pacman)
p_load(rgdal,rgeos,maptools,raster,rasterVis)
library(RColorBrewer)

##########################
# SPECIFY FILENAMES AND DESIRED PROJECTION
##########################
# Define Projection
projec_lambers <- "+proj=laea +lat_0=28 +lon_0=95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
projec_utm <- '+proj=utm +zone=45N +datum=WGS84'

projec <- projec_utm

# PATHS  (adapt and make sure all data is available)
path_DEM <- 'D:\\Work\\GeospatialData\\Langtang\\DEMs'                                # Folder for all DEMs
#path_output <- 'F:\\PhD\\Research\\SPHY\\SPHYLangtang\\figures'                       # Folder for all figures
path_ccidata <- 'D:\\Work\\GeospatialData\\Langtang\\landuse\\cci-landcover-2015'          # Folder for raw landcover data
filename <- 'ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif'                          # CCI file name
path_RGI <- 'D:\\Work\\GeospatialData\\RGI60'                                     # Folder for RGI glacier outlines
RGI_filename <- '15_rgi60_SouthAsiaEast.shp'                                          # RGI filename
path_debris <- 'D:\\Work\\GeospatialData\\DCG_Scherler\\S2_2015-2017_NDSI\\S2_2015-2017_NDSI'  # folder debris data (Scherler 2018)
#path_thick <- 'F:\\PhD\\GeoSpatialData\\IceThickness_Farinotti\\RGI60-15\\RGI60-15'           # folder with ice thickness data (Farinotti 2019)
RGI_debris_filename <- '15_rgi60_SouthAsiaEast_S2_DC_2015_2017_NDSI.shp'
snow_filename <- 'snow_persistence_final.tif'

nepal_glac_1980 <- 'D:\\Work\\GeospatialData\\ICIMOD_Glaciercover\\Glaciers of Nepal 1980\\data\\Glacier_1980.shp'
nepal_glac_1990 <- 'D:\\Work\\GeospatialData\\ICIMOD_Glaciercover\\Glaciers of Nepal 1990\\data\\Glacier_1990.shp'
nepal_glac_2000 <- 'D:\\Work\\GeospatialData\\ICIMOD_Glaciercover\\Glaciers of Nepal 2000\\data\\Glacier_2000.shp'
nepal_glac_2010 <- 'D:\\Work\\GeospatialData\\ICIMOD_Glaciercover\\Glaciers of Nepal 2010\\data\\Glacier_2010.shp'

path_watershed <- 'D:\\Work\\GeospatialData\\Langtang\\WaterSheds'        # Location for all watersheds drainage points
path_subcatchments <- 'D:\\Work\\Research\\Manuscripts\\Hydrology\\Langtang\\CatchmentMapping\\OutlineLangtang'        # Outlines for all watersheds
path_topodata <- 'D:\\Work\\Research\\Manuscripts\\Hydrology\\Langtang\\CatchmentMapping\\Catchment_Topodata'

outlets <- c('Kyanjing_Meas','Langshisha_Meas','Lirung_Meas')
subcatchments <- c('Kyanjing_Subcatchment','Langshisha_Subcatchment','LirungSubcatchment')
#watersheds <- c('ws_syafru.tif','ws_kyan.tif','ws_las.tif','ws_lir.tif','ws_lam.tif','ws_vil.tif','ws_lan.tif','ws_shal.tif','ws_num.tif','ws_yala.tif','ws_kimoshun.tif','ws_gan1.tif','ws_gan2.tif')
#outletType <- c(1,2,2,2,3,3,4,4,5,6,6,6,6) # 1 - main outlet, 2 - measurements, 3 - ungauged catchments, 4 - ungauged subcatchments with debris covered glacier 5 - unguaged subcatchment with nearly no glaciers, 6 - unguaged catchment with clean ice cover

DEM_name <- 'LangtangDEM_cropped.tif'

# if available, provide .shp for catchment outline for visualisation, set 'cSHP' to 1
#cSHP <- 1
#file_catchmentoutline <- 'F:\\PhD\\Research\\SPHY\\CatchmentMapping\\OutlineLangtang\\catch_proj_1.shp' # Catchment Outline (shp file)

# if available, provide raster file with data where no soil available, set 'bare' to 1
bare <- 1
file_barerock <- 'D:\\Work\\GeospatialData\\Langtang\\barerock_langtang30m.tif'

##########################
# Load Catchment DOMAIN
##########################
dem_domain <- raster(paste(path_DEM,'\\',DEM_name,sep=''))   # Load the domain DEM
dem_domain <- projectRaster(dem_domain,crs = projec)


contourDEM <- rasterToContour(dem_domain,levels = pretty(range(dem_domain[], na.rm = TRUE), 10))
#writeOGR(contourDEM, dsn = path_watershed&'\\contoursDomain.shp', layer='contourDEM',driver = "ESRI Shapefile")

dem_deg <- projectRaster(dem_domain,crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

#dem_lat <- domain
#dem_lat[seq(1,dim(domain)[1]*dim(domain)[2],1)] <- coordinates(domain_deg)[seq(1,dim(domain)[1]*dim(domain)[2],1),2]

#modID_raster <- domain
#modID_raster[seq(1,dim(domain)[1]*dim(domain)[2],1)] <- seq(1,dim(domain)[1]*dim(domain)[2],1)

##########################
# Load RGI and thickness data
##########################
ogrInfo(path_RGI&'\\'&RGI_filename)
RGI60_15<-readOGR(dsn=path_RGI&'\\'&RGI_filename)
ogrInfo(path_debris&'\\'&RGI_debris_filename)
RGI60_15_debris<-readOGR(dsn=path_debris&'\\'&RGI_debris_filename)
projection(RGI60_15_debris) <- projection(RGI60_15)

RGI60_15 <- spTransform(RGI60_15, projec)
RGI60_15_debris <- spTransform(RGI60_15_debris, projec)

# Restrict the datasets to the domain
sub_15 <- subset(RGI60_15, CenLon >= extent(dem_deg)[1] & CenLon <= extent(dem_deg)[2] & CenLat >= extent(dem_deg)[3] & CenLat <= extent(dem_deg)[4])
sub_15_debris <- subset(RGI60_15_debris, CenLon >= extent(dem_deg)[1] & CenLon <= extent(dem_deg)[2] & CenLat >= extent(dem_deg)[3] & CenLat <= extent(dem_deg)[4])

if(bare ==1){
  bareRaster <- raster(file_barerock)
  projection(bareRaster) <- projec
  bareRaster <- raster::resample(bareRaster,dem_domain)
  bareRaster <- mask(bareRaster,dem_domain)
}

##########################
# Load ICIMOD Glacier data
##########################
ogrInfo(nepal_glac_1980)
nglac1980 <-readOGR(dsn=nepal_glac_1980)
nglac1980 <- subset(nglac1980, Longitude >= extent(dem_deg)[1] & Longitude <= extent(dem_deg)[2] & Latitude >= extent(dem_deg)[3] & Latitude <= extent(dem_deg)[4])
nglac1980 <- spTransform(nglac1980, projec)

ogrInfo(nepal_glac_1990)
nglac1990 <-readOGR(dsn=nepal_glac_1990)
nglac1990 <- subset(nglac1990, Longitude >= extent(dem_deg)[1] & Longitude <= extent(dem_deg)[2] & Latitude >= extent(dem_deg)[3] & Latitude <= extent(dem_deg)[4])
nglac1990 <- spTransform(nglac1990, projec)

ogrInfo(nepal_glac_2000)
nglac2000 <-readOGR(dsn=nepal_glac_2000)
nglac2000 <- subset(nglac2000, Longitude >= extent(dem_deg)[1] & Longitude <= extent(dem_deg)[2] & Latitude >= extent(dem_deg)[3] & Latitude <= extent(dem_deg)[4])
nglac2000 <- spTransform(nglac2000, projec)

ogrInfo(nepal_glac_2010)
nglac2010 <-readOGR(dsn=nepal_glac_2010)
nglac2010 <- subset(nglac2010, Longitude >= extent(dem_deg)[1] & Longitude <= extent(dem_deg)[2] & Latitude >= extent(dem_deg)[3] & Latitude <= extent(dem_deg)[4])
nglac2010 <- spTransform(nglac2010, projec)

##########################
# Load subcatchments for evaluation
##########################
allOutlets <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]
for(subc in 1:length(outlets)){
  ogrInfo(path_watershed&'\\'&outlets[subc]&'.shp')
  out1 <- readOGR(dsn=path_watershed&'\\'&outlets[subc]&'.shp')
  out1 <- spTransform(out1, projec)
  out1$Id <- subc
  allOutlets <- allOutlets + out1
}

outletRaster <- rasterize(allOutlets,dem_domain,'Id') # Combine all outlet locations
outletRaster[is.na(outletRaster[])] <- 0

allSubcatchments <-  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame())
crs(allSubcatchments) <- projec
for(subc in 1:length(outlets)){
  ogrInfo(path_subcatchments&'\\'&subcatchments[subc]&'.shp')
  out1 <- readOGR(dsn=path_subcatchments&'\\'&subcatchments[subc]&'.shp')
  out1 <- spTransform(out1, projec)
  out1$Id <- subc
  allSubcatchments <- rbind(allSubcatchments,out1)
}

outletRaster <- rasterize(allOutlets,dem_domain,'Id') # Combine all outlet locations
outletRaster[is.na(outletRaster[])] <- 0

#writeRaster(outletRaster, file.path(path_maps, "outlet.tif"), format="GTiff",overwrite=T)

##########################
# Get Properties of subcatchments
##########################

r_glaciermask_cover <- rasterize(sub_15, dem_domain,getCover=TRUE) # relative cover of glacier over pixel
r_glaciermask_cover <- mask(r_glaciermask_cover,dem_domain)
r_glaciermask_debris_cover <- rasterize(sub_15_debris, dem_domain,getCover=TRUE) # relative cover of debris over pixel
r_glaciermask_debris_cover <- mask(r_glaciermask_debris_cover,dem_domain)

nglac1980 <- rasterize(nglac1980, dem_domain,getCover=TRUE) # relative cover of debris over pixel
nglac1980 <- mask(nglac1980,dem_domain)
nglac1990 <- rasterize(nglac1990, dem_domain,getCover=TRUE) # relative cover of debris over pixel
nglac1990 <- mask(nglac1990,dem_domain)
nglac2000 <- rasterize(nglac2000, dem_domain,getCover=TRUE) # relative cover of debris over pixel
nglac2000 <- mask(nglac2000,dem_domain)
nglac2010 <- rasterize(nglac2010, dem_domain,getCover=TRUE) # relative cover of debris over pixel
nglac2010 <- mask(nglac2010,dem_domain)

bareRaster[r_glaciermask_cover>0] <- NA

snowRaster <- raster(path_topodata&'\\'&snow_filename)
snowRaster <- crop(snowRaster,dem_domain)

StatsMatrix <- matrix(NA, nrow = length(outlets), ncol = 12)
for(subc in 1:length(outlets)){
  ogrInfo(path_subcatchments&'\\'&subcatchments[subc]&'.shp')
  subOutline <- readOGR(dsn=path_subcatchments&'\\'&subcatchments[subc]&'.shp')
  subOutline <- spTransform(subOutline, projec)

  subDem <- mask(dem_domain,subOutline)

  #elev_sub_his <- hist(elev_sub,breaks=seq(1000,7000,by=500))

  #outline_sub <- buffer(rasterToPolygons(elev_sub, dissolve=TRUE), width = 1, dissolve = T)
  #outline_sub <- as(outline_sub, "SpatialPolygonsDataFrame")

  glac_sub <- mask(r_glaciermask_cover,subDem)
  glac_sub[glac_sub>0] <- 1
  glac_sub[glac_sub<1] <- NA
  
  n1980glac_sub <- mask(nglac1980,subDem)
  n1980glac_sub[n1980glac_sub>0] <- 1
  n1980glac_sub[n1980glac_sub<1] <- NA
  n1990glac_sub <- mask(nglac1990,subDem)
  n1990glac_sub[n1990glac_sub>0] <- 1
  n1990glac_sub[n1990glac_sub<1] <- NA
  n2000glac_sub <- mask(nglac2000,subDem)
  n2000glac_sub[n2000glac_sub>0] <- 1
  n2000glac_sub[n2000glac_sub<1] <- NA
  n2010glac_sub <- mask(nglac2010,subDem)
  n2010glac_sub[n2010glac_sub>0] <- 1
  n2010glac_sub[n2010glac_sub<1] <- NA

  deb_sub <- mask(r_glaciermask_debris_cover,subDem)
  deb_sub[deb_sub>0] <- 1
  deb_sub[deb_sub<1] <- NA

  bare_sub <- mask(bareRaster,subDem)
  bare_sub[bare_sub>0] <- 1
  bare_sub[bare_sub<1] <- NA
  
  snow_sub <- mask(snowRaster,subOutline)
  snow_sub <- crop(snow_sub,glac_sub,snap="near")
  
  slope_sub <- terrain(subDem, opt="slope",unit='degrees')

  StatsMatrix[subc,1] <- area(subOutline) / 10^6
  StatsMatrix[subc,2] <- sum(glac_sub[],na.rm=T) *res(glac_sub)[1]*res(glac_sub)[2]/ 10^6 
  StatsMatrix[subc,3] <- sum(deb_sub[],na.rm=T) *res(deb_sub)[1]*res(deb_sub)[2]/ 10^6 
  StatsMatrix[subc,4] <- sum(bare_sub[],na.rm=T) *res(bare_sub)[1]*res(bare_sub)[2]/ 10^6
  StatsMatrix[subc,5] <- length(which(snow_sub[]>=95))/length(which(snow_sub[]>=0))
  StatsMatrix[subc,6] <- cellStats(slope_sub,'mean')
  StatsMatrix[subc,7] <- cellStats(subDem,'min')
  StatsMatrix[subc,8] <- cellStats(subDem,'max')
  
  StatsMatrix[subc,9] <- sum(n1980glac_sub[],na.rm=T) *res(glac_sub)[1]*res(glac_sub)[2]/ 10^6 
  StatsMatrix[subc,10] <- sum(n1990glac_sub[],na.rm=T) *res(glac_sub)[1]*res(glac_sub)[2]/ 10^6 
  StatsMatrix[subc,11] <- sum(n2000glac_sub[],na.rm=T) *res(glac_sub)[1]*res(glac_sub)[2]/ 10^6 
  StatsMatrix[subc,12] <- sum(n2010glac_sub[],na.rm=T) *res(glac_sub)[1]*res(glac_sub)[2]/ 10^6 
}

png(file='D:\\Work\\Research\\CatchmentHydrology\\Langtang\\Figures\\GlacierAreaLossLangtang.png', res = 160,width=3600,height=1200)

oldpar<-par(mar=c(3.5,3.75,1.5,0.5), mgp=c(2.35,1,0),cex=1.5)
plot(seq(1980,2010,10),StatsMatrix[1,9:12]/StatsMatrix[1,9]*100,type='l',ylim=c(50,100),ylab='Area coverage [%]',xlab = 'year')
points(seq(1980,2010,10),StatsMatrix[2,9:12]/StatsMatrix[2,9]*100,type='l',col='red')
points(seq(1980,2010,10),StatsMatrix[3,9:12]/StatsMatrix[3,9]*100,type='l',col='green')
dev.off()