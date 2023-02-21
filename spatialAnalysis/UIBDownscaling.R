################################################################################
# Downscaling of climate data for the Upper Indus Basin
#
# UIBDownscaling.R
#
# ReadMe: 
#
# Downscaling climate data (T, P, snow cover) from ERA5-Land based on HydroBasins.
# Downscaling using the KrigR package
# Requires ECMWF API Key and Username
#
# Input:
# 1) HydroBasins (https://www.hydrosheds.org/products/hydrobasins) for domain
# 
#
# Created:          2023/02/12
# Latest Revision:  2023/02/21
#
# Jakob F Steiner | jakob@x-hydrolab.org | x-hydrolab.org 
################################################################################
# clear entire workspace (excl. packages)
rm(list = ls())
gc()

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

# packages (if not installed yet: install.packages('examplePackage')
library(devtools)
#Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
#devtools::install_github("https://github.com/ErikKusch/KrigR")
library(KrigR)

# load basin shp file
path_outlines <- 'C:\\Work\\GeospatialData\\HydroSheds\\hybas_as_lev01-12_v1c'
fnUIBOutline <- 'hybas_as_lev08_v1c'

RootDir <- 'C:\\Work\\Research\\Collaborations\\HMA\\NeoshaMIT\\'
  
path_output <- paste(RootDir&'Output')
path_rawdata <- paste(RootDir&'BaseData\\ERA5Land')
path_DEMdata <- paste(RootDir&'BaseData\\DEMData')

# Load subcatchments
ogrInfo(path_outlines,fnUIBOutline)
UIB_pEXT<-readOGR(dsn=path_outlines,layer=fnUIBOutline)
UIB_pEXT<-SpatialPolygons(UIB_pEXT@polygons,proj4string=UIB_pEXT@proj4string)
#projection(UIB_pEXT)<-CRS("+init=epsg:4326")

# Read ECMWF API KEY DATA
ECMWF_API <- read.csv('C:\\Work\\Code\\ecmwf_API.csv')

# Load Temperature data
UIB_RAW_T <- download_ERA(
  Variable = '2m_temperature',
  Type = 'reanalysis',
  DataSet = 'era5-land',
  DateStart = '2017-01-01',
  DateStop = '2017-12-31',
  TResolution = 'day',
  TStep = 1,
  Extent = UIB_pEXT[19283],
  Dir = path_rawdata,
  API_User = ECMWF_API$ECMWF_USER,
  API_Key = ECMWF_API$ECMWF_KEY
)

UIB_RAW_P <- download_ERA(
  Variable = 'total_precipitation',
  Type = 'reanalysis',
  DataSet = 'era5-land',
  DateStart = '2017-01-01',
  DateStop = '2017-12-31',
  TResolution = 'day',
  TStep = 1,
  Extent = UIB_pEXT[19283],
  Dir = path_rawdata,
  API_User = ECMWF_API$ECMWF_USER,
  API_Key = ECMWF_API$ECMWF_KEY
)

UIB_RAW_SC <- download_ERA(
  Variable = 'snow_cover',
  Type = 'reanalysis',
  DataSet = 'era5-land',
  DateStart = '2017-01-01',
  DateStop = '2017-01-31',
  TResolution = 'day',
  TStep = 1,
  Extent = UIB_pEXT[19283],
  Dir = path_rawdata,
  API_User = ECMWF_API$ECMWF_USER,
  API_Key = ECMWF_API$ECMWF_KEY
)

Covs_ls <- download_DEM(Train_ras = UIB_RAW_SC,
                        Target_res = 0.01,
                        Shape = UIB_pEXT[19283],
                        Dir = path_DEMdata,
                        Keep_Temporary = TRUE)

Shisper_Krig_T <- krigR(Data = UIB_RAW_T,
                      Covariates_coarse = Covs_ls[[1]],
                      Covariates_fine = Covs_ls[[2]],
                      KrigingEquation = 'ERA ~ DEM',
                      Keep_Temporary = TRUE,
                      Cores = 4,
                      FileName = 'ShisperDownscaled_T.nc',
                      Dir = path_output)

Shisper_Krig_P <- krigR(Data = UIB_RAW_P,
                      Covariates_coarse = Covs_ls[[1]],
                      Covariates_fine = Covs_ls[[2]],
                      KrigingEquation = 'ERA ~ DEM',
                      Keep_Temporary = TRUE,
                      Cores = 4,
                      FileName = 'ShisperDownscaled_P.nc',
                      Dir = path_output)

Shisper_Krig_SC <- krigR(Data = UIB_RAW_SC,
                        Covariates_coarse = Covs_ls[[1]],
                        Covariates_fine = Covs_ls[[2]],
                        KrigingEquation = 'ERA ~ DEM',
                        Keep_Temporary = TRUE,
                        Cores = 4,
                        FileName = 'ShisperDownscaled_SC.nc',
                        Dir = path_output)