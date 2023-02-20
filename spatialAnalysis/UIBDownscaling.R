################################################################################
# Downscaling of climate data for the Upper Indus Basin
#
# UIBDownscaling.R
#
# ReadMe: 
#
# KrigR
#
# Input:
# 
#
# Created:          2023/02/07
# Latest Revision:  2023/02/07
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
#library(rgdal)
#library(rgeos)
#library(maptools)
#library(raster)
#library(ggplot2)
#library(dplyr)
#library(ggridges)
#library(RColorBrewer)
#library(hydroGOF)

library(devtools)
#Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
#devtools::install_github("https://github.com/ErikKusch/KrigR")

library(KrigR)
library(KrigR)