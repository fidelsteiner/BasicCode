################################################################################
# Model incoming longwave radiation only based on RH and T measurements from an AWS
# 
# LWMod_KokSteiner.R
#
# ReadMe: Derive incoming longwave radiation for based on  de Kok et al. (2019)  https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.6249
# The model has been developed for hourly data. Coarser time steps are likely also possible.
#
# Output: LWMod           <- modelled incoming longwave radiation [W m-2]
# Input:  Ta_data         <- Air Temperature (2m measurement height, [degC])
#         RH_data         <- relative humidity data (2m, %)
#         SWIN_data       <- measured incoming shortwave radiation (2m, [W m-2])
#

# Created:          2019/09/02
# Latest Revision:  2019/09/02
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht | Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 

LWMod_KokSteiner <- function(Ta_data,RH_data,SWIN_data) { 

  sigma <- 5.67*10^-8
    dry_cond_new <- which(RH_data<=80&SWIN_data<50|RH_data<=60&SWIN_data>=50)     # dry branch of data
    wet_cond_new <- which(RH_data>80&SWIN_data<50|RH_data>60&SWIN_data>=50)                 # wet branch of data
    
    LWMod <- Ta_data * NA
    LWMod[dry_cond_new] <- -75.2802438 + 0.82201551 * RH_data[dry_cond_new] + 0.7927014 * sigma * (Ta_data[dry_cond_new]+273.15)^4
    LWMod[wet_cond_new] <- -212.59405573 + 1.88964724 * RH_data[wet_cond_new] + 1.05610277 * sigma * (Ta_data[wet_cond_new]+273.15)^4

    LWMod_HMA <- return(LWMod)
  
}
