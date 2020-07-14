################################################################################
# multiObjmin - multiobjective minimizer of error
# 
# mOm.R
#
# ReadMe:
# Multiobjective minimization of error for time series
# Calculates the following statistics and determines a weighted fit
# R2, NSE, RMSE, MBE
#
# Created:          2020/02/05
# Latest Revision:  2020/02/05
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht 
# Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 
################################################################################

mOm <- function(qObs,qCalc){

of1 <- sum((log(qCalc) - log(qObs))^2, na.rm=T) / sum((log(qCalc) - log(mean(qCalc,na.rm=T)))^2,na.rm=T); 

of2 <- sum((qCalc - qObs)^2, na.rm=T) / sum((qCalc - mean(qObs, na.rm=T))^2, na.rm=T);   # 1 minus R2

of3 <- 1 - sum((qObs - qCalc)^2,na.rm=T) / sum((qObs - mean(qObs,na.rm=T))^2,na.rm=T);     # NSE

of4 <- sqrt(1/length(qCalc)*sum((qCalc-qObs)^2,na.rm=T)); # RMSE

of5 <- 1/length(qCalc)*sum(qCalc-qObs,na.rm=T);  # MBE (mean biased error)

#o.f7 = abs(sqrt(1/(length(qCalc)-1)*nansum((qCalc-nanmean(qCalc)).^2)) - sqrt(1/(length(qObs)-1)*nansum((qObs-nanmean(qObs)).^2)));    % compare standard deviations

#coc = corrcoef(qCalc,qObs,'rows','complete');
#o.f8 = 1 - coc(1,2);

f <- (of2 + of3 + of4 + of5) / 4;      # for fit with Adjustment Factor

return(f)
}