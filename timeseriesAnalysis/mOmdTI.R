################################################################################
# multiObjmin - multiobjective minimizer of error for index melt model under debris
# 
# mOmdTI.R
#
# ReadMe:
# Multiobjective minimization of error for time series
# Calculates the following statistics and determines a weighted fit
# R2, NSE, RMSE, MBE
# based on mOm.R
#
# Created:          2020/02/05
# Latest Revision:  2020/02/05
#
# Jakob F Steiner| PhD candidate | Faculty of Geosciences | Universiteit Utrecht | Princetonlaan 8a, 3584 CB Utrecht 
# Vening Meinesz building, room 4.30 | P.O. Box 80.115, 3508 TC Utrecht | j.f.steiner@uu.nl | www.uu.nl/staff/jfsteiner | www.mountainhydrology.org 
################################################################################

mOmdTI <- function(qObs,Temp,SW,debDep,lag_T,lag_S,modtype){

  switch(modtype,
         'dTI' = {
           qObs2 <- qObs[(round(debDep*lag_T)):length(qObs)]
           Temp <- TAir_all[1:(length(qObs)-round(debDep*lag_T)+1)]

           qoptim <- function(x,Temp,SW,qObs2){ 
             qCalc <- x[1] * debDep^x[2] * Temp
               of1 <- sum((log(qCalc) - log(qObs2))^2, na.rm=T) / sum((log(qCalc) - log(mean(qCalc,na.rm=T)))^2,na.rm=T); 
               
               of2 <- sum((qCalc - qObs2)^2, na.rm=T) / sum((qCalc - mean(qObs2, na.rm=T))^2, na.rm=T);   # 1 minus R2
               
               of3 <- sum((qObs2 - qCalc)^2,na.rm=T) / sum((qObs2 - mean(qObs2,na.rm=T))^2,na.rm=T);     # 1 minus NSE
               
               of4 <- sqrt(1/length(qCalc)*sum((qCalc-qObs2)^2,na.rm=T)); # RMSE
               
               of5 <- 1/length(qCalc)*sum(qCalc-qObs2,na.rm=T);  # MBE (mean biased error)
               
               
               f <- (of3 + of4) / 2;      # for fit with Adjustment Factor

               return(f)
               }

           out <- optim(par = c(0.3,-0.8),  # initial guess
             fn = qoptim,
             Temp = Temp,
             qObs2 = qObs2)
           
           return(out)
            #browser()
         },
         'dETI' = {
           qObs2 <- qObs[(round(debDep*lag_T)):length(qObs)]
           Temp <- TAir_all[1:(length(qObs)-round(debDep*lag_T)+1)]
           SW <- SW_all[1:(length(qObs)-round(debDep*lag_S)+1)]
           
           qoptim <- function(x,Temp,SW,qObs2){ 
             qCalc  <- x[1] * debDep^x[2] * Temp + x[3] * exp(debDep*x[4]) * SW*(1-0.13)
             of1 <- sum((log(qCalc) - log(qObs2))^2, na.rm=T) / sum((log(qCalc) - log(mean(qCalc,na.rm=T)))^2,na.rm=T); 
             
             of2 <- sum((qCalc - qObs2)^2, na.rm=T) / sum((qCalc - mean(qObs2, na.rm=T))^2, na.rm=T);   # 1 minus R2
             
             of3 <- sum((qObs2 - qCalc)^2,na.rm=T) / sum((qObs2 - mean(qObs2,na.rm=T))^2,na.rm=T);     # 1 minus NSE
             
             of4 <- sqrt(1/length(qCalc)*sum((qCalc-qObs2)^2,na.rm=T)); # RMSE
             
             of5 <- 1/length(qCalc)*sum(qCalc-qObs2,na.rm=T);  # MBE (mean biased error)
             
             f <- (of3 + of4 + of5) / 3;      # for fit with Adjustment Factor

             return(f)
           }
           
           out <- optim(par = c(0.3,-0.8,0.008,-10),  # initial guess
                        fn = qoptim,
                        Temp = Temp,
                        qObs2 = qObs2,
                        SW = SW)
           
           return(out)
           #browser()
         },
         'dETI2' = {
           qObs2 <- qObs[(round(debDep*lag_T)):length(qObs)]
           Temp <- TAir_all[1:(length(qObs)-round(debDep*lag_T)+1)]
           SW <- SW_all[1:(length(qObs)-round(debDep*lag_S)+1)]
           
           qoptim <- function(x,Temp,SW,qObs2){ 
             qCalc  <- x[1] * debDep^x[2] * Temp + x[3] * debDep^x[4] * SW*(1-0.13)
             of1 <- sum((log(qCalc) - log(qObs2))^2, na.rm=T) / sum((log(qCalc) - log(mean(qCalc,na.rm=T)))^2,na.rm=T); 
             
             of2 <- sum((qCalc - qObs2)^2, na.rm=T) / sum((qCalc - mean(qObs2, na.rm=T))^2, na.rm=T);   # 1 minus R2
             
             of3 <- sum((qObs2 - qCalc)^2,na.rm=T) / sum((qObs2 - mean(qObs2,na.rm=T))^2,na.rm=T);     # 1 minus NSE
             
             of4 <- sqrt(1/length(qCalc)*sum((qCalc-qObs2)^2,na.rm=T)); # RMSE
             
             of5 <- 1/length(qCalc)*sum(qCalc-qObs2,na.rm=T);  # MBE (mean biased error)
             
             f <- (of3 + of4 + of5) / 3;      # for fit with Adjustment Factor
             
             return(f)
           }
           
           out <- optim(par = c(0.3,-0.8,0.008,-10),  # initial guess
                        fn = qoptim,
                        Temp = Temp,
                        qObs2 = qObs2,
                        SW = SW)
           
           return(out)
           #browser()
         })

}