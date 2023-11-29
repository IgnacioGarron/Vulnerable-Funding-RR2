library(tidyverse)
library(openxlsx) # Convierte formato de fecha de excel
library(readxl) # excel
library(openxlsx) # Convierte formato de fecha de excel
library(kableExtra) # html tables
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
library(broom)
library(QregBB)
########################################################
# Importa datos de consolidados
rm(list = ls()) # Limpiar environment
data <- read.csv("../Data/Data_final.csv")

# Vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$date) # fechas forma 1



banner("Parte 2:", "Quantile regressions for credit", emph = TRUE)
###########################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###                 REGRESIONES CUANT?LICAS PARA CR?DITO                 ###
###                                                                      ###
############################################################################
############################################################################


# horizon

h_horizon<-c(0,1,4,8,12) # Acá empieza loop para h


LAC_credit<-c("Argentina","Bolivia","Brazil",  "Chile",        "Colombia",       "Costa Rica",
              "Guatemala"  ,    "Honduras",   "Mexico",
              "Peru","Uruguay" )

#LAC_stock<-c("Mexico","Peru","Chile","Peru")

length(LAC_credit)

tabla_raw<-data.frame(matrix(nrow=length(LAC_credit),ncol=6))
tabla_raw[,1]<-LAC_credit
colnames(tabla_raw)<-c("country","h=0","h=1","h=4","h=8","h=12")

# M1: y_t+h=b1*y_t+b2*nfci_t+b3*g_macro+b4*g_fin
M1_credit_factor_coef_b2_2008=tabla_raw
M1_credit_factor_sig_b2_2008=tabla_raw
M1_credit_factor_coef_b2_2019=tabla_raw
M1_credit_factor_sig_b2_2019=tabla_raw


# M2: y_t+h=b1*y_t+b2*fu_t+b3*g_macro+b4*g_fin
M2_credit_factor_coef_b2_2008=tabla_raw
M2_credit_factor_sig_b2_2008=tabla_raw
M2_credit_factor_coef_b2_2019=tabla_raw
M2_credit_factor_sig_b2_2019=tabla_raw

for (h in h_horizon){
  
  data_reg <- data %>% group_by(country) %>%
    mutate(credit=log(credit/lag(credit)),
           stock=log(stock/lag(stock)),
           gdp=log(gdp,lag(gdp))) %>% 
    mutate_each(funs = scale,
                credit_z=credit,
                stock_z=stock,
                gdp_z=gdp,
                inf_z=inf,
                yield_z=yield,
                NFCI_z=NFCI,
                Real_z=VOL_GROWTH,                #fci_z=fci, 
                credit_f_z=credit_f, # credit factor
                f_global_z=VOL_WPI,
                f_fin_z=SV,
                USUN_z=USUN) %>% 
    mutate(stock_h=lead(stock_z,h),
           credit_h=lead(credit_z,h),
           gdp_h=lead(gdp_z,h))
 
    for(temp in c(1,2)){
    
      for (country_name in LAC_credit){ # acá empieza loop para país
          
          if(temp==1){
            data_model<-data_reg %>% 
            group_by(country) %>% 
            filter(complete.cases(credit_h,credit_z,NFCI_z),country==country_name)%>% 
            filter(date<="2008-10-01") 
            Y.train=as.matrix(data_model[,"credit_h"]) 
            
          }else if (temp==2){
            data_model<-data_reg %>% 
            group_by(country) %>% 
            filter(complete.cases(credit_h,credit_z,NFCI_z),country==country_name) 
            Y.train=as.matrix(data_model[,"credit_h"]) 
          }
        
        if (h==0){
          X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z","Real_z","credit_f_z","f_global_z")]))
        } else {
          X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","NFCI_z","Real_z","credit_f_z","f_global_z")]))
        }
    
          tau=c(0.25)
          
          M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)

            if (h==0){ j=2
            } else if(h==1){ j=3
            } else if(h==4){ j=4
            } else if(h==8){ j=5
            } else if(h==12){ j=6}
    
      if (h!=0){ # no hay h=0 para rezago
      
        if(temp==1){
          M1_credit_factor_coef_b2_2008[M1_credit_factor_coef_b2_2008$country==country_name,j]=M1$beta.hat[3]
          M1_credit_factor_sig_b2_2008[M1_credit_factor_sig_b2_2008$country==country_name,j]=
            ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        }else if(temp==2){
          M1_credit_factor_coef_b2_2019[M1_credit_factor_coef_b2_2019$country==country_name,j]=M1$beta.hat[3]
          M1_credit_factor_sig_b2_2019[M1_credit_factor_sig_b2_2019$country==country_name,j]=
            ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        }
    
    } else {
      
      if(temp==1){
        M1_credit_factor_coef_b2_2008[M1_credit_factor_coef_b2_2008$country==country_name,j]=
          M1$beta.hat[2]
        M1_credit_factor_sig_b2_2008[M1_credit_factor_sig_b2_2008$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
      }else if(temp==2){
        M1_credit_factor_coef_b2_2019[M1_credit_factor_coef_b2_2019$country==country_name,j]=
          M1$beta.hat[2]
        M1_credit_factor_sig_b2_2019[M1_credit_factor_sig_b2_2019$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
      }
      }
    }
  }
}





tabla<-data.frame(matrix(nrow=4,ncol=6))
tabla[,1]<-c("IQR","Sig.","IQR","Sig.")
colnames(tabla)<-c("","h=0","h=1","h=4","h=8","h=12")

tabla_f<- data.frame()
for (varcoef in c("NFCI")){
    
  if (varcoef == "NFCI"){
    M_b_2008 <- M1_credit_factor_coef_b2_2008
    M_sig_2008 <- M1_credit_factor_sig_b2_2008
    M_b_2019 <- M1_credit_factor_coef_b2_2019
    M_sig_2019 <- M1_credit_factor_sig_b2_2019
  } else if (varcoef == "FUI"){
    M_b_2008 <- M2_credit_factor_coef_b2_2008
    M_sig_2008 <- M2_credit_factor_sig_b2_2008
    M_b_2019 <- M2_credit_factor_coef_b2_2019
    M_sig_2019 <- M2_credit_factor_sig_b2_2019
  }
    
      h_name=paste0("h",h)
      for (j in 2:6){
        
        tabla[1,j]=paste0("[",round(quantile(M_b_2008[,j],0.25),2),";",
                            round(quantile(M_b_2008[,j],0.75),2),"]")
        tabla[2,j]=round(mean(M_sig_2008[,j]),2)
        
        tabla[3,j]=paste0("[",round(quantile(M_b_2019[,j],0.25),2),";",
                            round(quantile(M_b_2019[,j],0.75),2),"]")
        tabla[4,j]=round(mean(M_sig_2019[,j]),2)
      }
    tabla_f<-rbind(tabla_f,tabla)
}
write.table(tabla_f, file = paste0("../Tables/LAC_CaR_rev_.txt"), sep = ",", quote = FALSE, row.names = F)


