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
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Vulnerable_Funding")
data <- read.csv("Data/Data_final.csv")

# Vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$date) # fechas forma 1



banner("Parte 1:", "Tables", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 1:                               ###
###                              VARIABLES                              ###
###                                                                     ###
###########################################################################
###########################################################################

#set seed for bootstrap
set.seed(123) 

credit_countries<-c("Argentina",      "Australia" ,     "Austria",      "Belgium",        "Bolivia",       
                    "Brazil",         "Canada"     ,    "Chile",        "Colombia",       "Costa Rica",    
                    "Cyprus",         "Denmark"   ,     "Finland",      "France",         "Germany",       
                    "Greece",         "Guatemala"  ,    "Honduras",     "Iceland",        "India",         
                    "Ireland",        "Israel"      ,   "Italy",          "Japan",          "Korea",         
                    "Malaysia",       "Mexico"   ,      "Morocco",      "Netherlands",    "New Zealand",   
                    "Norway",         "Pakistan",       "Peru",         "Philippines",    "Portugal",      
                    "South Africa",   "Spain" ,         "Sweden",       "Switzerland",    "Taiwan",        
                    "Thailand",       "Turkey",         "United Kingdom", "Uruguay")    

stock_countries<-c("Australia"      ,"Austria"        ,"Belgium"        ,"Canada"         ,"Chile",         
                   "Denmark"        ,"Finland"        ,"France"         ,"Germany"        ,"India",         
                   "Ireland"        ,"Israel"         ,"Italy"          ,"Japan"          ,"Mexico",        
                   "Netherlands"    ,"New Zealand"    ,"Norway"         ,"Peru"           ,"Philippines",   
                   "South Africa"   ,"Spain"          ,"Sweden"         ,"Switzerland"    ,"United Kingdom")

gdp_countries<-c("Argentina"      ,"Australia"      ,"Austria"        ,"Belgium"        ,"Brazil"         ,"Canada",        
                  "Chile"          ,"Denmark"        ,"Finland"        ,"France"         ,"Germany"        ,"Greece",        
                  "Iceland"        ,"India"          ,"Ireland"        ,"Israel"         ,"Italy"          ,"Japan" ,        
                  "Korea"          ,"Mexico"         ,"Morocco"        ,"Netherlands"    ,"New Zealand"    ,"Norway",        
                  "Pakistan"       ,"Philippines"    ,"Portugal"       ,"South Africa"   ,"Spain"          ,"Sweden" ,       
                  "Switzerland"    ,"Taiwan"         ,"Turkey"         ,"United Kingdom" ,"Uruguay"        ,"Luxembourg")    

length(credit_countries)
length(stock_countries)
length(gdp_countries)

tabla_raw<-data.frame(matrix(nrow=length(credit_countries),ncol=6))
tabla_raw[,1]<-credit_countries
colnames(tabla_raw)<-c("country","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

# M1: y_t+h=b1*y_t+b2*nfci_t+b3*g_macro+b4*g_fin
M1_credit_factor_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_coef_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_coef_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_sig_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)
M1_credit_factor_sig_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                             "h8"=tabla_raw,"h12"=tabla_raw)

# M2: y_t+h=b1*y_t+b2*fu_t+b3*g_macro+b4*g_fin
M2_credit_factor_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_coef_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_coef_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)

M2_credit_factor_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_sig_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                             "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_sig_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                             "h8"=tabla_raw,"h12"=tabla_raw)

# M3: y_t+h=b1*y_t+b2*nfci_t+b3*fu_t+b4*g_macro+b5*g_fin
M3_credit_factor_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_coef_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                       "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_coef_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_coef_b5=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                              "h8"=tabla_raw,"h12"=tabla_raw)

M3_credit_factor_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_sig_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_sig_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                             "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_sig_b5=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                             "h8"=tabla_raw,"h12"=tabla_raw)

#TL predictions
M1_credit_factor_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M2_credit_factor_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)
M3_credit_factor_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
                      "h8"=tabla_raw,"h12"=tabla_raw)


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
                f_global_z=global_factor,
                f_fin_z=fin_factor,
                USUN_z=USUN) %>% 
    mutate(stock_h=lead(stock_z,h),
           credit_h=lead(credit_z,h),
           gdp_h=lead(gdp_z,h))
  

  for (country_name in credit_countries){ # acá empieza loop para país
    
    data_model<-data_reg %>% 
      group_by(country) %>% 
      filter(complete.cases(credit_h,credit_z,NFCI_z),country==country_name) 
    Y.train=as.matrix(data_model[,"credit_h"])

    if (h==0){
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z","f_global_z","f_fin_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN_z","f_global_z","f_fin_z")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z","USUN_z","f_global_z","f_fin_z")]))
    } else {
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","NFCI_z","f_global_z","f_fin_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","USUN_z","f_global_z","f_fin_z")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","NFCI_z","USUN_z","f_global_z","f_fin_z")]))
    }
    
    tau_q =c(0.05,0.25,0.50,0.75,0.95) # aca empieza loop quantile
      
    for (tau in tau_q){
      if (tau==0.05){ j=2
      } else if(tau==0.25){ j=3
      } else if(tau==0.50){ j=4
      } else if(tau==0.75){ j=5
      } else if(tau==0.95){ j=6}


      M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
      M2<- QregBB(Y.train,X.train2,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
       M3<- QregBB(Y.train,X.train3,tau=tau,l=4,B=500,h=NULL,alpha=0.1)

      #TL
      pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
      M1_credit_factor_TL[[paste0("h",h)]][M1_credit_factor_TL[[paste0("h",h)]]$country==country_name,j]=
        mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))

      pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
      M2_credit_factor_TL[[paste0("h",h)]][M2_credit_factor_TL[[paste0("h",h)]]$country==country_name,j]=
        mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))

      pred=(as.matrix(X.train3)%*%as.matrix(M3$beta.hat))
      M3_credit_factor_TL[[paste0("h",h)]][M3_credit_factor_TL[[paste0("h",h)]]$country==country_name,j]=
        mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))

      if (h!=0){ # no hay h=0 para rezago
        
        #b1
      M1_credit_factor_coef_b1[[paste0("h",h)]][M1_credit_factor_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        M1$beta.hat[2]
      
      M1_credit_factor_coef_b2[[paste0("h",h)]][M1_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        M1$beta.hat[3]
      
      M1_credit_factor_coef_b3[[paste0("h",h)]][M1_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
        M1$beta.hat[4]
      
      M1_credit_factor_coef_b4[[paste0("h",h)]][M1_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
        M1$beta.hat[5]

      
      M2_credit_factor_coef_b1[[paste0("h",h)]][M2_credit_factor_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        M2$beta.hat[2]
      
      M2_credit_factor_coef_b2[[paste0("h",h)]][M2_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        M2$beta.hat[3]
      
      M2_credit_factor_coef_b3[[paste0("h",h)]][M2_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
        M2$beta.hat[4]
      
      M2_credit_factor_coef_b4[[paste0("h",h)]][M2_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
        M2$beta.hat[5]
      
      
      M3_credit_factor_coef_b1[[paste0("h",h)]][M3_credit_factor_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        M3$beta.hat[2]
      
      M3_credit_factor_coef_b2[[paste0("h",h)]][M3_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        M3$beta.hat[3]
      
      M3_credit_factor_coef_b3[[paste0("h",h)]][M3_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
        M3$beta.hat[4]
      
      M3_credit_factor_coef_b4[[paste0("h",h)]][M3_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
        M3$beta.hat[5]
      
      M3_credit_factor_coef_b5[[paste0("h",h)]][M3_credit_factor_coef_b5[[paste0("h",h)]]$country==country_name,j]=
        M3$beta.hat[6]
      
      # sig 
      
      M1_credit_factor_sig_b1[[paste0("h",h)]][M1_credit_factor_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)

      
      M1_credit_factor_sig_b2[[paste0("h",h)]][M1_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
      
      M1_credit_factor_sig_b3[[paste0("h",h)]][M1_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M1$SETBB.confint[4,1] & 0 < M1$SETBB.confint[4,2], 0, 1)
      
      M1_credit_factor_sig_b4[[paste0("h",h)]][M1_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M1$SETBB.confint[5,1] & 0 < M1$SETBB.confint[5,2], 0, 1)
      
      
      M2_credit_factor_sig_b1[[paste0("h",h)]][M2_credit_factor_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M2$SETBB.confint[2,1] & 0 < M2$SETBB.confint[2,2], 0, 1)

      M2_credit_factor_sig_b2[[paste0("h",h)]][M2_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M2$SETBB.confint[3,1] & 0 < M2$SETBB.confint[3,2], 0, 1)
      
      M2_credit_factor_sig_b3[[paste0("h",h)]][M2_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M2$SETBB.confint[4,1] & 0 < M2$SETBB.confint[4,2], 0, 1)
      
      M2_credit_factor_sig_b4[[paste0("h",h)]][M2_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M2$SETBB.confint[5,1] & 0 < M2$SETBB.confint[5,2], 0, 1)
      
      
      M3_credit_factor_sig_b1[[paste0("h",h)]][M3_credit_factor_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M3$SETBB.confint[2,1] & 0 < M3$SETBB.confint[2,2], 0, 1)

      M3_credit_factor_sig_b2[[paste0("h",h)]][M3_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M3$SETBB.confint[3,1] & 0 < M3$SETBB.confint[3,2], 0, 1)

      M3_credit_factor_sig_b3[[paste0("h",h)]][M3_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M3$SETBB.confint[4,1] & 0 < M3$SETBB.confint[4,2], 0, 1)
      
      M3_credit_factor_sig_b4[[paste0("h",h)]][M3_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M3$SETBB.confint[5,1] & 0 < M3$SETBB.confint[5,2], 0, 1)
      
      M3_credit_factor_sig_b5[[paste0("h",h)]][M3_credit_factor_sig_b5[[paste0("h",h)]]$country==country_name,j]=
        ifelse(0 > M3$SETBB.confint[6,1] & 0 < M3$SETBB.confint[6,2], 0, 1)

      } else {
        
        #b2
        M1_credit_factor_coef_b2[[paste0("h",h)]][M1_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[2]
        
        M1_credit_factor_coef_b3[[paste0("h",h)]][M1_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[3]
        
        M1_credit_factor_coef_b4[[paste0("h",h)]][M1_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[4]
        

        M2_credit_factor_coef_b2[[paste0("h",h)]][M2_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
          M2$beta.hat[2]
        
        M2_credit_factor_coef_b3[[paste0("h",h)]][M2_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M2$beta.hat[3]
        
        M2_credit_factor_coef_b4[[paste0("h",h)]][M2_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
          M2$beta.hat[4]
        
        
        M3_credit_factor_coef_b2[[paste0("h",h)]][M3_credit_factor_coef_b2[[paste0("h",h)]]$country==country_name,j]=
          M3$beta.hat[2]
        
        M3_credit_factor_coef_b3[[paste0("h",h)]][M3_credit_factor_coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M3$beta.hat[3]
        
        M3_credit_factor_coef_b4[[paste0("h",h)]][M3_credit_factor_coef_b4[[paste0("h",h)]]$country==country_name,j]=
          M3$beta.hat[4]
        
        M3_credit_factor_coef_b5[[paste0("h",h)]][M3_credit_factor_coef_b5[[paste0("h",h)]]$country==country_name,j]=
          M3$beta.hat[5]
        
        
        
        M1_credit_factor_sig_b2[[paste0("h",h)]][ M1_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
        
        M1_credit_factor_sig_b3[[paste0("h",h)]][ M1_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        
        M1_credit_factor_sig_b4[[paste0("h",h)]][ M1_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[4,1] & 0 < M1$SETBB.confint[4,2], 0, 1)
        
        

        M2_credit_factor_sig_b2[[paste0("h",h)]][M2_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M2$SETBB.confint[2,1] & 0 < M2$SETBB.confint[2,2], 0, 1)
        
        M2_credit_factor_sig_b3[[paste0("h",h)]][M2_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M2$SETBB.confint[3,1] & 0 < M2$SETBB.confint[3,2], 0, 1)
        
        M2_credit_factor_sig_b4[[paste0("h",h)]][M2_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M2$SETBB.confint[4,1] & 0 < M2$SETBB.confint[4,2], 0, 1)
        

        M3_credit_factor_sig_b2[[paste0("h",h)]][ M3_credit_factor_sig_b2[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M3$SETBB.confint[2,1] & 0 < M3$SETBB.confint[2,2], 0, 1)
        
        M3_credit_factor_sig_b3[[paste0("h",h)]][M3_credit_factor_sig_b3[[paste0("h",h)]]$country==country_name,j]=
           ifelse(0 > M3$SETBB.confint[3,1] & 0 < M3$SETBB.confint[3,2], 0, 1)
        
        M3_credit_factor_sig_b4[[paste0("h",h)]][M3_credit_factor_sig_b4[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M3$SETBB.confint[4,1] & 0 < M3$SETBB.confint[4,2], 0, 1)
        
        M3_credit_factor_sig_b5[[paste0("h",h)]][M3_credit_factor_sig_b5[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M3$SETBB.confint[5,1] & 0 < M3$SETBB.confint[5,2], 0, 1)
         
      }
   }
  }
}




save(M1_credit_factor_coef_b1,file ="Data/M1_credit_factor_coef_b1.RData")
save(M2_credit_factor_coef_b1,file ="Data/M2_credit_factor_coef_b1.RData")
save(M3_credit_factor_coef_b1,file ="Data/M3_credit_factor_coef_b1.RData")

save(M1_credit_factor_coef_b2,file ="Data/M1_credit_factor_coef_b2.RData")
save(M2_credit_factor_coef_b2,file ="Data/M2_credit_factor_coef_b2.RData")
save(M3_credit_factor_coef_b2,file ="Data/M3_credit_factor_coef_b2.RData")

save(M1_credit_factor_coef_b3,file ="Data/M1_credit_factor_coef_b3.RData")
save(M2_credit_factor_coef_b3,file ="Data/M2_credit_factor_coef_b3.RData")
save(M3_credit_factor_coef_b3,file ="Data/M3_credit_factor_coef_b3.RData")

save(M1_credit_factor_coef_b4,file ="Data/M1_credit_factor_coef_b4.RData")
save(M2_credit_factor_coef_b4,file ="Data/M2_credit_factor_coef_b4.RData")
save(M3_credit_factor_coef_b4,file ="Data/M3_credit_factor_coef_b4.RData")

save(M3_credit_factor_coef_b5,file ="Data/M3_credit_factor_coef_b5.RData")


save(M1_credit_factor_sig_b1,file ="Data/M1_credit_factor_sig_b1.RData")
save(M2_credit_factor_sig_b1,file ="Data/M2_credit_factor_sig_b1.RData")
save(M3_credit_factor_sig_b1,file ="Data/M3_credit_factor_sig_b1.RData")

save(M1_credit_factor_sig_b2,file ="Data/M1_credit_factor_sig_b2.RData")
save(M2_credit_factor_sig_b2,file ="Data/M2_credit_factor_sig_b2.RData")
save(M3_credit_factor_sig_b2,file ="Data/M3_credit_factor_sig_b2.RData")

save(M1_credit_factor_sig_b3,file ="Data/M1_credit_factor_sig_b3.RData")
save(M2_credit_factor_sig_b3,file ="Data/M2_credit_factor_sig_b3.RData")
save(M3_credit_factor_sig_b3,file ="Data/M3_credit_factor_sig_b3.RData")

save(M1_credit_factor_sig_b4,file ="Data/M1_credit_factor_sig_b4.RData")
save(M2_credit_factor_sig_b4,file ="Data/M2_credit_factor_sig_b4.RData")
save(M3_credit_factor_sig_b4,file ="Data/M3_credit_factor_sig_b4.RData")

save(M3_credit_factor_sig_b5,file ="Data/M3_credit_factor_sig_b5.RData")



save(M1_credit_factor_TL,file ="Data/M1_credit_factor_TL.RData")
save(M2_credit_factor_TL,file ="Data/M2_credit_factor_TL.RData")
save(M3_credit_factor_TL,file ="Data/M3_credit_factor_TL.RData")







