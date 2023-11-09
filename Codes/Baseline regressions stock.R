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

load("Data/M3_stock_coef_b3.RData")

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
# 
# tabla_raw<-data.frame(matrix(nrow=length(stock_countries),ncol=6))
# tabla_raw[,1]<-stock_countries
# colnames(tabla_raw)<-c("country","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")
# 
# # M1: y_t+h=b1*y_t+b2*nfci_t
# M1_stock_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M1_stock_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M1_stock_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# M1_stock_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# 
# # M2: y_t+h=b1*y_t+b2*fu_t
# M2_stock_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M2_stock_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M2_stock_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# M2_stock_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# 
# # M3: y_t+h=b1*y_t+24*nfci_t+b3*fu_t
# M3_stock_coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_coef_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                        "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_sig_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                       "h8"=tabla_raw,"h12"=tabla_raw)
# 
# #TL predictions
# M1_stock_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                   "h8"=tabla_raw,"h12"=tabla_raw)
# M2_stock_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                   "h8"=tabla_raw,"h12"=tabla_raw)
# M3_stock_TL=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
#                   "h8"=tabla_raw,"h12"=tabla_raw)


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

h_horizon<-c(0) # Acá empieza loop para h

for (h in h_horizon){
  
  data_reg <- data %>% group_by(country) %>%
    mutate(credit=log(credit/lag(credit)),
           stock=log(stock/lag(stock)),
           gdp=log(gdp/lag(gdp))) %>% 
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
  
  
  for (country_name in stock_countries){ # acá empieza loop para país
    
    data_model<-data_reg %>% 
      group_by(country) %>% 
      filter(complete.cases(stock_h,stock_z,NFCI_z),country==country_name) 
    Y.train=as.matrix(data_model[,"stock_h"])
    
    if (h==0){
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN_z")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z","USUN_z")]))
    } else {
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock_z","NFCI_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock_z","USUN_z")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock_z","NFCI_z","USUN_z")]))
    }
    
    tau_q =c(0.05,0.25,0.50,0.75,0.95) # aca empieza loop quantile
    
    for (tau in tau_q){
      if (tau==0.05){ j=2
      } else if(tau==0.25){ j=3
      } else if(tau==0.50){ j=4
      } else if(tau==0.75){ j=5
      } else if(tau==0.95){ j=6}
      
      
      # M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
      # M2<- QregBB(Y.train,X.train2,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
      M3<- QregBB(Y.train,X.train3,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
      
      # #TL
      # pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
      # M1_stock_TL[[paste0("h",h)]][M1_stock_TL[[paste0("h",h)]]$country==country_name,j]=
      #   mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
      # 
      # pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
      # M2_stock_TL[[paste0("h",h)]][M2_stock_TL[[paste0("h",h)]]$country==country_name,j]=
      #   mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
      # 
      # pred=(as.matrix(X.train3)%*%as.matrix(M3$beta.hat))
      # M3_stock_TL[[paste0("h",h)]][M3_stock_TL[[paste0("h",h)]]$country==country_name,j]=
      #   mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
      # 
      if (h!=0){ # no hay h=0 para rezago
        # 
        # #b1
        # M1_stock_coef_b1[[paste0("h",h)]][M1_stock_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        #   M1$beta.hat[2]
        # 
        # M1_stock_sig_b1[[paste0("h",h)]][M1_stock_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
        # 
        # M2_stock_coef_b1[[paste0("h",h)]][M2_stock_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        #   M2$beta.hat[2]
        # 
        # M2_stock_sig_b1[[paste0("h",h)]][M2_stock_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M2$SETBB.confint[2,1] & 0 < M2$SETBB.confint[2,2], 0, 1)
        # 
        # M3_stock_coef_b1[[paste0("h",h)]][M3_stock_coef_b1[[paste0("h",h)]]$country==country_name,j]=
        #   M3$beta.hat[2]
        # 
        # M3_stock_sig_b1[[paste0("h",h)]][M3_stock_sig_b1[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M3$SETBB.confint[2,1] & 0 < M3$SETBB.confint[2,2], 0, 1)
        # 
        # #b2
        # M1_stock_coef_b2[[paste0("h",h)]][M1_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]= 
        #   M1$beta.hat[3]
        # 
        # M1_stock_sig_b2[[paste0("h",h)]][M1_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]= 
        #   ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        # 
        # M2_stock_coef_b2[[paste0("h",h)]][M2_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]= 
        #   M2$beta.hat[3]
        # 
        # M2_stock_sig_b2[[paste0("h",h)]][M2_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M2$SETBB.confint[3,1] & 0 < M2$SETBB.confint[3,2], 0, 1)
        # 
        # M3_stock_coef_b2[[paste0("h",h)]][M3_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        #   M3$beta.hat[3]
        # 
        # M3_stock_sig_b2[[paste0("h",h)]][M3_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M3$SETBB.confint[3,1] & 0 < M3$SETBB.confint[3,2], 0, 1)
        # 
        # #b3
        # M3_stock_coef_b3[[paste0("h",h)]][M3_stock_coef_b3[[paste0("h",h)]]$country==country_name,j]=
        #   M3$beta.hat[4]
        # 
        # M3_stock_sig_b3[[paste0("h",h)]][M3_stock_sig_b3[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M3$SETBB.confint[4,1] & 0 < M3$SETBB.confint[4,2], 0, 1)
        # 
      } else {
        
        # #b2
        # M1_stock_coef_b2[[paste0("h",h)]][M1_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        #   M1$beta.hat[2]
        # 
        # M1_stock_sig_b2[[paste0("h",h)]][ M1_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
        # 
        # M2_stock_coef_b2[[paste0("h",h)]][M2_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        #   M2$beta.hat[2]
        # 
        # M2_stock_sig_b2[[paste0("h",h)]][M2_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M2$SETBB.confint[2,1] & 0 < M2$SETBB.confint[2,2], 0, 1)
        # 
        # M3_stock_coef_b2[[paste0("h",h)]][M3_stock_coef_b2[[paste0("h",h)]]$country==country_name,j]=
        #   M3$beta.hat[2]
        # 
        # M3_stock_sig_b2[[paste0("h",h)]][ M3_stock_sig_b2[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M3$SETBB.confint[2,1] & 0 < M3$SETBB.confint[2,2], 0, 1)
        # 
        M3_stock_coef_b3[[paste0("h",h)]][M3_stock_coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M3$beta.hat[3]
        
        # M3_stock_sig_b3[[paste0("h",h)]][M3_stock_sig_b3[[paste0("h",h)]]$country==country_name,j]=
        #   ifelse(0 > M3$SETBB.confint[3,1] & 0 < M3$SETBB.confint[3,2], 0, 1)
        # 
      }
    }
  }
}


save(M3_stock_coef_b3,file ="Data/M3_stock_coef_b3.RData")


# save(M1_stock_coef_b1,file ="M1_stock_coef_b1.RData")
# save(M2_stock_coef_b1,file ="M2_stock_coef_b1.RData")
# save(M3_stock_coef_b1,file ="M3_stock_coef_b1.RData")
# 
# save(M1_stock_coef_b2,file ="M1_stock_coef_b2.RData")
# save(M2_stock_coef_b2,file ="M2_stock_coef_b2.RData")
# save(M3_stock_coef_b2,file ="M3_stock_coef_b2.RData")
# 
# save(M3_stock_coef_b3,file ="M3_stock_coef_b3.RData")
# 
# save(M3_gdp_coef_b3,file ="Data/M3_gdp_coef_b3.RData")
# 
# 
# save(M1_stock_sig_b1,file ="M1_stock_sig_b1.RData")
# save(M2_stock_sig_b1,file ="M2_stock_sig_b1.RData")
# save(M3_stock_sig_b1,file ="M3_stock_sig_b1.RData")
# 
# save(M1_stock_sig_b2,file ="M1_stock_sig_b2.RData")
# save(M2_stock_sig_b2,file ="M2_stock_sig_b2.RData")
# save(M3_stock_sig_b2,file ="M3_stock_sig_b2.RData")
# 
# save(M3_stock_sig_b3,file ="M3_stock_sig_b3.RData")
# 
# 
# save(M1_stock_TL,file ="M1_stock_TL.RData")
# save(M2_stock_TL,file ="M2_stock_TL.RData")
# save(M3_stock_TL,file ="M3_stock_TL.RData")

