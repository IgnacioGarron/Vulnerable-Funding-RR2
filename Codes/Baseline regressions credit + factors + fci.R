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
fci <- read.csv("../Data/nfci.csv")[,-c(1,2)]

# Vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$date) # fechas forma 1

# Merge with nfci country-level data
date <- data %>% filter(country=="Bolivia" & date>='1973-01-01' & date<='2016-10-01') %>% 
  select(date)
fci<-cbind(date,fci)
fci_long<-fci %>% pivot_longer(names_to="country",values_to = "fci",cols=-1)

data<-left_join(data,fci_long,by=c("date","country"))

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

# data %>% 
#   group_by(country) %>% 
#   filter(complete.cases(credit,fci) & date=="1973-01-01") %>% select(country) 
# credit_countries<-as.matrix(credit_countries)

credit_countries<-c("Australia","Austria","Belgium","Canada","Denmark","Finland","France",
"Germany","Greece","Iceland","Ireland","Italy","Japan","Korea","Mexico",
"Netherlands","Norway","Portugal","Spain", "Sweden","Switzerland")


  

length(credit_countries)


tabla_raw<-data.frame(matrix(nrow=length(credit_countries),ncol=6))
tabla_raw[,1]<-credit_countries
colnames(tabla_raw)<-c("country","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

tabla_raw_pred<-data.frame(matrix(ncol=length(credit_countries),nrow=188)) #length fci 1973-1
colnames(tabla_raw_pred)<-credit_countries
#rownames(tabla_raw_pred)<-date$date


# M1: y_t+h=b1*y_t+b2*nfciUS_t+b3*fci_it+b4*RealUS_t+b5*cross_factor+b6*Worldvol
coef_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
coef_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
coef_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
coef_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
coef_b5=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
coef_b6=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
             "h8"=tabla_raw,"h12"=tabla_raw)
sig_b1=list("h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)
sig_b2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)
sig_b3=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)
sig_b4=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)
sig_b5=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)
sig_b6=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
            "h8"=tabla_raw,"h12"=tabla_raw)

# M2: y_t+h=b1*y_t+b3*fci_it+b4*RealUS_t+b5*cross_factor+b6*Worldvol (SIN NFCI)



#TL predictions
TL1=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
         "h8"=tabla_raw,"h12"=tabla_raw)

TL2=list("h0"=tabla_raw,"h1"=tabla_raw,"h4"=tabla_raw,
         "h8"=tabla_raw,"h12"=tabla_raw)


#CaR predictions
Pred_list=list("h0"=tabla_raw_pred,"h1"=tabla_raw_pred[-1,],"h4"=tabla_raw_pred[-1:-4,],
               "h8"=tabla_raw_pred[-1:-8,],"h12"=tabla_raw_pred[-1:-12,])


banner("Parte 2:", "Quantile regressions for credit", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 2:                               ###
###                   QUANTILE REGRESSIONS FOR CREDIT                   ###
###                                                                     ###
###########################################################################
###########################################################################

# Test

# h<-4
# tau<-0.05
# country_name="Bolivia"

# horizon
h_horizon<-c(0,1,4,8,12) # Acá empieza loop para h


for (h in h_horizon){
  start.time <- Sys.time()
  
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
                Real_z=VOL_GROWTH,
                fci_z=fci, 
                credit_f_z=credit_f, # credit factor
                f_global_z=VOL_WPI,
                f_fin_z=SV,
                USUN_z=USUN) %>% 
    mutate(stock_h=lead(stock_z,h),
           credit_h=lead(credit_z,h),
           gdp_h=lead(gdp_z,h))
  
  
  for (country_name in credit_countries){ # acá empieza loop para país CAMBIAR PARA GAR E EAR
    
    data_model<-data_reg %>% 
      group_by(country) %>% 
      filter(complete.cases(credit_h,credit_z,fci_z,NFCI_z,f_global_z),country==country_name) 
    Y.train=as.matrix(data_model[,"credit_h"])
    
    if (h==0){
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI_z","fci_z","Real_z","credit_f_z","f_global_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("fci_z","Real_z","credit_f_z","f_global_z")]))
    } else {
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","NFCI_z","fci_z","Real_z","credit_f_z","f_global_z")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit_z","fci_z","Real_z","credit_f_z","f_global_z")]))
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
      
      #TL
      pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
      TL1[[paste0("h",h)]][TL1[[paste0("h",h)]]$country==country_name,j]=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
      
      diff_length=length(Pred_list[[paste0("h",h)]][,country_name])-length(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
      if(tau==0.05 & diff_length==0){
        Pred_list[[paste0("h",h)]][,country_name]=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
      } else if (tau==0.05 & diff_length!=0) {
        Pred_list[[paste0("h",h)]][,country_name]=c(as.matrix(X.train1)%*%as.matrix(M1$beta.hat),rep(NA,diff_length))}
      
      pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
      TL2[[paste0("h",h)]][TL2[[paste0("h",h)]]$country==country_name,j]=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
      
      if (h!=0){ # no hay h=0 para rezago
        
        #b
        coef_b1[[paste0("h",h)]][coef_b1[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[2]
        
        coef_b2[[paste0("h",h)]][coef_b2[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[3]
        
        coef_b3[[paste0("h",h)]][coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[4]
        
        coef_b4[[paste0("h",h)]][coef_b4[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[5]
        
        coef_b5[[paste0("h",h)]][coef_b5[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[6]
        
        coef_b6[[paste0("h",h)]][coef_b6[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[7]
        
        
        # sig 
        
        sig_b1[[paste0("h",h)]][sig_b1[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
        
        
        sig_b2[[paste0("h",h)]][sig_b2[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        
        sig_b3[[paste0("h",h)]][sig_b3[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[4,1] & 0 < M1$SETBB.confint[4,2], 0, 1)
        
        sig_b4[[paste0("h",h)]][sig_b4[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[5,1] & 0 < M1$SETBB.confint[5,2], 0, 1)
        
        sig_b5[[paste0("h",h)]][sig_b5[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[6,1] & 0 < M1$SETBB.confint[6,2], 0, 1)
        
        sig_b6[[paste0("h",h)]][sig_b6[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[7,1] & 0 < M1$SETBB.confint[7,2], 0, 1)
        
        
      } else {
        
        #b
        coef_b2[[paste0("h",h)]][coef_b2[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[2]
        
        coef_b3[[paste0("h",h)]][coef_b3[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[3]
        
        coef_b4[[paste0("h",h)]][coef_b4[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[4]
        
        coef_b5[[paste0("h",h)]][coef_b5[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[5]
        
        coef_b6[[paste0("h",h)]][coef_b6[[paste0("h",h)]]$country==country_name,j]=
          M1$beta.hat[6]
        
        #sig
        sig_b2[[paste0("h",h)]][sig_b2[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[2,1] & 0 < M1$SETBB.confint[2,2], 0, 1)
        
        sig_b3[[paste0("h",h)]][sig_b3[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[3,1] & 0 < M1$SETBB.confint[3,2], 0, 1)
        
        sig_b4[[paste0("h",h)]][sig_b4[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[4,1] & 0 < M1$SETBB.confint[4,2], 0, 1)
        
        sig_b5[[paste0("h",h)]][sig_b5[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[5,1] & 0 < M1$SETBB.confint[5,2], 0, 1)
        
        sig_b6[[paste0("h",h)]][sig_b6[[paste0("h",h)]]$country==country_name,j]=
          ifelse(0 > M1$SETBB.confint[6,1] & 0 < M1$SETBB.confint[6,2], 0, 1)
        
      }
    }
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}


save(coef_b1,file ="../Data/M1_credit_fci_coef_b1.RData")
save(coef_b2,file ="../Data/M1_credit_fci_coef_b2.RData")
save(coef_b3,file ="../Data/M1_credit_fci_b3.RData")
save(coef_b4,file ="../Data/M1_credit_fci_b4.RData")
save(coef_b5,file ="../Data/M1_credit_fci_b5.RData")
save(coef_b6,file ="../Data/M1_credit_fci_b6.RData")


save(sig_b1,file ="../Data/M1_credit_fci_sig_b1.RData")
save(sig_b2,file ="../Data/M1_credit_fci_sig_b2.RData")
save(sig_b3,file ="../Data/M1_credit_fci_sig_b3.RData")
save(sig_b4,file ="../Data/M1_credit_fci_sig_b4.RData")
save(sig_b5,file ="../Data/M1_credit_fci_sig_b5.RData")
save(sig_b6,file ="../Data/M1_credit_fci_sig_b6.RData")


save(TL1,file ="../Data/M1_credit_fci_TL.RData")
save(TL2,file ="../Data/M2_credit_fci_TL.RData")
save(Pred_list,file ="../Data/M1_credit_fci_Pred.RData")

