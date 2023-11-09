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

# horizon

h_horizon<-c(0,1,4,8,12) # Acá empieza loop para h

country_name<-"Chile"



banner("Parte 2:", "Quantile regressions for credit", emph = TRUE)
###########################################################################
############################################################################
###                                                                      ###
###                               PARTE 1:                               ###
###                 REGRESIONES CUANT?LICAS PARA CR?DITO                 ###
###                                                                      ###
############################################################################
############################################################################


tabla<-data.frame(matrix(nrow=32,ncol=7))
tabla[,1]<-c("Cons.","","y_t","","NFCI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","FUI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","NFCI_t","","FUI_t","","Gmacro_t","","Gfin_t","")
tabla[,2]<-c("Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI")
colnames(tabla)<-c("","","h=0","h=1","h=4","h=8","h=12")


j=3
for (h in h_horizon){
 
  data_reg <- data %>% group_by(country) %>%
    mutate(credit=log(credit/lag(credit)),
           stock=log(stock/lag(stock)),
           gdp=log(gdp,lag(gdp))) %>% 
    mutate(stock_h=lead(stock,h),
           credit_h=lead(credit,h),
           gdp_h=lead(gdp,h))
  
  data_model<-data_reg %>% 
    group_by(country) %>% 
    filter(complete.cases(credit_h,credit,NFCI),country==country_name) 
  Y.train=as.matrix(data_model[,"credit_h"])
  
  
    if (h==0){
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","global_factor","fin_factor")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN","global_factor","fin_factor")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN","global_factor","fin_factor")]))
      X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI")]))
      X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN")]))
      X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN")]))
    } else {
      X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","NFCI","global_factor","fin_factor")]))
      X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","USUN","global_factor","fin_factor")]))
      X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","NFCI","USUN","global_factor","fin_factor")]))
      X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","NFCI")]))
      X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","USUN")]))
      X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("credit","NFCI","USUN")]))
      
    }

  
  tau=c(0.05) # aca empieza loop quantile

  M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2<- QregBB(Y.train,X.train2,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3<- QregBB(Y.train,X.train3,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M1tb<- QregBB(Y.train,X.train1tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2tb<- QregBB(Y.train,X.train2tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3tb<- QregBB(Y.train,X.train3tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
    
  #TL
  pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
  TL1= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
  TL2=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  pred=(as.matrix(X.train3)%*%as.matrix(M3$beta.hat))
  TL3=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  pred=(as.matrix(X.train1tb)%*%as.matrix(M1tb$beta.hat))
  TL1tb= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  pred=(as.matrix(X.train2tb)%*%as.matrix(M2tb$beta.hat))
  TL2tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  pred=(as.matrix(X.train3tb)%*%as.matrix(M3tb$beta.hat))
  TL3tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
    
  
  if (h!=0){
    
    #M1
    tabla[1,j+1]=round(M1$beta.hat[1],3)
    tabla[2,j+1]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[3,j+1]=round(M1$beta.hat[2],3)
    tabla[4,j+1]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[5,j+1]=round(M1$beta.hat[3],3)
    tabla[6,j+1]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[7,j+1]=round(M1$beta.hat[4],3)
    tabla[8,j+1]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
    tabla[9,j+1]=round(M1$beta.hat[5],3)
    tabla[10,j+1]=paste0("[",round(M1$SETBB.confint[5,1],3),";",round(M1$SETBB.confint[5,2],3),"]") 
    
    #M1
    tabla[11,j+1]=round(M2$beta.hat[1],3)
    tabla[12,j+1]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[13,j+1]=round(M2$beta.hat[2],3)
    tabla[14,j+1]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[15,j+1]=round(M2$beta.hat[3],3)
    tabla[16,j+1]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[17,j+1]=round(M2$beta.hat[4],3)
    tabla[18,j+1]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    tabla[19,j+1]=round(M2$beta.hat[5],3)
    tabla[20,j+1]=paste0("[",round(M2$SETBB.confint[5,1],3),";",round(M2$SETBB.confint[5,2],3),"]") 
    
     #M1
    tabla[21,j+1]=round(M3$beta.hat[1],3)
    tabla[22,j+1]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[23,j+1]=round(M3$beta.hat[2],3)
    tabla[24,j+1]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[25,j+1]=round(M3$beta.hat[3],3)
    tabla[26,j+1]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[27,j+1]=round(M3$beta.hat[4],3)
    tabla[28,j+1]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[29,j+1]=round(M3$beta.hat[5],3)
    tabla[30,j+1]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    tabla[31,j+1]=round(M3$beta.hat[6],3)
    tabla[32,j+1]=paste0("[",round(M3$SETBB.confint[6,1],3),";",round(M3$SETBB.confint[6,2],3),"]") 
    

    j=j+1
    }else{
    #M1
    tabla[1,3]=""
    tabla[2,3]=""
    tabla[3,3]=round(M1$beta.hat[1],3)
    tabla[4,3]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[5,3]=round(M1$beta.hat[2],3)
    tabla[6,3]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[7,3]=round(M1$beta.hat[3],3)
    tabla[8,3]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[9,3]=round(M1$beta.hat[4],3)
    tabla[10,3]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
     #M2
    tabla[11,3]=""
    tabla[12,3]=""
    tabla[13,3]=round(M2$beta.hat[1],3)
    tabla[14,3]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[15,3]=round(M2$beta.hat[2],3)
    tabla[16,3]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[17,3]=round(M2$beta.hat[3],3)
    tabla[18,3]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[19,3]=round(M2$beta.hat[4],3)
    tabla[20,3]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    #M3
    tabla[21,3]=""
    tabla[22,3]=""
    tabla[23,3]=round(M3$beta.hat[1],3)
    tabla[24,3]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[25,3]=round(M3$beta.hat[2],3)
    tabla[26,3]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[27,3]=round(M3$beta.hat[3],3)
    tabla[28,3]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[29,3]=round(M3$beta.hat[4],3)
    tabla[30,3]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[31,3]=round(M3$beta.hat[5],3)
    tabla[32,3]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    
          
    }
}   
  
write.table(tabla, file = paste0("Tables/baseline_CaR_",tau,"_",country_name,".txt"), sep = ",", quote = FALSE, row.names = F)




banner("Parte 2:", "Quantile regressions for stocks", emph = TRUE)
###########################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###                 REGRESIONES CUANT?LICAS PARA STOCKS                  ###
###                                                                      ###
############################################################################
############################################################################


tabla<-data.frame(matrix(nrow=32,ncol=7))
tabla[,1]<-c("Cons.","","y_t","","NFCI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","FUI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","NFCI_t","","FUI_t","","Gmacro_t","","Gfin_t","")
tabla[,2]<-c("Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI")
colnames(tabla)<-c("","","h=0","h=1","h=4","h=8","h=12")

j=3
for (h in h_horizon){
  
  data_reg <- data %>% group_by(country) %>%
    mutate(credit=log(credit/lag(credit)),
           stock=log(stock/lag(stock)),
           gdp=log(gdp,lag(gdp))) %>% 
    mutate(stock_h=lead(stock,h),
           credit_h=lead(credit,h),
           gdp_h=lead(gdp,h))
  
  data_model<-data_reg %>% 
    group_by(country) %>% 
    filter(complete.cases(stock_h,stock,NFCI),country==country_name) 
  Y.train=as.matrix(data_model[,"stock_h"])
  
  
  if (h==0){
    X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","global_factor","fin_factor")]))
    X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN","global_factor","fin_factor")]))
    X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN","global_factor","fin_factor")]))
    X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI")]))
    X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN")]))
    X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN")]))
  } else {
    X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","NFCI","global_factor","fin_factor")]))
    X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","USUN","global_factor","fin_factor")]))
    X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","NFCI","USUN","global_factor","fin_factor")]))
    X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","NFCI")]))
    X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","USUN")]))
    X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("stock","NFCI","USUN")]))
    
  }
  
  
  tau=c(0.05) # aca empieza loop quantile
  
  M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2<- QregBB(Y.train,X.train2,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3<- QregBB(Y.train,X.train3,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M1tb<- QregBB(Y.train,X.train1tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2tb<- QregBB(Y.train,X.train2tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3tb<- QregBB(Y.train,X.train3tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  
  #TL
  pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
  TL1= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
  TL2=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train3)%*%as.matrix(M3$beta.hat))
  TL3=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train1tb)%*%as.matrix(M1tb$beta.hat))
  TL1tb= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train2tb)%*%as.matrix(M2tb$beta.hat))
  TL2tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train3tb)%*%as.matrix(M3tb$beta.hat))
  TL3tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  
  if (h!=0){
    
    #M1
    tabla[1,j+1]=round(M1$beta.hat[1],3)
    tabla[2,j+1]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[3,j+1]=round(M1$beta.hat[2],3)
    tabla[4,j+1]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[5,j+1]=round(M1$beta.hat[3],3)
    tabla[6,j+1]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[7,j+1]=round(M1$beta.hat[4],3)
    tabla[8,j+1]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
    tabla[9,j+1]=round(M1$beta.hat[5],3)
    tabla[10,j+1]=paste0("[",round(M1$SETBB.confint[5,1],3),";",round(M1$SETBB.confint[5,2],3),"]") 
    
    #M1
    tabla[11,j+1]=round(M2$beta.hat[1],3)
    tabla[12,j+1]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[13,j+1]=round(M2$beta.hat[2],3)
    tabla[14,j+1]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[15,j+1]=round(M2$beta.hat[3],3)
    tabla[16,j+1]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[17,j+1]=round(M2$beta.hat[4],3)
    tabla[18,j+1]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    tabla[19,j+1]=round(M2$beta.hat[5],3)
    tabla[20,j+1]=paste0("[",round(M2$SETBB.confint[5,1],3),";",round(M2$SETBB.confint[5,2],3),"]") 
    
    #M1
    tabla[21,j+1]=round(M3$beta.hat[1],3)
    tabla[22,j+1]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[23,j+1]=round(M3$beta.hat[2],3)
    tabla[24,j+1]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[25,j+1]=round(M3$beta.hat[3],3)
    tabla[26,j+1]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[27,j+1]=round(M3$beta.hat[4],3)
    tabla[28,j+1]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[29,j+1]=round(M3$beta.hat[5],3)
    tabla[30,j+1]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    tabla[31,j+1]=round(M3$beta.hat[6],3)
    tabla[32,j+1]=paste0("[",round(M3$SETBB.confint[6,1],3),";",round(M3$SETBB.confint[6,2],3),"]") 
    
    
    j=j+1
  }else{
    #M1
    tabla[1,3]=""
    tabla[2,3]=""
    tabla[3,3]=round(M1$beta.hat[1],3)
    tabla[4,3]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[5,3]=round(M1$beta.hat[2],3)
    tabla[6,3]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[7,3]=round(M1$beta.hat[3],3)
    tabla[8,3]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[9,3]=round(M1$beta.hat[4],3)
    tabla[10,3]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
    #M2
    tabla[11,3]=""
    tabla[12,3]=""
    tabla[13,3]=round(M2$beta.hat[1],3)
    tabla[14,3]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[15,3]=round(M2$beta.hat[2],3)
    tabla[16,3]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[17,3]=round(M2$beta.hat[3],3)
    tabla[18,3]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[19,3]=round(M2$beta.hat[4],3)
    tabla[20,3]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    #M3
    tabla[21,3]=""
    tabla[22,3]=""
    tabla[23,3]=round(M3$beta.hat[1],3)
    tabla[24,3]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[25,3]=round(M3$beta.hat[2],3)
    tabla[26,3]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[27,3]=round(M3$beta.hat[3],3)
    tabla[28,3]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[29,3]=round(M3$beta.hat[4],3)
    tabla[30,3]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[31,3]=round(M3$beta.hat[5],3)
    tabla[32,3]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    
    
  }
}   

write.table(tabla, file = paste0("Tables/baseline_EaR_",tau,"_",country_name,".txt"), sep = ",", quote = FALSE, row.names = F)



banner("Parte 3:", "Quantile regressions for gdp", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 3:                               ###
###                     QUANTILE REGRESSIONS FOR GDP                     ###
###                                                                      ###
############################################################################
############################################################################


tabla<-data.frame(matrix(nrow=32,ncol=7))
tabla[,1]<-c("Cons.","","y_t","","NFCI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","FUI_t","","Gmacro_t","","Gfin_t","",
             "Cons.","","y_t","","NFCI_t","","FUI_t","","Gmacro_t","","Gfin_t","")
tabla[,2]<-c("Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI",
             "Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI","Coef.","CI")
colnames(tabla)<-c("","","h=0","h=1","h=4","h=8","h=12")

j=3
for (h in h_horizon){
  
  data_reg <- data %>% group_by(country) %>%
    mutate(credit=log(credit/lag(credit)),
           stock=log(stock/lag(stock)),
           gdp=log(gdp,lag(gdp))) %>% 
    mutate(stock_h=lead(stock,h),
           credit_h=lead(credit,h),
           gdp_h=lead(gdp,h))
  
  data_model<-data_reg %>% 
    group_by(country) %>% 
    filter(complete.cases(gdp_h,gdp,NFCI),country==country_name) 
  Y.train=as.matrix(data_model[,"gdp_h"])
  
  
  if (h==0){
    X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","global_factor","fin_factor")]))
    X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN","global_factor","fin_factor")]))
    X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN","global_factor","fin_factor")]))
    X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI")]))
    X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("USUN")]))
    X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("NFCI","USUN")]))
  } else {
    X.train1<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","NFCI","global_factor","fin_factor")]))
    X.train2<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","USUN","global_factor","fin_factor")]))
    X.train3<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","NFCI","USUN","global_factor","fin_factor")]))
    X.train1tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","NFCI")]))
    X.train2tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","USUN")]))
    X.train3tb<-as.matrix(cbind(rep(1,length(Y.train[,1])),data_model[,c("gdp","NFCI","USUN")]))
    
  }
  
  
  tau=c(0.05) # aca empieza loop quantile
  
  M1<- QregBB(Y.train,X.train1,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2<- QregBB(Y.train,X.train2,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3<- QregBB(Y.train,X.train3,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M1tb<- QregBB(Y.train,X.train1tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M2tb<- QregBB(Y.train,X.train2tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  M3tb<- QregBB(Y.train,X.train3tb,tau=tau,l=4,B=500,h=NULL,alpha=0.1)
  
  #TL
  pred=(as.matrix(X.train1)%*%as.matrix(M1$beta.hat))
  TL1= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train2)%*%as.matrix(M2$beta.hat))
  TL2=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train3)%*%as.matrix(M3$beta.hat))
  TL3=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train1tb)%*%as.matrix(M1tb$beta.hat))
  TL1tb= mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train2tb)%*%as.matrix(M2tb$beta.hat))
  TL2tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  pred=(as.matrix(X.train3tb)%*%as.matrix(M3tb$beta.hat))
  TL3tb=mean((Y.train-pred)*(tau-ifelse(Y.train<pred,1,0)))
  
  
  if (h!=0){
    
    #M1
    tabla[1,j+1]=round(M1$beta.hat[1],3)
    tabla[2,j+1]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[3,j+1]=round(M1$beta.hat[2],3)
    tabla[4,j+1]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[5,j+1]=round(M1$beta.hat[3],3)
    tabla[6,j+1]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[7,j+1]=round(M1$beta.hat[4],3)
    tabla[8,j+1]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
    tabla[9,j+1]=round(M1$beta.hat[5],3)
    tabla[10,j+1]=paste0("[",round(M1$SETBB.confint[5,1],3),";",round(M1$SETBB.confint[5,2],3),"]") 
    
    #M1
    tabla[11,j+1]=round(M2$beta.hat[1],3)
    tabla[12,j+1]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[13,j+1]=round(M2$beta.hat[2],3)
    tabla[14,j+1]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[15,j+1]=round(M2$beta.hat[3],3)
    tabla[16,j+1]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[17,j+1]=round(M2$beta.hat[4],3)
    tabla[18,j+1]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    tabla[19,j+1]=round(M2$beta.hat[5],3)
    tabla[20,j+1]=paste0("[",round(M2$SETBB.confint[5,1],3),";",round(M2$SETBB.confint[5,2],3),"]") 
    
    #M1
    tabla[21,j+1]=round(M3$beta.hat[1],3)
    tabla[22,j+1]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[23,j+1]=round(M3$beta.hat[2],3)
    tabla[24,j+1]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[25,j+1]=round(M3$beta.hat[3],3)
    tabla[26,j+1]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[27,j+1]=round(M3$beta.hat[4],3)
    tabla[28,j+1]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[29,j+1]=round(M3$beta.hat[5],3)
    tabla[30,j+1]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    tabla[31,j+1]=round(M3$beta.hat[6],3)
    tabla[32,j+1]=paste0("[",round(M3$SETBB.confint[6,1],3),";",round(M3$SETBB.confint[6,2],3),"]") 
    
    
    j=j+1
  }else{
    #M1
    tabla[1,3]=""
    tabla[2,3]=""
    tabla[3,3]=round(M1$beta.hat[1],3)
    tabla[4,3]=paste0("[",round(M1$SETBB.confint[1,1],3),";",round(M1$SETBB.confint[1,2],3),"]") 
    tabla[5,3]=round(M1$beta.hat[2],3)
    tabla[6,3]=paste0("[",round(M1$SETBB.confint[2,1],3),";",round(M1$SETBB.confint[2,2],3),"]") 
    tabla[7,3]=round(M1$beta.hat[3],3)
    tabla[8,3]=paste0("[",round(M1$SETBB.confint[3,1],3),";",round(M1$SETBB.confint[3,2],3),"]") 
    tabla[9,3]=round(M1$beta.hat[4],3)
    tabla[10,3]=paste0("[",round(M1$SETBB.confint[4,1],3),";",round(M1$SETBB.confint[4,2],3),"]") 
    #M2
    tabla[11,3]=""
    tabla[12,3]=""
    tabla[13,3]=round(M2$beta.hat[1],3)
    tabla[14,3]=paste0("[",round(M2$SETBB.confint[1,1],3),";",round(M2$SETBB.confint[1,2],3),"]") 
    tabla[15,3]=round(M2$beta.hat[2],3)
    tabla[16,3]=paste0("[",round(M2$SETBB.confint[2,1],3),";",round(M2$SETBB.confint[2,2],3),"]") 
    tabla[17,3]=round(M2$beta.hat[3],3)
    tabla[18,3]=paste0("[",round(M2$SETBB.confint[3,1],3),";",round(M2$SETBB.confint[3,2],3),"]") 
    tabla[19,3]=round(M2$beta.hat[4],3)
    tabla[20,3]=paste0("[",round(M2$SETBB.confint[4,1],3),";",round(M2$SETBB.confint[4,2],3),"]") 
    #M3
    tabla[21,3]=""
    tabla[22,3]=""
    tabla[23,3]=round(M3$beta.hat[1],3)
    tabla[24,3]=paste0("[",round(M3$SETBB.confint[1,1],3),";",round(M3$SETBB.confint[1,2],3),"]") 
    tabla[25,3]=round(M3$beta.hat[2],3)
    tabla[26,3]=paste0("[",round(M3$SETBB.confint[2,1],3),";",round(M3$SETBB.confint[2,2],3),"]") 
    tabla[27,3]=round(M3$beta.hat[3],3)
    tabla[28,3]=paste0("[",round(M3$SETBB.confint[3,1],3),";",round(M3$SETBB.confint[3,2],3),"]") 
    tabla[29,3]=round(M3$beta.hat[4],3)
    tabla[30,3]=paste0("[",round(M3$SETBB.confint[4,1],3),";",round(M3$SETBB.confint[4,2],3),"]") 
    tabla[31,3]=round(M3$beta.hat[5],3)
    tabla[32,3]=paste0("[",round(M3$SETBB.confint[5,1],3),";",round(M3$SETBB.confint[5,2],3),"]") 
    
    
  }
}   

write.table(tabla, file = paste0("Tables/baseline_GaR_",tau,"_",country_name,".txt"), sep = ",", quote = FALSE, row.names = F)

      