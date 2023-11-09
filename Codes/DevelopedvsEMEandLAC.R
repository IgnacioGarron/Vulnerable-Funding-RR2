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


EME_credit<-c("Argentina","Bolivia","Brazil",  "Chile",        "Colombia",       "Costa Rica",
              "Guatemala"  ,    "Honduras",   "India",  "Mexico",
              "Pakistan",       "Peru",         "Philippines", "Malaysia",       "Morocco",
              "Thailand",       "Turkey", "Uruguay","South Africa"  )

developded_credit<-c(   "Australia"   ,     "Austria",      "Belgium",             
                        "Canada"  ,      
                        "Cyprus",         "Denmark"   ,     "Finland",      "France",         "Germany",       
                        "Greece",          "Iceland",              
                        "Ireland",        "Israel"      ,   "Italy",          "Japan",          "Korea",         
                        "Netherlands",    "New Zealand",   
                        "Norway",          "Portugal",      
                        "Spain" ,         "Sweden",       "Switzerland",    "Taiwan",        
                        "United Kingdom")    

EME_stock<-c("Mexico","Peru" ,"Philippines", "India", "Chile",  "Peru"           ,"Philippines", 
             "South Africa")

developded_stock<-c("Australia"      ,"Austria"        ,"Belgium"        ,"Canada"         ,        
                    "Denmark"        ,"Finland"        ,"France"         ,"Germany"        ,         
                    "Ireland"        ,"Israel"         ,"Italy"          ,"Japan"          ,        
                    "Netherlands"    ,"New Zealand"    ,"Norway"    ,   
                    "Spain"          ,"Sweden"         ,"Switzerland"    ,"United Kingdom")


##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

tabla<-data.frame(matrix(nrow=10,ncol=7))
tabla[,1]<-c("NFCI_t","","FU_t","","dTL",
             "NFCI_t","","FU_t","","dTL")
tabla[,2]<-c("IQR","Sig.","IQR","Sig.","dTL","IQR","Sig.",
             "IQR","Sig.","dTL")
colnames(tabla)<-c("","","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

for (var in c("stock","credit")){
  
  if (var=="credit"){
    sort1<-EME_credit
    sort2<-developded_credit
    
    sort1<-data.frame(matrix(sort1))
    colnames(sort1)<-"country"
    
    sort2<-data.frame(matrix(sort2))
    colnames(sort2)<-"country"
  } else{
    sort1<-EME_stock
    sort2<-developded_stock
    
    sort1<-data.frame(matrix(sort1))
    colnames(sort1)<-"country"
    
    sort2<-data.frame(matrix(sort2))
    colnames(sort2)<-"country"
  }

  M3_b2 <- loadRData(paste0("Data/M3_",var,"_factor_coef_b2.RData"))
  M3_sig2 <- loadRData(paste0("Data/M3_",var,"_factor_sig_b2.RData"))
  M3_b3 <- loadRData(paste0("Data/M3_",var,"_factor_coef_b3.RData"))
  M3_sig3 <- loadRData(paste0("Data/M3_",var,"_factor_sig_b3.RData"))
  
  M3_TL <- loadRData(paste0("Data/M3_",var,"_factor_TL.RData"))
  M3_TLb <- loadRData(paste0("Data/M3_",var,"_TL.RData"))
  
  for (h in c("0","1","4","8","12")){
    h_name=paste0("h",h)
    for (j in 2:6){
      #M3

      s1_b2<-left_join(sort1,M3_b2[[h_name]],by="country")
      s1_sig2<-left_join(sort1,M3_sig2[[h_name]],by="country")
      s1_b3<-left_join(sort1,M3_b3[[h_name]],by="country")
      s1_sig3<-left_join(sort1,M3_sig3[[h_name]],by="country")
      
      s1_TL<-left_join(sort1,M3_TL[[h_name]],by="country")
      s1_TLb<-left_join(sort1,M3_TLb[[h_name]],by="country")
      
      s2_b2<-left_join(sort2,M3_b2[[h_name]],by="country")
      s2_sig2<-left_join(sort2,M3_sig2[[h_name]],by="country")
      s2_b3<-left_join(sort2,M3_b3[[h_name]],by="country")
      s2_sig3<-left_join(sort2,M3_sig3[[h_name]],by="country")
      
      s2_TL<-left_join(sort2,M3_TL[[h_name]],by="country")
      s2_TLb<-left_join(sort2,M3_TLb[[h_name]],by="country")
      
      
      tabla[1,j+1]=paste0("[",round(quantile(s1_b2[,j],0.25),2),";",
                           round(quantile(s1_b2[,j],0.75),2),"]")
      tabla[2,j+1]=round(mean(s1_sig2[,j]),2)
      tabla[3,j+1]=paste0("[",round(quantile(s1_b3[,j],0.25),2),";",
                           round(quantile(s1_b3[,j],0.75),2),"]")
      tabla[4,j+1]=round(mean(s1_sig3[,j]),2)
      tabla[5,j+1]=round(mean(s1_TL[,j]-s1_TLb[,j]),2)
      
      tabla[6,j+1]=paste0("[",round(quantile(s2_b2[,j],0.25),2),";",
                          round(quantile(s2_b2[,j],0.75),2),"]")
      tabla[7,j+1]=round(mean(s2_sig2[,j]),2)
      tabla[8,j+1]=paste0("[",round(quantile(s2_b3[,j],0.25),2),";",
                          round(quantile(s2_b3[,j],0.75),2),"]")
      tabla[9,j+1]=round(mean(s2_sig3[,j]),2)
      tabla[10,j+1]=round(mean(s2_TL[,j]-s2_TLb[,j]),2)
      
      
    }
    write.table(tabla, file = paste0("Tables/DEVvsEME_reg_factor_",var,"h_",h,".txt"), sep = ",", quote = FALSE, row.names = F)
  }
}

