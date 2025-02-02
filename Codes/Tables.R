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

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

 tabla<-data.frame(matrix(nrow=18,ncol=7))
 tabla[,1]<-c("","y_t","","NFCI_t","","",
              "y_t","","FUI_t","","",
              "y_t","","NFCI_t","","FUI_t","","")
 tabla[,2]<-c("","IQR","Sig.","IQR","Sig.","TL",
              "IQR","Sig.","IQR","Sig.","TL",
              "IQR","Sig.","IQR","Sig.","IQR","Sig.","TL")
 colnames(tabla)<-c("","","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

 for (var in c("gdp","stock","credit")){
   
   M1_b1 <- loadRData(paste0("Data/M1_",var,"_coef_b1.RData"))
   M1_sig1 <- loadRData(paste0("Data/M1_",var,"_sig_b1.RData"))
   M1_b2 <- loadRData(paste0("Data/M1_",var,"_coef_b2.RData"))
   M1_sig2 <- loadRData(paste0("Data/M1_",var,"_sig_b2.RData"))
   M1_TL <- loadRData(paste0("Data/M1_",var,"_TL.RData"))
   
   M2_b1 <- loadRData(paste0("Data/M2_",var,"_coef_b1.RData"))
   M2_sig1 <- loadRData(paste0("Data/M2_",var,"_sig_b1.RData"))
   M2_b2 <- loadRData(paste0("Data/M2_",var,"_coef_b2.RData"))
   M2_sig2 <- loadRData(paste0("Data/M2_",var,"_sig_b2.RData"))
   M2_TL <- loadRData(paste0("Data/M2_",var,"_TL.RData"))
   
   M3_b1 <- loadRData(paste0("Data/M3_",var,"_coef_b1.RData"))
   M3_sig1 <- loadRData(paste0("Data/M3_",var,"_sig_b1.RData"))
   M3_b2 <- loadRData(paste0("Data/M3_",var,"_coef_b2.RData"))
   M3_sig2 <- loadRData(paste0("Data/M3_",var,"_sig_b2.RData"))
   M3_b3 <- loadRData(paste0("Data/M3_",var,"_coef_b3.RData"))
   M3_sig3 <- loadRData(paste0("Data/M3_",var,"_sig_b3.RData"))
   M3_TL <- loadRData(paste0("Data/M3_",var,"_TL.RData"))
   
   for (h in c("1","4","8","12")){
     h_name=paste0("h",h)
     for (j in 2:6){
       #M1
       tabla[2,j+1]=paste0("[",round(quantile(M1_b1[[h_name]][,j],0.25),2),";",
                           round(quantile(M1_b1[[h_name]][,j],0.75),2),"]")
       tabla[3,j+1]=round(mean(M1_sig1[[h_name]][,j]),2)
       tabla[4,j+1]=paste0("[",round(quantile(M1_b2[[h_name]][,j],0.25),2),";",
                           round(quantile(M1_b2[[h_name]][,j],0.75),2),"]")
       tabla[5,j+1]=round(mean(M1_sig2[[h_name]][,j]),2)
       tabla[6,j+1]=round(mean(M1_TL[[h_name]][,j]),2)
       #M2
       tabla[7,j+1]=paste0("[",round(quantile(M2_b1[[h_name]][,j],0.25),2),";",
                           round(quantile(M2_b1[[h_name]][,j],0.75),2),"]")
       tabla[8,j+1]=round(mean(M2_sig1[[h_name]][,j]),2)
       tabla[9,j+1]=paste0("[",round(quantile(M2_b2[[h_name]][,j],0.25),2),";",
                           round(quantile(M2_b2[[h_name]][,j],0.75),2),"]")
       tabla[10,j+1]=round(mean(M2_sig2[[h_name]][,j]),2)
       tabla[11,j+1]=round(mean(M2_TL[[h_name]][,j]),2)
       #M3
       tabla[12,j+1]=paste0("[",round(quantile(M3_b1[[h_name]][,j],0.25),2),";",
                            round(quantile(M3_b1[[h_name]][,j],0.75),2),"]")
       tabla[13,j+1]=round(mean(M3_sig1[[h_name]][,j]),2)
       tabla[14,j+1]=paste0("[",round(quantile(M3_b2[[h_name]][,j],0.25),2),";",
                            round(quantile(M3_b2[[h_name]][,j],0.75),2),"]")
       tabla[15,j+1]=round(mean(M3_sig2[[h_name]][,j]),2)
       tabla[16,j+1]=paste0("[",round(quantile(M3_b3[[h_name]][,j],0.25),2),";",
                            round(quantile(M3_b3[[h_name]][,j],0.75),2),"]")
       tabla[17,j+1]=round(mean(M3_sig3[[h_name]][,j]),2)
       tabla[18,j+1]=round(mean(M3_TL[[h_name]][,j]),2)
       
     }
     write.table(tabla, file = paste0("Tables/baseline_reg_",var,"h_",h,".txt"), sep = ",", quote = FALSE, row.names = F)
     
   }
 }

########### FOR H=0
 
 
 tabla<-data.frame(matrix(nrow=12,ncol=7))
 tabla[,1]<-c("","NFCI_t","","",
              "FU_t","","",
              "NFCI_t","","FU_t","","")
 tabla[,2]<-c("","IQR","Sig.","TL",
              "IQR","Sig.","TL",
              "IQR","Sig.","IQR","Sig.","TL")
 colnames(tabla)<-c("","","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")
 
 for (var in c("gdp","stock","credit")){
   

   M1_b2 <- loadRData(paste0("Data/M1_",var,"_coef_b2.RData"))
   M1_sig2 <- loadRData(paste0("Data/M1_",var,"_sig_b2.RData"))
   M1_TL <- loadRData(paste0("Data/M1_",var,"_TL.RData"))
  

   M2_b2 <- loadRData(paste0("Data/M2_",var,"_coef_b2.RData"))
   M2_sig2 <- loadRData(paste0("Data/M2_",var,"_sig_b2.RData"))
   M2_TL <- loadRData(paste0("Data/M2_",var,"_TL.RData"))
   

   M3_b2 <- loadRData(paste0("Data/M3_",var,"_coef_b2.RData"))
   M3_sig2 <- loadRData(paste0("Data/M3_",var,"_sig_b2.RData"))
   M3_b3 <- loadRData(paste0("Data/M3_",var,"_coef_b3.RData"))
   M3_sig3 <- loadRData(paste0("Data/M3_",var,"_sig_b3.RData"))
   M3_TL <- loadRData(paste0("Data/M3_",var,"_TL.RData"))
   
 
 for (h in c("0")){
   h_name=paste0("h",h)
   for (j in 2:6){
     #M1
     tabla[2,j+1]=paste0("[",round(quantile(M1_b2[[h_name]][,j],0.25),2),";",
                         round(quantile(M1_b2[[h_name]][,j],0.75),2),"]")
     tabla[3,j+1]=round(mean(M1_sig2[[h_name]][,j]),2)
     tabla[4,j+1]=round(mean(M1_TL[[h_name]][,j]),2)
     #M2
     tabla[5,j+1]=paste0("[",round(quantile(M2_b2[[h_name]][,j],0.25),2),";",
                         round(quantile(M2_b2[[h_name]][,j],0.75),2),"]")
     tabla[6,j+1]=round(mean(M2_sig2[[h_name]][,j]),2)
     tabla[7,j+1]=round(mean(M2_TL[[h_name]][,j]),2)
     #M3
     tabla[8,j+1]=paste0("[",round(quantile(M3_b2[[h_name]][,j],0.25),2),";",
                          round(quantile(M3_b2[[h_name]][,j],0.75),2),"]")
     tabla[9,j+1]=round(mean(M3_sig2[[h_name]][,j]),2)
     tabla[10,j+1]=paste0("[",round(quantile(M3_b3[[h_name]][,j],0.25),2),";",
                          round(quantile(M3_b3[[h_name]][,j],0.75),2),"]")
     tabla[11,j+1]=round(mean(M3_sig3[[h_name]][,j]),2)
     tabla[12,j+1]=round(mean(M3_TL[[h_name]][,j]),2)
     
   }
   write.table(tabla, file = paste0("Tables/baseline_reg_",var,"h_",h,".txt"), sep = ",", quote = FALSE, row.names = F)
   
 }
 }

