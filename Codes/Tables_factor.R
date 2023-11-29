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

length(credit_countries)
length(stock_countries)

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

tabla<-data.frame(matrix(nrow=34,ncol=7))
tabla[,1]<-c("h=0","nfciUS_t","","macroUS_t","","crossG_t","","macroG_t","",
             "h=1","y_it","","nfciUS_t","","macroUS_t","","crossG_t","","macroG_t","",
             "h=4","y_it","","nfciUS_t","","macroUS_t","","crossG_t","","macroG_t","","M1","M2","M3")
tabla[,2]<-c("","IQR","Sig.","IQR","Sig.","IQR","Sig.","IQR","Sig.",
             "","IQR","Sig.","IQR","Sig.","IQR","Sig.","IQR","Sig.","IQR","Sig.",
             "","IQR","Sig.","IQR","Sig.","IQR","Sig.","IQR","Sig.","IQR","Sig.","dTL","dTL","dTL")
colnames(tabla)<-c("","","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

for (var in c("credit","stock","gdp")){
  
  M1_b1 <- loadRData(paste0("../Data/M1_",var,"_baseline_coef_b1.RData"))
  M1_sig1 <- loadRData(paste0("../Data/M1_",var,"_baseline_sig_b1.RData"))
  M1_b2 <- loadRData(paste0("../Data/M1_",var,"_baseline_coef_b2.RData"))
  M1_sig2 <- loadRData(paste0("../Data/M1_",var,"_baseline_sig_b2.RData"))
  M1_b3 <- loadRData(paste0("../Data/M1_",var,"_baseline_b3.RData"))
  M1_sig3 <- loadRData(paste0("../Data/M1_",var,"_baseline_sig_b3.RData"))
  M1_b4 <- loadRData(paste0("../Data/M1_",var,"_baseline_b4.RData"))
  M1_sig4 <- loadRData(paste0("../Data/M1_",var,"_baseline_sig_b4.RData"))
  M1_b5 <- loadRData(paste0("../Data/M1_",var,"_baseline_b5.RData"))
  M1_sig5 <- loadRData(paste0("../Data/M1_",var,"_baseline_sig_b5.RData"))
  
  M1_TL  <- loadRData(paste0("../Data/M1_",var,"_baseline_TL.RData"))
  M2_TL <- loadRData(paste0("../Data/M2_",var,"_baseline_TL.RData"))
  
  for (j in 2:6){
  h_name<-paste0("h0")

  tabla[2,j+1]=paste0("[",round(quantile(M1_b2[[h_name]][,j],0.25),2),";",
                          round(quantile(M1_b2[[h_name]][,j],0.75),2),"]")
  tabla[3,j+1]=round(mean(M1_sig2[[h_name]][,j]),2)
      
  tabla[4,j+1]=paste0("[",round(quantile(M1_b3[[h_name]][,j],0.25),2),";",
                          round(quantile(M1_b3[[h_name]][,j],0.75),2),"]")
  tabla[5,j+1]=round(mean(M1_sig3[[h_name]][,j]),2)
      
  tabla[6,j+1]=paste0("[",round(quantile(M1_b4[[h_name]][,j],0.25),2),";",
                          round(quantile(M1_b4[[h_name]][,j],0.75),2),"]")
  tabla[7,j+1]=round(mean(M1_sig4[[h_name]][,j]),2)
      
  tabla[8,j+1]=paste0("[",round(quantile(M1_b5[[h_name]][,j],0.25),2),";",
                          round(quantile(M1_b5[[h_name]][,j],0.75),2),"]")
  tabla[9,j+1]=round(mean(M1_sig5[[h_name]][,j]),2)
  
  h_name<-paste0("h1")
  
  tabla[11,j+1]=paste0("[",round(quantile(M1_b1[[h_name]][,j],0.25),2),";",
                      round(quantile(M1_b1[[h_name]][,j],0.75),2),"]")
  tabla[12,j+1]=round(mean(M1_sig1[[h_name]][,j]),2)
  
  tabla[13,j+1]=paste0("[",round(quantile(M1_b2[[h_name]][,j],0.25),2),";",
                      round(quantile(M1_b2[[h_name]][,j],0.75),2),"]")
  tabla[14,j+1]=round(mean(M1_sig2[[h_name]][,j]),2)
  
  tabla[15,j+1]=paste0("[",round(quantile(M1_b3[[h_name]][,j],0.25),2),";",
                      round(quantile(M1_b3[[h_name]][,j],0.75),2),"]")
  tabla[16,j+1]=round(mean(M1_sig3[[h_name]][,j]),2)
  
  tabla[17,j+1]=paste0("[",round(quantile(M1_b4[[h_name]][,j],0.25),2),";",
                      round(quantile(M1_b4[[h_name]][,j],0.75),2),"]")
  tabla[18,j+1]=round(mean(M1_sig4[[h_name]][,j]),2)
      
  tabla[19,j+1]=paste0("[",round(quantile(M1_b5[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b5[[h_name]][,j],0.75),2),"]")
  tabla[20,j+1]=round(mean(M1_sig5[[h_name]][,j]),2)
  
  h_name<-paste0("h4")
  
  tabla[22,j+1]=paste0("[",round(quantile(M1_b1[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b1[[h_name]][,j],0.75),2),"]")
  tabla[23,j+1]=round(mean(M1_sig1[[h_name]][,j]),2)
  
  tabla[24,j+1]=paste0("[",round(quantile(M1_b2[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b2[[h_name]][,j],0.75),2),"]")
  tabla[25,j+1]=round(mean(M1_sig2[[h_name]][,j]),2)
  
  tabla[26,j+1]=paste0("[",round(quantile(M1_b3[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b3[[h_name]][,j],0.75),2),"]")
  tabla[27,j+1]=round(mean(M1_sig3[[h_name]][,j]),2)
  
  tabla[28,j+1]=paste0("[",round(quantile(M1_b4[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b4[[h_name]][,j],0.75),2),"]")
  tabla[29,j+1]=round(mean(M1_sig4[[h_name]][,j]),2)
  
  tabla[30,j+1]=paste0("[",round(quantile(M1_b5[[h_name]][,j],0.25),2),";",
                       round(quantile(M1_b5[[h_name]][,j],0.75),2),"]")
  tabla[31,j+1]=round(mean(M1_sig5[[h_name]][,j]),2)
      
  tabla[32,j+1]=round(mean((-M1_TL[["h0"]][,j]+M2_TL[["h0"]][,j])/M2_TL[["h0"]][,j]*100),2)
  tabla[33,j+1]=round(mean((-M1_TL[["h1"]][,j]+M2_TL[["h1"]][,j])/M2_TL[["h1"]][,j]*100),2)
  tabla[34,j+1]=round(mean((-M1_TL[["h4"]][,j]+M2_TL[["h4"]][,j])/M2_TL[["h4"]][,j]*100),2)
  }
  write.table(tabla, file = paste0("../Tables/baseline_reg_",var,".txt"), sep = ",", quote = FALSE, row.names = F)
}

