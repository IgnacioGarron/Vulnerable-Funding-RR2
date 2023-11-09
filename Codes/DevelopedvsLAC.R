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


#EME_credit<-c("Argentina","Bolivia","Brazil",  "Chile",        "Colombia",       "Costa Rica",
#              "Guatemala"  ,    "Honduras",   "India",  "Mexico",
#              "Pakistan",       "Peru",         "Philippines", "Malaysia",       "Morocco",
#              "Thailand",       "Turkey", "Uruguay","South Africa"  )

EME_credit<-c("Argentina","Bolivia","Brazil",  "Chile",        "Colombia",       "Costa Rica",
              "Guatemala"  ,    "Honduras",   "Mexico",
               "Peru","Uruguay" )


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

tabla<-data.frame(matrix(nrow=4,ncol=7))
tabla[,1]<-c("LAC","","Developed","")
tabla[,2]<-c("IQR","Sig.","IQR","Sig.")
colnames(tabla)<-c("","","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")


for (var in c("credit")){
  
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
  
  for (varcoef in c("NFCI","FUI")){
    
    tabla_f<- data.frame()
    
    if (varcoef == "NFCI"){
      M_b <- loadRData(paste0("Data/M1_",var,"_factor_coef_b2.RData"))
      M_sig <- loadRData(paste0("Data/M1_",var,"_factor_sig_b2.RData"))
    } else if (varcoef == "FUI"){
      M_b <- loadRData(paste0("Data/M2_",var,"_factor_coef_b2.RData"))
      M_sig <- loadRData(paste0("Data/M2_",var,"_factor_sig_b2.RData"))
    }
    
    for (h in c("0","1","4","8","12")){
      h_name=paste0("h",h)
      for (j in 2:6){
        #M3
        
        s1_b2<-left_join(sort1,M_b[[h_name]],by="country")
        s1_sig2<-left_join(sort1,M_sig[[h_name]],by="country")
        
        s2_b2<-left_join(sort2,M_b[[h_name]],by="country")
        s2_sig2<-left_join(sort2,M_sig[[h_name]],by="country")
        
        tabla[1,j+1]=paste0("[",round(quantile(s1_b2[,j],0.25),2),";",
                            round(quantile(s1_b2[,j],0.75),2),"]")
        tabla[2,j+1]=round(mean(s1_sig2[,j]),2)
        
        tabla[3,j+1]=paste0("[",round(quantile(s2_b2[,j],0.25),2),";",
                            round(quantile(s2_b2[,j],0.75),2),"]")
        tabla[4,j+1]=round(mean(s2_sig2[,j]),2)
        
      }
      tabla_f<-rbind(tabla_f,tabla)
    }
    write.table(tabla_f, file = paste0("Tables/DEVvsEME_reg_factor_",var,"_",varcoef,"_.txt"), sep = ",", quote = FALSE, row.names = F)
  }

}

