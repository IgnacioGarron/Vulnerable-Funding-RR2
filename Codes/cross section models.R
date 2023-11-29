library(ggplot2)
library(openxlsx) # Convierte formato de fecha de excel
library(tidyr) # data import
library(readxl) # excel
library(dplyr) # data manipulation
library(openxlsx) # Convierte formato de fecha de excel
library(kableExtra) # html tables
library(zoo) # time series
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
library(broom)
library(ggpubr) #ggarrane
########################################################
# Importa datos de consolidados
rm(list = ls()) # Limpiar environment

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



# Importa datos para ordenar
#1 US inv, 2 credit to GDP, 3 market CAP to GDP, 4 financial ins., 5 financial market ins.

Sort_1 <- read_excel("../Data/USdirectinvest.xlsx", sheet = "inv_GDP") %>% 
      pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
      group_by(country) %>% 
      summarise(Inv_GDP=mean(value,na.rm = TRUE))
    o_1<-"Inv_GDP" 

Sort_2 <- read_excel("../Data/USdirectinvest.xlsx", sheet = "Credit_to_GDP") %>% 
      pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
      group_by(country) %>% 
      summarise(Credit_to_GDP=mean(value,na.rm = TRUE))
    o_2<-"Credit_to_GDP"

Sort_3<- read_excel("../Data/USdirectinvest.xlsx", sheet = "Chinn-Ito") %>% 
      pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
      group_by(country) %>% 
      summarise(Chinn_Ito=mean(value,na.rm = TRUE))
    o_3<-"Chinn_Ito"

  
banner("Parte 1:", "Cross database", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 1:                               ###
###                            CROSS DATABASE                            ###
###                                                                      ###
############################################################################
############################################################################
  
data_cross<-data.frame(matrix(ncol=7))
names(data_cross)<-c("h","country","Inv_GDP","Credit_to_GDP",
                       "Chinn_Ito","Quantile","Value")
  
varname<-c("credit","stock")
 
for (var in varname){
    
 for (beta in c("b2")){
   
   if (beta=="b2") M_b <- loadRData(paste0("../Data/M1_",var,"_baseline_coef_b2.RData"))
   else M_b <- loadRData(paste0("../Data/M2_",var,"_baseline_coef_b2.RData"))
    
      
  if (var=="credit" & beta=="b2"){
        title="NFCI"
  } else if (var=="credit" & beta=="b3"){
        title="FUI" 
  }else if (var=="stock" & beta=="b2"){
        title="NFCI" 
  }else if (var=="stock" & beta=="b3"){
        title="FUI" 
  }
   
    for (h in c(0,1,4,8,12)){
      
      #merge inv
      modelc<-left_join(M_b[[paste0("h",h)]],Sort_1,by="country")
      modelc<-left_join(modelc,Sort_2,by="country")
      modelc<-left_join(modelc,Sort_3,by="country")

      modelc<-modelc %>% 
          pivot_longer(names_to="Quantile",values_to="Value",
                       cols=c(-country,-c(o_1,o_2,o_3))) 
        
      modelc<-cbind("h"=rep(h,nrow(modelc)),modelc)
        
      data_cross<-rbind(data_cross,modelc)
      
    }
  
  write.csv(data_cross[-1,], file = paste0("../Data/Cross_",var,"_",title,".csv"))  
  
  model<-data_cross[-1,] %>% 
    group_by(Quantile,h) %>% 
    do(tidy(estimatr::lm_robust(Value ~Inv_GDP+Credit_to_GDP+
                                  Chinn_Ito,data=.,se_type = "stata"))) %>% 
    select(Quantile,h,term,estimate,p.value) %>% 
    mutate(estimate=round(estimate, 3))  %>% 
    mutate(sig=case_when(p.value<0.01~"***",
                         p.value<0.05~"**",
                         p.value<0.1~"*",
                         TRUE~"")) %>%
    unite("coef",c(estimate,sig),remove=FALSE,sep="") %>% 
    select(h,term, coef,Quantile) %>% 
    #  unite("varname",c(h,term),remove=FALSE,sep="") %>% 
    #  select(varname, coef,Quantile) %>% 
    pivot_wider(names_from =Quantile,values_from=coef,id_cols=c(h,term)) 
  
  write.table(model, file = paste0("../Tables/Cross_",var,"_",title,".txt"),
              sep = ",", quote = FALSE, row.names = F)  
  
 }

 }


 