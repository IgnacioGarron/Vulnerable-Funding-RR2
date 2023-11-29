library(tidyverse)
library(openxlsx) # Convierte formato de fecha de excel
library(readxl) # excel
library(openxlsx) # Convierte formato de fecha de excel
library(kableExtra) # html tables
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
library(broom)
library(QregBB)
library(ggpubr) # ggarrrange
library(corrplot) # corrplot
library(gridGraphics)

########################################################
# Importa datos de consolidados
rm(list = ls()) # Limpiar environment

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#CORRPLOT SAVE
myfun <- function(ff){
  corrplot(as.matrix(ff),cl.ratio = 0.2, tl.srt = 45,
           addCoef.col = 'black',col = COL2('PRGn',10))
  recordPlot() # record the latest plot
}

for (varname in c("credit","stock")){
  # NFCI beta vs NFCI beta GaR, M3
  if (varname == "credit") name_col<-"CaR"
  else name_col<-"EaR"
  h=0
  credit <- loadRData(paste0("../Data/M1_",varname,"_baseline_coef_b2.RData"))
  gdp <- loadRData(paste0("../Data/M1_gdp_baseline_coef_b2.RData"))
  new<-left_join(credit[[paste0("h",h)]][,1:2],gdp[[paste0("h",h)]][,1:2],by="country")
  colnames(new)<-c("h",paste0(name_col,", h=",h),paste0("GaR, h=",h))
  
  for (h in c(1,4,8,12)){
    credit <- loadRData(paste0("../Data/M1_",varname,"_baseline_coef_b2.RData"))
    gdp <- loadRData(paste0("../Data/M1_gdp_baseline_coef_b2.RData"))
    new2<-left_join(credit[[paste0("h",h)]][,1:2],gdp[[paste0("h",h)]][,1:2],by="country")
    colnames(new2)<-c("h",paste0(name_col,", h=",h),paste0("GaR, h=",h))
    new<-cbind(new,new2[,-1])
  }
  
  M<-new %>%  select(-h) %>% 
    do(data.frame(t(cor(.[,], .[,],use = "complete.obs"))))
  
  M<-M[c(1,3,5,7,9),c(2,4,6,8,10)]
  
  colnames(M)<-c("GaR, h=0","GaR, h=1","GaR, h=4","GaR, h=8","GaR, h=12")
  
  

  g1<-myfun(M)
  
   
  ggsave(paste0("../Figures/Scatter_",varname,"_NFCI.jpg"),
         ggarrange(g1,ncol = 1,nrow=1), width = 8, height = 4)
  
}


