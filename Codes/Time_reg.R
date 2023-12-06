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
library(lmtest)
library(sandwich)
########################################################
# Importa datos de consolidados
rm(list = ls()) # Limpiar environment

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

for (var in c("stock","credit")){

Y.credit <- loadRData(paste0("../Data/M1_",var,"_baseline_Pred.RData"))
Y.gdp <- loadRData(paste0("../Data/M1_gdp_baseline_Pred.RData"))

Y1<-cbind("Time"=1:187,Y.gdp[["h1"]]) %>% pivot_longer(names_to = "country",values_to = "GaRt+1",cols=-"Time")
X1<-cbind("Time"=1:187,Y.credit[["h0"]][-188,]) %>% pivot_longer(names_to = "country",values_to = "CaRt",cols=-"Time")
X2<-cbind("Time"=1:187,Y.gdp[["h0"]][-188,]) %>% pivot_longer(names_to = "country",values_to = "GaRt",cols=-"Time")

data_reg<-left_join(Y1,X1,by = c("country","Time"))
data_reg<-left_join(data_reg,X2,by = c("country","Time"))

Y1<-cbind("Time"=1:184,Y.gdp[["h4"]]) %>% pivot_longer(names_to = "country",values_to = "GaRt+4",cols=-"Time")
X1<-cbind("Time"=1:184,Y.credit[["h0"]][-188:-185,]) %>% pivot_longer(names_to = "country",values_to = "CaRt",cols=-"Time")
X2<-cbind("Time"=1:184,Y.gdp[["h0"]][-188:-185,]) %>% pivot_longer(names_to = "country",values_to = "GaRt",cols=-"Time")

data_reg4<-left_join(Y1,X1,by = c("country","Time"))
data_reg4<-left_join(data_reg4,X2,by = c("country","Time"))

Y1<-cbind("Time"=1:180,Y.gdp[["h8"]]) %>% pivot_longer(names_to = "country",values_to = "GaRt+8",cols=-"Time")
X1<-cbind("Time"=1:180,Y.credit[["h0"]][-188:-181,]) %>% pivot_longer(names_to = "country",values_to = "CaRt",cols=-"Time")
X2<-cbind("Time"=1:180,Y.gdp[["h0"]][-188:-181,]) %>% pivot_longer(names_to = "country",values_to = "GaRt",cols=-"Time")

data_reg8<-left_join(Y1,X1,by = c("country","Time"))
data_reg8<-left_join(data_reg8,X2,by = c("country","Time"))

Y1<-cbind("Time"=1:176,Y.gdp[["h12"]]) %>% pivot_longer(names_to = "country",values_to = "GaRt+12",cols=-"Time")
X1<-cbind("Time"=1:176,Y.credit[["h0"]][-188:-177,]) %>% pivot_longer(names_to = "country",values_to = "CaRt",cols=-"Time")
X2<-cbind("Time"=1:176,Y.gdp[["h0"]][-188:-177,]) %>% pivot_longer(names_to = "country",values_to = "GaRt",cols=-"Time")

data_reg12<-left_join(Y1,X1,by = c("country","Time"))
data_reg12<-left_join(data_reg12,X2,by = c("country","Time"))

if (var=="stock") {x_name="EaR(t)"
} else { x_name="CaR(t)"}

g1<-data_reg %>% group_by(Time) %>% mutate(`GaRt+1`=mean(`GaRt+1`),CaRt=mean(CaRt,na.rm = T)) %>% 
  ggplot(aes(y=`GaRt+1`,x=CaRt)) + geom_smooth(method = "lm") +geom_point(alpha=1/10)+
  ylab("GaR(t+1)")+xlab(x_name)

g2<-data_reg %>% group_by(Time) %>% mutate(`GaRt+1`=mean(`GaRt+1`),GaRt=mean(GaRt,na.rm = T)) %>% 
  ggplot(aes(y=`GaRt+1`,x=GaRt)) + geom_smooth(method = "lm") +geom_point(alpha=1/10)+
  ylab("GaR(t+1)")+xlab("GaR(t)")


ggsave(paste0("../Figures/Time_series",var,".jpg"),
       ggarrange(g1,g2,ncol = 2,nrow=1), width = 8, height = 4)


mdls <- list(
fit1 = lm(`GaRt+1`~CaRt+GaRt, data=data_reg),# Set up the model
fit2 = lm(`GaRt+4`~CaRt+GaRt, data=data_reg4),# Set up the model
fit3 = lm(`GaRt+8`~CaRt+GaRt, data=data_reg8),# Set up the model
fit4 = lm(`GaRt+12`~CaRt+GaRt, data=data_reg12)# Set up the model
)

# Calculate robust confidence intervals
se_robust <- function(x)
  coeftest(x, vcov.=NeweyWest(x, lag=0, adjust=TRUE, verbose=TRUE))[, "Std. Error"]


stargazer::stargazer(
  mdls, type = "html", single.row = F, 
  se = lapply(mdls, se_robust),out=paste0("../Tables/time_reg",var,".htm"))

}



  
  
  33.46+100.51+41.54+200.95+145.32+65.03+81.76+144.01+218.93+247.30+202.97+376.10/2+39.17+
    152.85+177.18+179.83+161.95*7.5+204.29+788.10+33.51+7.5+54.02+260.12+476.82+15+8.21+15+300
  
  5596.055 # IGNACIO
  376.10/2+103.21+100  #PAPA
  3677.86-300=3377.86 #ADOLFO
  377.47*7.5=2831.025 #MAMA
  12196.2
  5596.055+377.47*7.5+3377.86+376.10/2+103.21+100
