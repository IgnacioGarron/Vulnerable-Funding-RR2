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
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Vulnerable_Funding")
data <- read.csv("Data/Data_final.csv")


##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$date) # fechas forma 1



# Importa datos para ordenar
 #1 US inv, 2 credit to GDP, 3 market CAP to GDP, 4 financial ins., 5 financial market ins.

for (order in c(1,2,6)){

if(order==1){
Sort <- read_excel("Data/USdirectinvest.xlsx", sheet = "inv_GDP") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Sort=mean(value,na.rm = TRUE))
ylab<-"Countries ordered by U.S. Investment relative to country's GDP"
o<-"inv_GDP" 
} else if(order==2){
Sort <- read_excel("Data/USdirectinvest.xlsx", sheet = "Credit_to_GDP") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Sort=mean(value,na.rm = TRUE))
ylab<-"Countries ordered by country's credit to GDP"
o<-"Credit_to_GDP"
} else if(order==3){
Sort  <- read_excel("Data/USdirectinvest.xlsx", sheet = "MarketCAP_to_GDP") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Sort=mean(value,na.rm = TRUE))
ylab<-"Countries ordered by country's market CAP to GDP"
o<-"MarketCAP_to_GDP"
} else if(order==4){
  Sort<- read_excel("Data/USdirectinvest.xlsx", sheet = "fin_depth") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Sort=mean(value,na.rm = TRUE))
ylab<-"Countries ordered by country's Financial Institutions Depth Index"
o<-"fin_depth"
} else if(order==5){
Sort<- read_excel("Data/USdirectinvest.xlsx", sheet = "market_depth") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Sort=mean(value,na.rm = TRUE))
ylab<-"Countries ordered by country's Financial Markets Depth Index"
o<-"market_depth"
} else if(order==6){
  Sort<- read_excel("Data/USdirectinvest.xlsx", sheet = "Chinn-Ito") %>% 
    pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
    group_by(country) %>% 
    summarise(Sort=mean(value,na.rm = TRUE))
  ylab<-"Countries ordered by country's Chinn-Ito Index"
  o<-"Chinn-Ito"
}else{
 print("error") 
}


banner("Parte 1:", "heat map para credit", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 1:                               ###
###                         HEAT MAP PARA CREDIT                         ###
###                                                                      ###
############################################################################
############################################################################
varname<-c("credit","stock")
us_shock<-c("NFCI","FUI")

for (var in varname){

for (us in us_shock){

if (us=="NFCI") model<-"M1"
if (us=="FUI") model<-"M2"
  
M_b <- loadRData(paste0("Data/",model,"_",var,"_factor_coef_b2.RData"))
M_sig <- loadRData(paste0("Data/",model,"_",var,"_factor_sig_b2.RData"))


# if (var=="credit" & us=="NFCI"){
#   title="NFCI coefficients for real credit growth"
# } else if (var=="credit" & beta=="FUI"){
#   title="FUI coefficients for real credit growth" 
# }else if (var=="stock" & beta=="NFCI"){
#   title="NFCI coefficients for stock prices growth" 
# }else if (var=="stock" & beta=="FUI"){
#   title="FUI coefficients for stock prices growth" 
# }
title<-""


for (h in c(0,1,4,8,12)){
  
#merge inv
modelc<-left_join(M_b[[paste0("h",h)]],Sort,by="country")
modelc$country <- factor(modelc$country, levels=(modelc$country)[order(modelc$Sort)])

temp1<-modelc %>% arrange(Sort) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country,-Sort))

#gather min  and max
minmax<- modelc %>% arrange(Sort) %>% 
           pivot_longer(names_to="variable",values_to="value",cols=c(-country,-Sort)) %>% 
           mutate(tot=1) %>% group_by(tot) %>% 
           summarise(min=min(value),max=max(value))

if (minmax$min<=-1.4) print(paste0("min está mal =",minmax$min))
if (minmax$max>=1.2) print(paste0("max está mal=",minmax$max))

f1<-modelc %>% arrange(Sort) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country,-Sort)) %>% 
  ggplot(aes(x=variable, y=country, fill= value)) +
  geom_tile() +
  scale_fill_gradient2(name = "Coefficients", low = "red", high = "darkblue", mid = "white", 
                       midpoint =0, limits=c(-1.4,1.2), na.value="transparent")+
  ylab("") +
  xlab(expression("Quantiles"~tau)) + 
  labs(title=paste0("Horizon h=",h),
       caption = "") +
  scale_x_discrete(labels=c('0.05', '0.25', '0.50', '0.75', '0,95'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text( hjust = 1),
        plot.title = element_text(size=10))



# HEATMAP significant
models<-left_join(M_sig[[paste0("h",h)]],Sort,by="country")
models$country <- factor(modelc$country, levels=(modelc$country)[order(modelc$Sort)])

temp2<-models %>% arrange(Sort) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country,-Sort))

models_join<-left_join(temp1,temp2,by=c("country","Sort","variable"))


f2<-models_join %>%
  mutate(value=case_when(value.y == 1 & value.x < 0 ~ 'Sig. and (-) coefficient',
                         value.y == 0 ~ 'Not Sig.',
                         value.y == 1 & value.x >= 0 ~ 'Sig. and (+) coefficient')) %>% 
  ggplot(aes(x=variable, y=country, fill= factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("white","red",'darkblue'),name="Coefficients")+
  ylab("") +
  xlab(expression("Quantiles"~tau)) + 
  labs(title=paste0("Horizon h=",h),
       caption = "") +
  scale_x_discrete(labels=c('0.05', '0.25', '0.50', '0.75', '0,95'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text.y = element_text( hjust = 1))

if (h==0)  g1_1<-f1
if (h==1)  g1_2<-f1
if (h==4)  g1_3<-f1
if (h==8)  g1_4<-f1
if (h==12) g1_5<-f1

if (h==0)  g2_1<-f2
if (h==1)  g2_2<-f2
if (h==4)  g2_3<-f2
if (h==8)  g2_4<-f2
if (h==12) g2_5<-f2
  

#           ggsave(paste0("Figures/heatmap_",var,"_",us,"_h=",h,"_o=",o,".png"), 
#                  ggarrange(f1,f2,ncol=2),width = 9, height = 6)
}

ggsave(paste0("Figures/heatmap_coeff_",var,"_",us,"o=",o,".png"), 
      ggarrange(g1_1,g1_2,g1_3,
                g1_4,g1_5,
                nrow=1,common.legend = T,legend = "bottom"),
                width = 12, height = 6)


ggsave(paste0("Figures/heatmap_sig_",var,"_",us,"o=",o,".png"), 
       ggarrange(g2_1,g2_2,g2_3,
                 g2_4,g2_5,
                 nrow=1,common.legend = T,legend = "bottom"),
       width = 12, height = 6)

}
}
}