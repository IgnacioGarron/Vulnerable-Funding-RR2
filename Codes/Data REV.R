# Correr en versi?n 4.0.2

library(tidyverse)
library(openxlsx) # Convierte formato de fecha de excel
library(readxl) # excel
library(openxlsx) # Convierte formato de fecha de excel
library(kableExtra) # html tables
library(zoo) # time series
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
library(xts)
library(nowcasting) #IC Factors
library(ggpubr) # ggarrrange
library(fbi) #FRED-Q


banner("Parte 1:", "Procesamiento de la base de datos", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 1:                               ###
###                  PROCESAMIENTO DE LA BASE DE DATOS                  ###
###                                                                     ###
###########################################################################
###########################################################################



########################################################
# Importa datos de FMI
rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Vulnerable_Funding") # Direcci?n principal
data <- read_excel("Data/MonnetPuydata_2020.xls", sheet = "Sheet1")
# Genera un vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$qdate) # fechas forma 1
data$date_n <- as.yearqtr(paste0(data$year, "-", data$quarter),format = "%Y-%q") # se crea un vector de fechas adicional

########################################################
# Generar variables siguiendo las recomendaciones de McCracken y Ng(2016,2020)
data <- data %>% group_by(country) %>% 
                        mutate(credit=ifs_n_credit/ifs_prices,
                        yield=ifs_n_bonds, 
                        gdp=ifs_r_gdp, 
                        stock=ifs_n_shares, 
                        inf=ifs_prices) %>% 
                        dplyr::select(date,credit,country,yield,gdp,stock,inf)
              
# Test ADF 
complete_credit <-data  %>%   group_by(country) %>%   
  dplyr::filter(complete.cases(credit,date)) %>% 
  dplyr::select(date,country,credit) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_yield <-data  %>%   group_by(country) %>%   
  dplyr::filter(complete.cases(yield,date)) %>% 
  dplyr::select(date,country,yield) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_gdp <-data  %>%   group_by(country) %>%   
  dplyr::filter(complete.cases(gdp,date)) %>% 
  dplyr::select(date,country,gdp) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_stock <-data  %>%   group_by(country) %>%   
  dplyr::filter(complete.cases(stock,date)) %>% 
  dplyr::select(date,country,stock) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_inf <-data  %>%   group_by(country) %>%   
  dplyr::filter(complete.cases(inf,date)) %>% 
  dplyr::select(date,country,inf) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))


complete_data<- rbind(complete_credit,complete_yield,complete_gdp,complete_stock,complete_inf)

# Segundo, se realizan las transformaciones

complete_data2<- complete_data  %>% 
  group_by(country) %>% 
  mutate(value = case_when(variable =="stock" ~ (log(value)-log(lag(value,1)))*100,
                           variable =="yield" ~ (value-lag(value,1))*100, 
                           variable =="credit" ~ (log(value)-log(lag(value,1)))*100,
                           variable =="gdp" ~ (log(value)-log(lag(value,1)))*100,
                           variable =="inf" ~ (log(value)-2*log(lag(value,1))+log(lag(value,2)))*100))



data_final_long<-complete_data2[complete_data2$date>="1960-01-01",]

banner("Parte 2:", "Calcular factores para base Monnet and Puy", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###              CALCULAR FACTORES PARA BASE MONNET AND PUY              ###
###                                                                      ###
############################################################################
############################################################################

  
data_wide_all <- data_final_long %>% 
  pivot_wider(names_from = c("variable","country"),values_from="value",names_sort=TRUE) %>%
  dplyr::select(starts_with(c("credit","stock","yield","gdp","inf"))) %>% 
  mutate_all(.funs = funs(scale(.,)))

  replace_outliers <- function(x, na.rm = TRUE, ...) {
  y=as.matrix(x)
    for (j in 1:ncol(y)){
    qnt <- quantile(y[,j], probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(y[,j], na.rm = na.rm)
    y[y[,j]< (qnt[1] - H),j] <- qnt[1] - H
    y[y[,j]> (qnt[2] + H),j] <- qnt[2] + H
    }
  return(y)
  }

  # Remove outliers
data_wide_all2<-  replace_outliers(data_wide_all )
plot(data_wide_all[,80],t="l")
lines(data_wide_all2[,80],t="l",col=2)

dfm_global<-dynfactoR::dfm(data_wide_all2,r=2,p=2)
 


loadings_global<-as.data.frame(dfm_global$C[,1])
global_factor<-dfm_global$twostep[,1]
loadings_fin<-as.data.frame(dfm_global$C[,2])
#zero restriction for non-financial indicators
loadings_fin[90:174,]<-0

data_sinNA=data_wide_all
for (j in 1:length(data_sinNA)) {
  for (i in 1:240){
    if (is.na(data_sinNA[i,j])){
      data_sinNA[i,j]<-0
  }
}
}

fin_factor<-as.matrix(data_sinNA)%*%as.matrix(loadings_fin)

loadings_fin$names<-names(data_wide_all)
colnames(loadings_fin)<-c("y","names")

loadings_global$names<-names(data_wide_all)
colnames(loadings_global)<-c("y","names")

g1<-loadings_fin %>% arrange(desc(y)) %>% 
  ggplot(data=.,aes(x=names, y=y)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=names, 
                   xend=names, 
                   y=min(y), 
                   yend=max(y)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  aes(x = fct_inorder(names))+
  labs(title="", 
       subtitle="", 
       caption="",x="",y="") +  
  theme_classic()+
  coord_flip()

ggsave(paste0("Figures/FIG1",".png"),
      ggarrange(g1,ncol = 1), width = 6, height = 18)

g2<-loadings_global %>% arrange(desc(y)) %>% 
  ggplot(data=.,aes(x=names, y=y)) + 
  geom_point(col="blue", size=3) +   # Draw points
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=names, 
                   xend=names, 
                   y=min(y), 
                   yend=max(y)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  aes(x = fct_inorder(names))+
  labs(title="", 
       subtitle="", 
       caption="",x="",y="") +  
  theme_classic()+
  coord_flip()

ggsave(paste0("Figures/FIG2",".png"),
       ggarrange(g2,ncol = 1), width = 6, height = 18)

plot(global_factor,t="l")
lines(fin_factor,t="l",col=2)


#write.csv( data, file = "Data/Data_final.csv")

banner("Parte 3:", "FACTOR FRED-QD", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 3:                               ###
###                            FACTOR FRED-QD                            ###
###                                                                      ###
############################################################################
############################################################################

#Bajar datos desde 1960
fredqd<-fredqd("Data/FRED-Q.csv",transform = T)
data_fredqd_all<-data.frame(fredqd[fredqd$date>="1960-01-01" & fredqd$date<="2019-12-01",])
fredqd_desc<-fredqd_description

unique(fredqd_desc$group)


fredqd_des<-fredqd_description %>% 
      mutate(g_nonfin=case_when(group=="NIPA" & sw_factors==1~1,
                             group=="Industrial Production"& sw_factors==1~1,
                             group=="Employment and Unemployment"& sw_factors==1~1,
                             group=="Housing"& sw_factors==1~1,
                             group=="Prices"& sw_factors==1~1,
                             group=="Inventories, Orders, and Sales"& sw_factors==1~1,
                             group=="Earnings and Productivity"& sw_factors==1~1,
                             TRUE~0),
                            variable=fred) %>% 
      select(variable,g_nonfin)



fredqd_long<-data_fredqd_all %>%  pivot_longer(names_to="variable",values_to="value",cols=-date)


data_fredqd_nonfin<-left_join(fredqd_long,fredqd_des,by="variable") %>% filter(g_nonfin==1) %>% arrange(variable) %>% 
 pivot_wider(names_from = "variable",values_from="value")%>% 
  mutate_all(.funs = funs(scale(.)))


data_fredqd_nonfin<-data_fredqd_nonfin[,3:length(data_fredqd_nonfin)]
data_fredqd_nonfin<-replace_outliers(data_fredqd_nonfin)
dfm_fred<-dynfactoR::dfm(data_fredqd_nonfin,r=2,p=2)


loadings_fred1<-as.data.frame(dfm_fred$C[,1])
loadings_fred1$names<-colnames(data_fredqd_nonfin)
colnames(loadings_fred1)<-c("y","names")

g1<-loadings_fred1%>% arrange(desc(y)) %>% 
  ggplot(data=.,aes(x=names, y=y)) + 
  geom_point(col="black", size=3) +   # Draw points
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=names, 
                   xend=names, 
                   y=min(y), 
                   yend=max(y)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  aes(x = fct_inorder(names))+
  labs(title="", 
       subtitle="", 
       caption="",x="",y="") +  
  theme_classic()+
  coord_flip()

loadings_fred1<-as.data.frame(dfm_fred$C[,2])
loadings_fred1$names<-colnames(data_fredqd_nonfin)
colnames(loadings_fred1)<-c("y","names")

g2<-loadings_fred1%>% arrange(desc(y)) %>% 
  ggplot(data=.,aes(x=names, y=y)) + 
  geom_point(col="red", size=3) +   # Draw points
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=names, 
                   xend=names, 
                   y=min(y), 
                   yend=max(y)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  aes(x = fct_inorder(names))+
  labs(title="", 
       subtitle="", 
       caption="",x="",y="") +  
  theme_classic()+
  coord_flip()

ggsave(paste0("Figures/FIG3",".png"),
       ggarrange(g1,g2,ncol = 2), width = 12, height = 16)



plot(dfm_fred$twostep[,1],t="l")
lines(dfm_fred$twostep[,2],t="l")


banner("Parte 4:", "MERGE", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 4:                               ###
###                                MERGE                                 ###
###                                                                      ###
############################################################################
############################################################################

data_final_wide <- complete_data %>% pivot_wider(names_from = c("variable"),values_from="value",names_sort=TRUE)

factors<-cbind(data_final_long[1:240,1],global_factor,fin_factor,dfm_fred$twostep[,1],dfm_fred$twostep[,2])
names(factors)<-c("date","global_factor","fin_factor","iv1","iv2")

data_final_wide <-left_join(data_final_wide,factors,by ="date")

data_final<- data_final_wide %>% mutate(q=substr(quarters(date), 2, 2),year=substr(date,1,4)) %>%
  filter(year>1959)


# Introduciendo el NFCI (Adrian et al.,2009) y EPU (Baker et al.,2016)

US <- read_excel("Data/USconditions.xlsx", sheet = "Data")
US$date<-as.Date(US$date,format = "%Y-%q") # datos desde 1971-01-01
data_final<-left_join(data_final,US,by ="date")


write.csv( data_final, file = "Data/Data_final.csv")



