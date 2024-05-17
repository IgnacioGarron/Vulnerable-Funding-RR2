# Correr en versi?n 4.0.2

library(tidyverse)
library(openxlsx) # Convierte formato de fecha de excel
library(readxl) # excel
library(openxlsx) # Convierte formato de fecha de excel
library(readstata13) # sata
library(kableExtra) # html tables
library(zoo) # time series
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
library(aTSA) # ADF
library(ggpubr) # ggarrrange

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
data <- read_excel("../Data/MonnetPuydata_2020.xls", sheet = "Sheet1")
# Genera un vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$qdate) # fechas forma 1
data$date_n <- as.yearqtr(paste0(data$year, "-", data$quarter),format = "%Y-%q") # se crea un vector de fechas adicional

########################################################
# Generar variables siguiendo las recomendaciones de McCracken y Ng(2016,2020)
# Primero, se prueba estacionariedad con ADF
data <- data %>% group_by(country) %>% 
                        mutate(`Real credit growth`=ifs_n_credit/ifs_prices,
                        yield=ifs_n_bonds, 
                        `Real GDP growth`=ifs_r_gdp, 
                        `Stock growth`=ifs_n_shares, 
                        inf=ifs_prices) %>% 
                        select(date,`Real credit growth`,country,`Real GDP growth`,`Stock growth`) # inf, yield

# Test ADF 
complete_credit <-data  %>%   group_by(country) %>%   
  filter(complete.cases(`Real credit growth`,date)) %>% 
  select(date,country,`Real credit growth`) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

#complete_yield <-data  %>%   group_by(country) %>%   
#  filter(complete.cases(yield,date)) %>% 
#  select(date,country,yield) %>% 
#  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_gdp <-data  %>%   group_by(country) %>%   
  filter(complete.cases(`Real GDP growth`,date)) %>% 
  select(date,country,`Real GDP growth`) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

complete_stock <-data  %>%   group_by(country) %>%   
  filter(complete.cases(`Stock growth`,date)) %>% 
  select(date,country,`Stock growth`) %>% 
  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))

#complete_inf <-data  %>%   group_by(country) %>%   
#  filter(complete.cases(inf,date)) %>% 
#  select(date,country,inf) %>% 
#  pivot_longer(names_to = "variable", values_to="value",cols=c(-country,-date))


complete_data<- rbind(complete_credit,complete_gdp,complete_stock)
#complete_yield,complete_inf

#Figura 1

g1<-ggplot(complete_data) + 
  geom_jitter(aes(x=date, y=value, group=variable, color=country), show.legend=FALSE) +
  facet_wrap(~variable, scales = "free_y") + 
  labs(title="a. Untransformed data", x="", y="",caption="Note: Each color represents a country.")+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))


tests1 <- complete_data  %>% 
  group_by(country,variable) %>%
  summarise(n=n(),lag=floor(12*(length(n)/100)^(1/4)),p_adf = adf.test(value, floor(12*(length(n)/100)^(1/4)), output=FALSE)$type3[3,3]) 

#Figura 2

g2<-ggplot(tests1) + 
  geom_point(aes(y=variable, x=p_adf, colour=country), show.legend = FALSE,position = "jitter") +
   geom_vline( xintercept = 0.05) +
  scale_x_continuous(breaks=seq(0, 1, 0.5),limits=c(0, 1)) +
  labs(title="b. ADF tests for untransformed data, p-value", x="", y="",
       caption = paste0("Note: Each color represents a country."))+
  viridis::scale_color_viridis(discrete=TRUE,option="magma")  +
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

#Tabla A1
Tabla1<- complete_data %>% 
  group_by(country,variable) %>% 
  summarise(start1=first(as.yearqtr(date)),end1=last(as.yearqtr(date)),n=n())

write.table(Tabla1, file = "../Tables/TablaA1.txt", sep = ",", quote = FALSE, row.names = F)

## Tabla A2

Credit_to_GDPA <- read_excel("../Data/USdirectinvest.xlsx", sheet = "Credit_to_GDP") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(Credit_to_GDP=mean(value,na.rm = TRUE),N1=sum(!is.na(value)))

Chinn_Ito <- read_excel("../Data/USdirectinvest.xlsx", sheet = "Chinn-Ito") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(MarketCAP_to_GDP=mean(value,na.rm = TRUE),N2=sum(!is.na(value)))

inv_GDP <- read_excel("../Data/USdirectinvest.xlsx", sheet = "inv_GDP") %>% 
  pivot_longer(names_to="year",values_to="value",cols=c(-country,-isocode)) %>% 
  group_by(country) %>% 
  summarise(inv_GDP=mean(value,na.rm = TRUE),N3=sum(!is.na(value)))

TablaA2<-left_join(Credit_to_GDPA,Chinn_Ito,by="country")

TablaA2<-left_join(TablaA2,inv_GDP ,by="country") %>% 
  mutate_each(funs(round(., 2)),-c(country,N2,N1,N3)) 

write.table(TablaA2, file = "../Tables/TablaA2.txt", sep = ",", quote = FALSE, row.names = F)


# Segundo, se realizan las transformaciones

complete_data2<- complete_data  %>% 
  group_by(country) %>% 
  mutate(value = case_when(variable =="Stock growth" ~ log(value)-log(lag(value,1)), # variable =="yield" ~ value-lag(value,1), 
                           variable =="Real credit growth" ~ log(value)-log(lag(value,1)),
                           variable =="Real GDP growth" ~ log(value)-log(lag(value,1))))
                           #variable =="inf" ~ log(value)-2*log(lag(value,1))+log(lag(value,2))))


g3<-ggplot(complete_data2[complete_data2$date>="1960-01-01",]) + 
  geom_jitter(aes(x=date, y=value, group=variable, color=country), show.legend=FALSE) +
  facet_wrap(~variable, scales = "free_y") + 
  labs(title="a. Time series plots (1960-2019)", x="", y="",
       caption="Note: Each color represents a country.")+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))


tests2 <- complete_data2[complete_data2$date>="1960-01-01",]  %>% 
  group_by(country,variable) %>%
  summarise(n=n(),lag=floor(12*(length(n)/100)^(1/4)),p_adf = adf.test(value, floor(12*(length(n)/100)^(1/4)), output=FALSE)$type3[3,3]) 

tests2[tests2$p_adf>=0.05,]

#Figura 2
g4<-ggplot(tests2) + 
  geom_point(aes(y=variable, x=p_adf, colour=country), show.legend = FALSE,position = "jitter") +
  geom_vline( xintercept = 0.05) +
  scale_x_continuous(breaks=seq(0, 1, 0.5),limits=c(0, 1)) +
  labs(title="b. ADF tests, p-value", x="", y="",
       caption = paste0("Note: Each color represents a country."))+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))
  


data_final_long<-complete_data2[complete_data2$date>="1960-01-01",]



# Table A4
  
  TablaA4 <-  data %>%  select(country,date,`Real credit growth`,`Real GDP growth`,`Stock growth`) %>% #inf,yield
    pivot_longer(names_to="variable",values_to="value",cols=-c(date,country)) %>% 
    group_by(country,variable) %>% 
    summarise(Mean=round(mean(value,na.rm = T),2),Sd=round(sd(value,na.rm = T),2),Min=round(min(value,na.rm = T),2)
              ,Max=round(max(value,na.rm = T),2)) 
  
    

write.table(TablaA4, file = "../Tables/TablaA4.txt", sep = ",", quote = FALSE, row.names = F)


ggsave(paste0("../Figures/figA1.png"),
       ggarrange(g3,g4, ncol = 2,nrow = 1), width = 12, height = 6)