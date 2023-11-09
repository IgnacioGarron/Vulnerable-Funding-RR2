library(tidyverse)
library(openxlsx) # Convierte formato de fecha de excel
library(readxl) # excel
library(openxlsx) # Convierte formato de fecha de excel
library(readstata13) # sata
library(kableExtra) # html tables
library(zoo) # time series
library(quantreg) # quantile regressions
library(bannerCommenter) # Baners
#library(fUnitRoots) # unitroots
library(broom)
library(ggpubr) ## ggarrange para combinar 2 o m?s ggplot
library(fredr) # St Luis FED data
library(tis)

########################################################
# Importa datos de consolidados
rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Vulnerable_Funding")
data <- read.csv("Data/Data_final.csv")

# Vector de tiempo a partir de POSIXct
data$date   <-as.Date(data$date) # fechas forma 1


banner("Parte 1:", "Variables", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 1:                               ###
###                              VARIABLES                              ###
###                                                                     ###
###########################################################################
###########################################################################

h<-0
data_reg <- data %>% group_by(country) %>%
  mutate_each(funs = scale,
              credit_z=credit,
              stock_z=stock,
              gdp_z=gdp,
              inf_z=inf,
              yield_z=yield,
              NFCI_z=NFCI,
              EPU_z=EPU,
              iv1=iv1,
              iv2=iv2,
              global_factor=global_factor,
              fin_factor=fin_factor,
              USfin_z=USUN) %>% 
  mutate(stock_h=lead(stock,h),
         credit_h=lead(credit,h))

# US recessions
#1960-01-01, 1960-10-01
#1970-01-01, 1970-07-01

recessions.df = read.table(textConnection(
  "Peak, Trough
  1960-01-01, 1960-10-01
  1970-01-01, 1970-07-01
  1973-10-01, 1975-01-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-10-01
  1990-07-01, 1991-01-01
  2001-01-01, 2001-10-01
  2008-10-01, 2009-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


# Figure 1a

cor(data_reg[data_reg$country=="Bolivia" & data_reg$date>="1971-01-01",c("NFCI_z","USfin_z","iv1","iv2")])


data_plot <-data_reg %>%  filter(country=="Bolivia" ) %>% 
  select(country, date, global_factor,fin_factor,NFCI_z,USfin_z) %>% 
  pivot_longer(names_to = "variables",values_to="values",cols = c(-date,-country))

  
data_plot$variables[data_plot$variables=="global_factor"]<-"Global macroeconomic Factor"
data_plot$variables[data_plot$variables=="fin_factor"]<-"Global financial Factor"
data_plot$variables[data_plot$variables=="NFCI_z"]<-"National Financial Condition Index"
data_plot$variables[data_plot$variables=="USfin_z"]<-"Financial Uncertainty"

g1<-data_plot[data_plot$date>"1959-10-01" ,] %>% 
  ggplot(aes(x=date,y=values, col=variables)) +
  geom_line(size=0.8,position =  position_dodge(0.4)) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  scale_colour_discrete("")+
  scale_colour_manual(values=c("blue","black","orange","purple"))+
  labs(x = "",
       y = "",col="") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0)) 


ggsave(paste0("Figures/FIG_global",".png"),
       ggarrange(g1,ncol = 1), width = 8, height = 4)


data_plot <-data_reg %>%  filter(country=="Bolivia") %>% 
  select(country, date,iv1,iv2,NFCI_z) %>% 
  pivot_longer(names_to = "variables",values_to="values",cols = c(-date,-country))


data_plot$variables[data_plot$variables=="iv1"]<-"First IV real factor"
data_plot$variables[data_plot$variables=="iv2"]<-"Second IV real factor"
data_plot$variables[data_plot$variables=="NFCI_z"]<-"NFCI"

g2<-data_plot[data_plot$date>"1970-10-01" ,] %>% 
  ggplot(aes(x=date,y=values, col=variables)) +
  geom_line(size=0.8,position =  position_dodge(0.4)) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  scale_colour_discrete("")+
  scale_colour_manual(values=c("black","blue", "gray60"))+
  labs(x = "",
       y = "",col="") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave(paste0("Figures/FIG_iv",".png"),
       ggarrange(g2,ncol = 1), width = 8, height = 4)


# Figure 1b

data_plot <-data_reg %>%  filter(country=="Ireland") %>% 
  select(country, date, credit_z,NFCI_z,stock_z) %>% 
  pivot_longer(names_to = "variables",values_to="values",cols = c(-date,-country))



data_plot$variables[data_plot$variables=="stock_z"]<-"Stock returns"
data_plot$variables[data_plot$variables=="credit_z"]<-"Real credit growth"
data_plot$variables[data_plot$variables=="NFCI_z"]<-"NFCI"

data_plot[data_plot$date>"1970-12-01" ,] %>% 
  ggplot(aes(x=date,y=values, col=variables)) +
  geom_line(size=0.8,position =  position_dodge(0.4)) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  scale_colour_discrete("")+
  scale_colour_manual(values=c("blue", "red","darkgreen"))+
  labs(title = "Ireland and US financial conditions",subtitle = "Standarized variables",
       x = "",
       y = "", fill = "",col="",
       caption = paste0("Sources: Chicago National Financial Condition Index (NFCI) and authors' computation. \n",
                        "Note: Time span 1971Q1 to 2019Q4. Red shaded area represents NBER recessions at end of the period.")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0)) +
  ggsave("Figures and heatmap/Figure 1b.png", width = 8, height = 4)






# correlations

data_plot <-data_reg %>%  filter(country=="Bolivia") %>% 
  select(country, date, pc1_fin_z,pc1_2sfin_z,pc1_global_z,pc1_2sglobal_z) 

cor<-cor(data_plot[,3:6]) %>% round(d=2)

write.table(cor, file = "Data/cor.txt", sep = ",", quote = FALSE, row.names = T)

