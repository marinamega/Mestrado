library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto')

### DENSIDADE/COBERTURA AO LONGO DO TEMPO ###
### Densidade Lottia ###
mbon_p2p_jul2022 %>% 
  filter(motile == "Lottia subrugosa",
         !density_025m2 %in% c("?", "X")) %>% 
  mutate(density_025m2 = as.numeric(density_025m2),
         data = gsub("_", "-", data) %>% 
           as.POSIXct(format="%d-%m-%Y") %>% 
           as.Date(format = "%m-%Y"),
         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
  # distinct(density_025m2) %>% pull()
  ggplot(aes(x=data, y=density_025m2, group = data)) +
    geom_boxplot(shape=21, color="black", fill="#69b3a2") +
    ggtitle("Lottia subrugosa") +
    #geom_jitter(size = 0.5, alpha = 0.5) +
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))


### Cobertura Tetraclita stalactifera ###
mbon_p2p_jul2022 %>% 
  filter(type_cover == "Tetraclita stalactifera",
         !relative_cover %in% c("?", "X")) %>% 
  mutate(relative_cover = as.numeric(relative_cover),
         data = gsub("_", "-", data) %>% 
           as.POSIXct(format="%d-%m-%Y") %>% 
           as.Date(format = "%m-%Y"),
         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
  # distinct(density_025m2) %>% pull()
  ggplot(aes(x=data, y=relative_cover, group = data)) +
  geom_boxplot(shape=21, color="black", fill="#69b3a2") +
  ggtitle("Tetraclita stalactifera") +
  #geom_jitter(size = 0.5, alpha = 0.5) +
  facet_grid(tideHeight ~ locality ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))



####### tentando fazer loop
########## ERRO

teste<-mbon_p2p_jul2022[(1:500),]

for(i in teste$type_cover) {teste %>% 
    filter(!relative_cover %in% c("?", "X")) %>% 
    mutate(relative_cover = as.numeric(relative_cover),
           data = gsub("_", "-", data) %>% 
             as.POSIXct(format="%d-%m-%Y") %>% 
             as.Date(format = "%m-%Y"),
           tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
    ggplot(aes(x=data, y=relative_cover, group = data)) +
    geom_boxplot(shape=21, color="black", fill="#69b3a2") +
    ggtitle(i) +
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))}


### TEMPERATURA AO LONGO DO TEMPO ###
library(lubridate)
library(scales)
library(heatwaveR)

temp<- read.csv('temp_mesolitoral_2019-2021.csv', header = T, sep = ",", dec = ",") #chamando dataframe
temp$time<- as.POSIXct(temp$time)  # o tamanho vai pra 0 B, não sei pq...
str(temp$time)

#ajustando data e hora
#temp<- temp %>% separate(time, c('ano', 'mes', 'dia', 'hora', 'min', 'seg'))
#temp$date <- paste(temp$dia,temp$mes, temp$ano, sep = "_")
#temp<- temp[-(3:5)]
#temp$hora <- paste(temp$hora,temp$min, temp$seg, sep = ":")
#temp<- temp[-(4:5)]

#Mudando ordem das colunas
#temp <- temp[ ,c(1,2,9,3,4,5,6,7,8)]

#Alterações nas características dos dados
temp$temp <- as.numeric(temp$temp) #transformando em numérico
#temp$date <- as.Date(temp$date, format = "%d_%m_%Y") #transformando em data
#str(temp$date)

#Ajustando nome dos sensores e excluindo NA's
temp$sensor <- sub(" Fortshade", "teste", temp$sensor)
temp$sensor <- sub("Fortshade", "shade", temp$sensor)
temp$sensor <- sub("teste", "shade", temp$sensor)
temp$sensor <- sub(" Fortsun", "sun", temp$sensor)
temp <- mutate_at(temp, c("sensor"), ~replace(., is.na(.), 'sun'))

unique(temp$sensor)

#Calculando o 90º percentil
quantile(temp$temp, probs = 0.9) #sem separar por estações
perc_anual<- quantile(temp$temp, probs = 0.9)

#Tem que ter 3 anos de temperatura para calcular o threshold
#ts2clm(
#  temp,
#  x = date,
#  y = temp,
#  climatologyPeriod = c("2019-08-05", "2021-08-25"),
#  robust = FALSE,
#  maxPadLength = FALSE,
#  windowHalfWidth = 5,
#  pctile = 90,
#  smoothPercentile = TRUE,
#  smoothPercentileWidth = 31,
#  clmOnly = FALSE,
#  var = FALSE,
#  roundClm = 4)

#queria separar por estações para ver o 90º percentil apenas durante o verão. Ou não?
#tem que ver se em algum momento teve acima do 90º percentil por mais de 5 dias.

#Plotando ao longo do tempo
temp %>%
  mutate(date = gsub("_", "-", date) %>%
  sensor = factor(sensor, levels = c("shade", "sun"))) %>%
  ggplot(temp, aes(x = date, y = temp, group = date)) +
  geom_point(aes(color=sensor))+
  facet_wrap(~ sensor) +
  geom_boxplot(shape=21, color="black", fill="#69b3a2", outlier.shape = NA) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))




#não to conseguindo filtrar apenas pela data, leva em conta o horário também
#temp %>%
 # mutate(sensor = factor(sensor, levels = c("shade", "sun"))) %>%
  ##(aes(x=time, y=temp, group = time)) +
  #ggplot(temp, aes(x = time, y = temp, group = time)) +
  #geom_point(aes(color=sensor))+
  #facet_wrap(~ sensor) +
  #geom_boxplot(shape=21, color="black", fill="#69b3a2", outlier.shape = NA) +
  #theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) +
  #scale_x_datetime(labels = date_format("%d-%m-%Y"))

#teste<- temp[(1:2000),]
#format(teste$time,"%Y-%m-%d")

#teste %>%
 # filter(format(teste$time,"%Y-%m-%d")) %>% 
#  format(as.POSIXct(time, format = "%Y-%m-%d")) %>% 
 # mutate(sensor = factor(sensor, levels = c("shade", "sun"))) %>%
  #ggplot(aes(x=time, y=temp, group = time)) +
  #geom_point(aes(color=sensor))+
  #facet_wrap(~ sensor) +
  #geom_boxplot(shape=21, color="black", fill="#69b3a2", outlier.shape = NA) +
  #theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) +
  #scale_x_datetime(labels = label_date("%Y-%m-%d"))

 #str(temp$time)
  
# Plotando temperatura ao longo do dia
# separar por época do ano
#temp %>%
 # filter(!format(date,"%m") %in% c("04", "05", '06', '07', '08', '09', '10', '11')) %>% 
#  mutate(date = gsub("_", "-", date) %>%
 #          sensor = factor(sensor, levels = c("shade", "sun"))) %>%
#  ggplot(temp, aes(x = hora, y = temp, group = hora)) +
 # geom_point(aes(color=sensor))+
#  facet_wrap(~ sensor) +
 # geom_boxplot(shape=21, color="black", fill="#69b3a2", outlier.shape = NA) +
  #theme_classic() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

