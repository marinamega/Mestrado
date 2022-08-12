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

teste<-mbon_p2p_jul2022[(1:100),]

for(i in mbon_p2p_jul2022$type_cover) {mbon_p2p_jul2022 %>% 
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
temp<- read.csv('temp_mesolitoral_2019-2021.csv', header = T, sep = ",", dec = ",") #chamando dataframe

#ajustando data
temp<- temp %>% separate(time, c('ano', 'mes', 'dia', 'hora', 'min', 'seg'))
temp$date <- paste(temp$dia,temp$mes, temp$ano, sep = "_")
temp<- temp[-(3:5)]

#Mudando ordem das colunas
temp <- temp[ ,c(1,2,9,3,4,5,6,7,8)]

#Alterações nas características dos dados
temp$temp <- as.numeric(temp$temp) #transformando em numérico
temp$date <- as.Date(temp$date, format = "%d_%m_%Y") #transformando em data
str(temp$date)

#Ajustando nome dos sensores e excluindo NA's
temp$sensor <- sub(" Fortshade", "teste", temp$sensor)
temp$sensor <- sub("Fortshade", "shade", temp$sensor)
temp$sensor <- sub("teste", "shade", temp$sensor)
temp$sensor <- sub(" Fortsun", "sun", temp$sensor)
temp <- mutate_at(temp, c("sensor"), ~replace(., is.na(.), 'sun'))

unique(temp$sensor)

temp %>%
mutate(date = gsub("_", "-", date) %>%
       sensor = factor(sensor, levels = c("shade", "sun"))) %>%
  ggplot(temp, aes(x = date, y = temp, group = date)) +
  geom_point(aes(color=sensor))+
  facet_wrap(~ sensor) +
  geom_boxplot(shape=21, color="black", fill="#69b3a2", outlier.shape = NA) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

