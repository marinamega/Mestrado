library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)

setwd('C:/Users/marin/Documents/Mestrado/Projeto')
#lsub <- mbon_p2p_Aug2021[mbon_p2p_Aug2021$motile=="Lottia subrugosa", ]
#lsub <- lsub[!is.na(lsub$motile),]

## Editando Data Frame ##
lsub<- read.csv('lsub_mean.csv', header = T, sep = ";", dec = ",")
lsub <- na.omit(lsub)

names(lsub) <- c('date', 'site', 'tide_height', 'density')
#lsub$species <- sub("Lottia subrugosa", "Lottia_subrugosa", lsub$species)
lsub$date <- sub("/", "_", lsub$date)
lsub$date <- sub("/", "_", lsub$date)

## Separando por site ##
prainha<- lsub[lsub$site == "Prainha", ]
fortaleza<- lsub[lsub$site == "Fortaleza", ]
atalaia<- lsub[lsub$site == "Atalaia", ]


## PRAINHA ##
## Separando em faixas 
p_low<- prainha[prainha$tide_height == "low", ]
p_high<- prainha[prainha$tide_height == "high", ]
p_mid<- prainha[prainha$tide_height == "mid", ]

## Flutuações ao longo do tempo

# Plot
p_low %>%
  tail(100) %>%
  ggplot( aes(x=date, y=density)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Prainha Low")
  

p_mid %>%
  tail(100) %>%
  ggplot( aes(x=date, y=density)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Prainha Mid")

p_high %>%
  tail(100) %>%
  ggplot( aes(x=date, y=density)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Prainha High")


lsub %>% 
  ggplot(aes(x=date, y=as.numeric(density))) +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    facet_grid(tide_height ~ site) +
    theme_classic()
  

######################

#Densidade Lottia por tempo
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
    #geom_jitter(size = 0.5, alpha = 0.5) +
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

#### Temperatura por tempo
#### RUIM
temp<- read.csv('temp_mesolitoral_2019-2021.csv', header = T, sep = ",", dec = ",")
temp<- temp %>% separate(time, c('ano', 'mes', 'dia', 'hora', 'min', 'seg'))
temp$date <- paste(temp$dia,temp$mes, temp$ano, sep = "_")
temp<- temp[-(3:5)]
#temp_ok<- temp[-(4:5)]
#Mudar sequÃªncia
temp <- temp[ ,c(1,2,9,3,4,5,6,7,8)]
temp$temp <- as.numeric(temp$temp)
temp$date <- as.Date(temp$date, format = "%d_%m_%Y")
str(temp$date)
temp$sensor <- sub(" Fortshade", "Fortshade", temp$sensor)
temp$sensor <- sub(" Fortsun", "Fortsun", temp$sensor)
temp$sensor <- sub(" Fortshade", "Fortshade", temp$sensor)

temp %>%
mutate(date = gsub("_", "-", date) %>%
       sensor = factor(sensor, levels = c("Fortsun", "Fortshade", "NA"))) %>%
  ggplot(temp, aes(x = date, y = temp, group = date)) +
  geom_boxplot(shape=21, color="black", fill="#69b3a2") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
