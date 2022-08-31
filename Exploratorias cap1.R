library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(patchwork)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto')

mbon_p2p_jul2022 <- readxl::read_xlsx("mbon_p2p_jul2022.xlsx") #chamando dataframe

### VERIFICANDO OS GRUPOS MAIS REPRESENTATIVOS ###

#verificando se não há repetições/erros de digitação
unique(mbon_p2p_jul2022$type_cover)
unique(mbon_p2p_jul2022$motile)

plotcov<- mbon_p2p_jul2022 %>%
  filter(relative_cover >= 50) %>% ##coloquei 50 mas no gráfico o eixo y ta estranho
ggplot(aes(x=type_cover, y=relative_cover)) + 
  geom_bar(stat = "identity") +
  facet_grid(tideHeight ~ locality ) +
  theme(axis.text.x = element_text(angle = 90))

plotcov #plotando as coberturas mais representativas 

plotmot<- mbon_p2p_jul2022 %>% 
  filter(density_025m2 >= 50, ##coloquei 50 mas no gráfico o eixo y ta estranho
         !density_025m2 %in% NA) %>%
  ggplot(aes(x=motile, y=density_025m2)) + 
  geom_bar(stat = "identity") +
  facet_grid(tideHeight ~ locality ) +
  theme(axis.text.x = element_text(angle = 90))

plotmot #plotando os móveis mais representativos

plotcov + plotmot #unificando plots em um só

#mais abundantes
cover <- c("Amphiroa", "bare rock", "Chthamalus bisinuatus", "Jania", "Lithophyllum", "Mytilaster solisianus", "Tetraclita stalactifera", "Ulva fasciata", "Willeella brachyclados")
motile <- c("Echinolittorina lineolata", "Fissurella rosea", "Lottia subrugosa", "Stramonita haemastoma")

### DENSIDADE/COBERTURA AO LONGO DO TEMPO ###
#cobertura
for(i in cover) {
  a <- mbon_p2p_jul2022 %>% 
    filter(!relative_cover %in% c("?", "X"),
           type_cover == i) %>% 
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
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) 
  print(a)
  }

#juntando
#como fazer para juntar? estão no loop... teria que salvar cada um do loop como objeto 
# DA INTERNET: saber como aplicar pros meus dados
#mylist <- list()
#mtcars_short <- mtcars[1:5,]
#for(i in 1:nrow(mtcars_short)){
#  mydf <- mtcars[i,]
#  p <- ggplot(mydf, aes(x=mpg, y=carb)) + geom_point()
  
#  mylist[[i]] <- p
#}

#myplot <- patchwork::wrap_plots(mylist, nrow=1)
#nb_plots <- length(mylist)
#basewidth <- grDevices::dev.size(units = "px")[1]
#ggsave("test.png",
#       width = nb_plots*basewidth,
#       units = "px")

#móveis
for(e in motile) {
  b <- mbon_p2p_jul2022 %>% 
    filter(!density_025m2 %in% c("?", "X"),
           !motile %in% NA,
           motile == e) %>% 
    mutate(density_025m2 = as.numeric(density_025m2),
           data = gsub("_", "-", data) %>% 
             as.POSIXct(format="%d-%m-%Y") %>% 
             as.Date(format = "%m-%Y"),
           tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
    ggplot(aes(x=data, y=density_025m2, group = data)) +
    geom_boxplot(shape=21, color="black", fill="#69b3a2") +
    ggtitle(e) +
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) 
  print(b)
}


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
str(temp$temp)

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

