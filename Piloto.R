### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Campos/Piloto/Prainha')
library(readxl)
library(viridis)
library(dplyr)
library(ggplot2)
#install.packages('esquisser') #atualizar versão do R
#library(esquisser)

ago2022 <- read_excel('C:/Users/marin/Documents/Mestrado/Projeto/Mestrado/Ago2022.xlsx',
                      sheet = 'Planilha1') #chamando dataframe

ago2022$data <- as.Date(ago2022$data, format = "%d_%m_%Y") #transformando data em data
str(ago2022$data)
ago2022$ID <- as.character(ago2022$ID) #transformando ID em caracter
str(ago2022$ID)
ago2022$tamanho_mm <- as.numeric(ago2022$tamanho_mm) #transformando tamanho em numérico
str(ago2022$tamanho_mm)

#Renomear colunas
names(ago2022) <- c('ID', 'cor', 'data', 'tamanho_mm')

#Marcação e Recaptura
freq_data<- count(ago2022, data) #contando frequência das datas

freq_data %>%
  ggplot(aes(x=data, y=n)) + 
  geom_bar(stat = "identity") #plotando gráfico de barra com o número de capturados por dia


#Tamanho ao longo do tempo
ago2022 %>%
  filter(ago2022$cor == "azul",
         !tamanho_mm %in% NA) %>% 
  ggplot(aes(x=data, y=tamanho_mm)) +
  geom_point(size = 1) +
  geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_bw()


