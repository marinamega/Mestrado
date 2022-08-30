### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Campos/Piloto/Prainha')
library(readxl)
library(viridis)
library(dplyr)
library(ggplot2)

ago2022 <- read_excel('C:/Users/marin/Documents/Mestrado/Projeto/Mestrado/Ago2022.xlsx',
                      sheet = 'Planilha1')

ago2022$data <- as.Date(ago2022$data, format = "%d_%m_%Y")
str(ago2022$data)
ago2022$ID <- as.character(ago2022$ID)
str(ago2022$ID)

#Renomear colunas
names(ago2022) <- c('ID', 'cor', 'data', 'tamanho_mm')

#Marcação e Recaptura
MR<- table(ago2022$data)
plot(MR)  #fazer um plot melhor, separando por cor também

#Tamanho ao longo do tempo
ago2022 %>%
  filter(ago2022$cor == "azul",
         !tamanho_mm %in% NA) %>% 
  ggplot(aes(x=data, y=tamanho_mm)) +
  geom_point(size = 1) +
  geom_path(aes(group = ID), color="gray79") +
  theme_bw()

