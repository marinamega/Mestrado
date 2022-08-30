### DIRETÃ“RIO ###
setwd('C:/Users/marin/Documents/Mestrado/Campos/Piloto/Prainha')
library(readxl)
library(viridis)
library(dplyr)
library(ggplot2)

ago2022 <- read_excel('C:/Users/marin/Documents/Mestrado/Campos/Piloto/Prainha/Ago2022.xlsx',
                      sheet = 'Planilha1')

ago2022$data <- as.Date(ago2022$data, format = "%d_%m_%Y")
str(ago2022$data)

#Renomear colunas
names(ago2022) <- c('ID', 'cor', 'data', 'tamanho_mm')

ago2022 %>%
  filter(ago2022$cor == "azul") %>% 
  ggplot(aes(x=data, y=tamanho_mm)) +
  geom_point(size = 1) +
  geom_path(aes(group = ID), color="gray79") +
  theme_bw()

ago2022 %>%
  filter(ago2022$cor == "azul") %>% 
  ggplot(aes(x=ID, y=tamanho_mm)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("indivíduos")
