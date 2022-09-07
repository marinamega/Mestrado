### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Campos/Piloto/Prainha')
library(readxl)
library(viridis)
library(dplyr)
library(ggplot2)
#install.packages('esquisser') #atualizar versão do R
#library(esquisser)

set2022 <- read_excel('Set2022.xlsx',
                      sheet = 'Planilha1') #chamando dataframe

set2022$data <- as.Date(set2022$data, format = "%d_%m_%Y") #transformando data em data
str(set2022$data)
set2022$ID <- as.character(set2022$ID) #transformando ID em caracter
str(set2022$ID)

#Renomear colunas
names(set2022) <- c('ID', 'cor', 'data', 'tamanho_mm')

set2022$tamanho_mm <- as.numeric(set2022$tamanho_mm) #transformando tamanho em numérico
str(set2022$tamanho_mm)

#Marcação e Recaptura
freq_data<- count(set2022, data) #contando frequência das datas

freq_data %>%
  ggplot(aes(x=data, y=n)) + 
  geom_bar(stat = "identity") #plotando gráfico de barra com o número de capturados por dia
#data bugada

#Tamanho ao longo do tempo
set2022 %>%
  filter(!set2022$cor %in% "vermelho",
         !tamanho_mm %in% NA,
         data != '2022-08-26', data > '2022-08-01') %>% 
  ggplot(aes(x=as.factor(data), y=tamanho_mm)) +
  geom_boxplot() +
  # geom_point(size = 1) +
  # geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_bw()
                 
set2022 %>%
  filter(!set2022$cor %in% "vermelho",
         !set2022$data %in% c("2022_07_27", "2022_07_28"),
         !tamanho_mm %in% NA) %>% 
  ggplot(aes(x=data, y=tamanho_mm)) +
  geom_point(size = 1) +
  geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_bw()

set2022 %>%  #queria filtrar pelas duas ultimas datas, mas não to sabendo mexer com data em posicxt
  filter(!set2022$cor %in% "vermelho",
         !tamanho_mm %in% NA) %>%
  ggplot( aes(x=ID, y=tamanho_mm)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

set2022 %>% 
  select(-cor) %>% 
  filter(!is.na(tamanho_mm),
         data > "2022-08-25") %>%
  arrange(data, ID) %>% 
  mutate(diff_row = tamanho_mm - lag(tamanho_mm))


set2022 %>% 
  select(-cor) %>% 
  filter(!is.na(tamanho_mm)) %>%
  arrange(ID, data) %>%
  group_by(ID) %>%
  filter(n()>=2,
         data > "2022-08-01") %>% 
  mutate(diff_row = tamanho_mm - lag(tamanho_mm)) %>% 
  data.frame() %>% 
  mutate(diff = ifelse(diff_row > 0, "cresceu", 
                       ifelse(diff_row < 0, "encolheu", NA))) %>% 
  group_by(diff) %>% 
  summarise(med_dif = mean(diff_row),
            sd_dif = sd(diff_row),
            N = n_distinct(diff_row))

# DIFFERENCE BETWEEN VISIT BY PATIENT ------------------------------------- 
set2022 %>% 
  select(-cor) %>% 
  filter(!is.na(tamanho_mm),
         data > "2022-08-01") %>%
  arrange(ID, data) %>%
  group_by(ID) %>%
  filter(n()>=2) %>% 
  mutate(diffDate = difftime(data, lag(data,1)) / 86400) %>% 
  ungroup()
