### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Campos/Piloto')
library(readxl)
library(viridis)
library(dplyr)
library(ggplot2)
#install.packages('esquisser') #atualizar versão do R
#library(esquisser)


### PRAINHA ###
prainha <- read_excel('Set2022_Pra.xlsx',
                      sheet = 'Planilha1') #chamando dataframe

prainha$data <- as.Date(prainha$data, format = "%d_%m_%Y") #transformando data em data
str(prainha$data)
prainha$ID <- as.character(prainha$ID) #transformando ID em caracter
str(prainha$ID)

#Renomear colunas
names(prainha) <- c('ID', 'cor', 'data', 'tamanho_mm')

prainha$tamanho_mm <- as.numeric(prainha$tamanho_mm) #transformando tamanho em numérico
str(prainha$tamanho_mm)

#Marcação e Recaptura
freq_data<- count(prainha, data) #contando frequência das datas

freq_data %>%
  ggplot(aes(x=data, y=n)) + 
  geom_bar(stat = "identity") #plotando gráfico de barra com o número de capturados por dia
#data bugada

#Tamanho ao longo do tempo
total<- prainha %>%
  filter(!prainha$cor %in% "vermelho",
         !tamanho_mm %in% NA,
         data != '2022-08-26', data > '2022-08-01') %>% 
  ggplot(aes(x=as.factor(data), y=tamanho_mm)) +
  geom_boxplot() +
  ggtitle("a) todas as classes de tamanho")+
  xlab("campanha")+
  ylab("tamanho (mm)")+
  # geom_point(size = 1) +
  # geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_classic()

men14<- prainha %>%
  filter(!prainha$cor %in% "vermelho",
         !tamanho_mm %in% NA,
         data != '2022-08-26', data > '2022-08-01',
         tamanho_mm < 14) %>% 
  ggplot(aes(x=as.factor(data), y=tamanho_mm)) +
  geom_boxplot() +
  ggtitle("b) menores do que 14mm")+
  xlab("campanha")+
  ylab("tamanho (mm)")+
  # geom_point(size = 1) +
  # geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_classic()

mais14<- prainha %>%
  filter(!prainha$cor %in% "vermelho",
         !tamanho_mm %in% NA,
         data != '2022-08-26', data > '2022-08-01',
         tamanho_mm >= 14) %>% 
  ggplot(aes(x=as.factor(data), y=tamanho_mm)) +
  geom_boxplot() +
  ggtitle("c) maiores ou igual a 14mm")+
  xlab("campanha")+
  ylab("tamanho (mm)")+
  # geom_point(size = 1) +
  # geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_classic()

total + men14 + mais14

prainha %>%
  filter(!prainha$cor %in% "vermelho",
         !prainha$data %in% c("2022_07_27", "2022_07_28"),
         !tamanho_mm %in% NA,
         tamanho_mm < 14) %>% 
  ggplot(aes(x=data, y=tamanho_mm)) +
  ggtitle("Crescimento dos indiv?duos de Lottia subrugosa")+
  xlab("campanha")+
  ylab("tamanho (mm)")+
  geom_point(size = 1) +
  geom_path(aes(group = ID), color="gray79") +
  stat_summary(fun=mean, geom="line", color="dodgerblue3", aes(group=1), size=1) +
  theme_classic()

prainha %>%  #queria filtrar pelas duas ultimas datas, mas não to sabendo mexer com data em posicxt
  filter(!prainha$cor %in% "vermelho",
         !tamanho_mm %in% NA,
         tamanho_mm > 14) %>%
  ggplot( aes(x=ID, y=tamanho_mm)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

prainha %>% 
  select(-cor) %>% 
  filter(!is.na(tamanho_mm),
         data > "2022-08-25") %>%
  arrange(data, ID) %>% 
  mutate(diff_row = tamanho_mm - lag(tamanho_mm))


prainha %>% 
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
prainha %>% 
  select(-cor) %>% 
  filter(!is.na(tamanho_mm),
         data > "2022-08-01") %>%
  arrange(ID, data) %>%
  group_by(ID) %>%
  filter(n()>=2) %>% 
  mutate(diffDate = difftime(data, lag(data,1)) / 86400) %>% 
  ungroup()


### PRAIA GRANDE ###
pg <- read_excel('Set2022_PG.xlsx',
                      sheet = 'Planilha1') #chamando dataframe

pg$data <- as.Date(pg$data, format = "%d_%m_%Y") #transformando data em data
str(pg$data)
pg$ID <- as.character(pg$ID) #transformando ID em caracter
str(pg$ID)
pg$tamanho_mm <- as.numeric(pg$tamanho_mm) #transformando tamanho em numérico
str(pg$tamanho_mm)

#Marcação e Recaptura
freq_data_pg <- count(pg, data) #contando frequência das datas

freq_data_pg %>%
  ggplot(aes(x=data, y=n)) + 
  geom_bar(stat = "identity") #plotando gráfico de barra com o número de capturados por dia
#data bugada

