library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(patchwork)
library(tidyverse)
library(lubridate)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto/')

####### MONITORAMENTO DA COMUNIDADE #######

### chamando dataframe
entremares <- readxl::read_xlsx("monit_entremares_julho2023.xlsx") %>% 
  mutate(eventDate = as.POSIXct(eventDate),
         year = year(eventDate),
         month = month(eventDate),
         season = case_when(month %in% 1:3 ~ "verao",
                            month %in% 4:6 ~ "outono",
                            month %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno")))

# juntando amphiroa e jania
entremares<- entremares %>% 
  mutate(type_cover = recode(type_cover, "Jania" = "ACA"),
         type_cover = recode(type_cover, 'Amphiroa' = 'ACA'))

# tirando atalaia e PG
entremares <- entremares %>%
  filter(!locality == 'Praia Grande',
         !locality == 'Atalaia')

### editando e corrigindo dados
#verificando se não há repetições/erros de digitação
unique(entremares$type_cover) #sésseis
unique(entremares$motile) #móveis

entremares$relative_cover<- as.numeric(entremares$relative_cover) #transformando em numérico
str(entremares$relative_cover)

entremares$density_025m2<- as.numeric(entremares$density_025m2) #transformando em numérico
str(entremares$density_025m2)

### obtendo as médias para boxplot
# móveis
mean_density<- entremares %>%
  filter(!motile %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia') %>%
  group_by(eventDate, locality, season, tideHeight, motile) %>% 
  dplyr::summarise(mean = mean(density_025m2, na.rm = TRUE),
                   desvio = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

mean_density <- mean_density %>% #retirando os Na's
  filter(!desvio %in% NA)

# sésseis
mean_cover<- entremares %>%
  filter(!type_cover %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia') %>%
  group_by(eventDate, locality, season, tideHeight, type_cover) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

mean_cover <- mean_cover %>% #retirando os Na's
  filter(!desvio %in% NA)

### Plotando as médias dos mais representativos (BOXPLOT)
# móveis
plotmov <- mean_density %>% 
  filter(mean >= 4) %>% #subjetivo...
  ggplot(aes(x=motile, y=mean)) + 
  geom_boxplot() +
  ggtitle("b) organismos móveis")+
  xlab("organismos") +
  ylab(expression("densidade média (ind.0,25"~ m^-2~ ")")) +
  ylim(0,90) +
  facet_grid(tideHeight ~ locality, scales = "free_y" ) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))

plotmov

# sésseis
plotcov <- mean_cover %>%
  filter(mean >= 25) %>% #subjetivo
  ggplot(aes(x=type_cover, y=mean)) + 
  geom_boxplot() +
  ggtitle("a) organismos sésseis")+
  xlab("tipo de cobertura") +
  ylab("cobertura média relativa (%)")+
  ylim(20,100) +
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic")) 

plotcov 

#plotando os dois juntos
plotcov + plotmov

### Plotando as médias dos mais representativos (BARPLOT)
# móveis
bar_moveis <- entremares %>%
  filter(!motile %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia') %>%
  group_by(locality, tideHeight, motile) %>% 
  dplyr::summarise(mean = mean(density_025m2, na.rm = TRUE),
                   desvio = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

bar_moveis <- bar_moveis %>% #retirando os Na's
  filter(!desvio %in% NA)

bar_moveis %>% 
  filter(mean >= 4) %>% #ver se deixa stramonita ou não. Se sim, colocar 2. Vem outros grupos.
  ggplot(aes(x=motile, y=mean)) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=motile, ymin=mean-desvio, ymax=mean+desvio), width=0.4, colour="orange", alpha=0.9, size=0.8) +
  coord_flip() +
  ggtitle("b) organismos móveis")+
  xlab("organismos") +
  ylab(expression("densidade média (ind.0,25"~ m^-2~ ")")) +
  facet_grid(tideHeight ~ locality, scales = "free_y" ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))

# sésseis
bar_sesseis <- entremares %>%
  filter(!type_cover %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia') %>%
  group_by(locality, tideHeight, type_cover) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

bar_sesseis <- bar_sesseis %>% #retirando os Na's
  filter(!desvio %in% NA)

bar_sesseis %>% 
  filter(mean >= 10) %>% 
  ggplot(aes(x=type_cover, y=mean)) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=type_cover, ymin=mean-desvio, ymax=mean+desvio), width=0.4, colour="orange", alpha=0.9, size=0.8) +
  coord_flip() +
  ggtitle("b) organismos móveis")+
  xlab("organismos") +
  ylab(expression("cobertura (%)")) +
  facet_grid(tideHeight ~ locality, scales = "free_y" ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))

### guardando os mais abundantes
cover <- c("ACA", "Bare rock", "Chthamalus bisinuatus", "Mytilaster solisianus", "Tetraclita stalactifera")
motile <- c("Echinolittorina lineolata", "Fissurella rosea", "Lottia subrugosa", "Stramonita haemastoma")

### Dados populacionais
## individuais
# móveis
for(e in motile) {
  b <- mean_density %>% 
    filter(motile == e) %>% 
    ggplot(aes(x=eventDate, y=mean)) +
    geom_line() +
    geom_point()+
    ggtitle(e) +
    xlab("ano") +
    ylab("densidade (ind.0,25"~ m^-2~ ")")+
    facet_grid(tideHeight ~ locality )
  print(b)
}

# sésseis
for(i in cover) {
  a <- mean_cover %>% 
    filter(type_cover == i) %>% 
    ggplot(aes(x=eventDate, y=mean)) +
    geom_line() +
    geom_point()+
    ggtitle(i) +
    xlab("ano") +
    ylab("cobertura (%)")+
    facet_grid(tideHeight ~ locality )
  print(a)
}

## juntos 
mean_density %>%
  filter(mean >= 4) %>%
  ggplot( aes(x=eventDate, y=mean, group=motile, color=motile)) +
  geom_line() +
  geom_point() +
  xlab("ano") +
  ylab("densidade (ind.0,25"~ m^-2~ ")")+
  facet_grid(locality ~ tideHeight)

mean_cover %>%
  filter(mean >= 25) %>%
  ggplot( aes(x=eventDate, y=mean, group=type_cover, color=type_cover)) +
  geom_line() +
  geom_point() +
  xlab("ano") +
  ylab("cobertura (%)")+
  facet_grid(locality ~ tideHeight)

## decidir o que falta agora de dado de monitoramento
## observar padrões! Quando diminuem? Diferença entre as datas de amostragem?


####### DADOS DE TEMPERATURA (SENSORES NA ROCHA) #######
library(scales)
library(heatwaveR)

### chamando dataframe
temperatura <- read.csv('temp_arraial_julho2023.csv', sep = ',') %>% 
  mutate(date_time = as.POSIXct(date_time), 
         mes = month(date_time),
         hora = lubridate::hour(date_time),
         data = lubridate::date(date_time),
         ano = year(date_time),
         sensor = recode(sensor, "PG_SHADE" = "PGSHADE")) %>% 
  mutate(season = case_when(mes %in% 1:3 ~ "verao",
                            mes %in% 4:6 ~ "outono",
                            mes %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno")))

### editando/corrigindo dados
# tirando NA
temperatura<- temperatura %>% # tirando os NA's
  filter(!temp %in% NA)

# unificando sol e sombra
temperatura$sensor_stress[temperatura$sensor == 'FORTSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'FORTSUN'] <- 'sol'
temperatura$sensor_stress[temperatura$sensor == 'PGSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'PGSUN'] <- 'sol'

# verificando dadas iniciais e finais
min_date<- temperatura %>% 
  group_by(sensor) %>% 
  slice(which.min(data))

max_date<- temperatura %>% 
  group_by(sensor) %>% 
  slice(which.max(data))

min_temp<- temperatura %>% 
  group_by(sensor, season) %>% 
  slice(which.min(data))

max_temp<- temperatura %>% 
  group_by(sensor, season) %>% 
  slice(which.max(data))

### Plotando gráficos

# Temperatura ao longo do dia
temperatura %>% #original em boxplot
  ggplot(aes(x = hora, y = temp, group = hora)) +
  geom_boxplot(aes(color = sensor_stress)) +
  facet_grid(season + site ~ sensor_stress) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE)) %>%
  data.frame()

temp_hora %>% #novo em linhas
  ggplot(aes(x = hora, y = mean_temp, group=sensor_stress, color=sensor_stress)) +
  geom_line() +
  geom_point() +
  facet_grid(season ~ site) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

# Temperatura por estação
temp_season<- temperatura %>%
  group_by(site, season, sensor_stress, sensor) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE)) %>%
  data.frame()

temperatura %>% #original, boxplot
  ggplot(aes(x = season, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21) + #, outlier.shape = NA) + 
  theme_classic() +
  facet_grid(~ site) +
  xlab("Estação do Ano") +
  ylab("Temperatura (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

# Temperatura por dia
temp_perday<- temperatura %>%
  group_by(data, site, sensor_stress, sensor, season, ano, mes) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE)) %>%
  data.frame()

hist(temp_perday$tmean)

ggplot(temp_perday, aes(x=data, y=tmean, color = sensor)) + 
  geom_point(size=1) +
  theme_ipsum()

fortsun<- temp_perday %>% 
  filter(sensor == 'FORTSUN')

ggplot(fortsun, aes(x=data, y=tmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

fortshade<- temp_perday %>% 
  filter(sensor == 'FORTSHADE')

ggplot(fortshade, aes(x=data, y=tmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

pgsun<- temp_perday %>% 
  filter(sensor == 'PGSUN')

ggplot(pgsun, aes(x=data, y=tmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

pgshade<- temp_perday %>% 
  filter(sensor == 'PGSHADE')

ggplot(pgshade, aes(x=data, y=tmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

### Análise dos dados (estatística)
hist(temperatura$temp)
