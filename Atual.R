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

entremares %>% 
  ggplot(aes(x = year, fill = type_cover)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

# juntando amphiroa e jania
entremares<- entremares %>% 
  mutate(type_cover = recode(type_cover, "Jania" = "ACA"),
         type_cover = recode(type_cover, 'Amphiroa' = 'ACA'),
         type_cover = recode(type_cover, 'Alga vermelha' = 'Corticada'),
         type_cover = recode(type_cover, 'Arthrocardia' = 'ACA'),
         type_cover = recode(type_cover, 'Bryopsis' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Lithophyllum' = 'Crostosa'),
         type_cover = recode(type_cover, 'Centroceras clavulatum' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Ulva fasciata' = 'Foliacea'),
         type_cover = recode(type_cover, 'Colpomenia sinuosa' = 'Corticada'),
         type_cover = recode(type_cover, 'Sargassum vulgare' = 'Coriacea'),
         type_cover = recode(type_cover, 'Dictyota' = 'Foliacea'),
         type_cover = recode(type_cover, 'Pterocladiela capillacea' = 'Corticada'),
         type_cover = recode(type_cover, 'Gelidium' = 'Corticada'),
         type_cover = recode(type_cover, 'Codium intertextum' = 'Corticada'),
         type_cover = recode(type_cover, 'Padina gymnospora' = 'Foliacea'),
         type_cover = recode(type_cover, 'Cladophora montagneana' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Porphyra' = 'Foliacea'),
         type_cover = recode(type_cover, 'Gelidiaceae' = 'Corticada'),
         type_cover = recode(type_cover, 'Gelidium pusillum' = 'Corticada'),
         type_cover = recode(type_cover, 'Chaetomorpha antennina' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Laurencia' = 'Corticada'),
         type_cover = recode(type_cover, 'Ceramiales' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Hypnea' = 'Corticada'),
         type_cover = recode(type_cover, 'Hypnea spinella' = 'Corticada'),
         type_cover = recode(type_cover, 'Cladophora' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Filamentosa verde' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Peyssonnelia' = 'Crostosa'),
         type_cover = recode(type_cover, 'Ceramiaceae' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Spyridia filamentosa' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Asteronema breviarticulatum' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Gracilaria' = 'Corticada'),
         type_cover = recode(type_cover, 'Asparagopsis' = 'Filamentosa'),
         type_cover = recode(type_cover, 'Willeella brachyclados' = 'Filamentosa'))

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
density_perday<- entremares %>%
  filter(!motile %in% NA) %>%
  group_by(eventDate, locality, season, tideHeight, motile, year) %>% 
  dplyr::summarise(sum = sum(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   densidade_m2 = sum/2.5) %>%
  data.frame()

density<- entremares %>%
  filter(!motile %in% NA) %>%
  group_by(locality, season, tideHeight, motile, year) %>% 
  dplyr::summarise(sum = sum(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   densidade_m2 = sum/2.5) %>%
  data.frame()

density %>% 
  filter(!densidade_m2 < 3) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
  ggplot(aes(x = year, y = densidade_m2, fill = motile)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

# nao da certo. Filtra, mas gráfico de barras fica diferente.
densidade_dom<- density_perday %>% 
  filter(motile == c('Echinolittorina lineolata', 'Fissurella rosea', 'Lottia subrugosa', 'Stramonita haemastoma'))
unique(densidade_dom$motile)


#mean_density<- entremares %>%
#  filter(!motile %in% NA,
#         !locality == 'Praia Grande',
#         !locality == 'Atalaia') %>%
#  group_by(eventDate, locality, season, tideHeight, motile, year) %>% 
#  dplyr::summarise(mean = mean(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
#                   desvio = sd(density_025m2, na.rm = TRUE)) %>%
#  data.frame()

mean_density <- mean_density %>% #retirando os Na's
  filter(!desvio %in% NA)

# sésseis
cover_perday<- entremares %>%
  filter(!type_cover %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia') %>%
  group_by(eventDate, locality, season, tideHeight, type_cover, year) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

cover_perday <- cover_perday %>% #retirando os Na's
  filter(!type_cover %in% NA)

cover_perday %>% 
  ggplot(aes(x = year, fill = type_cover)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

cover<- entremares %>%
  filter(!type_cover %in% NA) %>%
  group_by(locality, season, tideHeight, type_cover, year) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

## PROBLEMAÇO: MAIS DE 100% DE COBERTURA
teste<- cover_perday %>%
  filter(!type_cover %in% NA) %>%
  group_by(eventDate, locality, season, tideHeight, year) %>% 
  dplyr::summarise(sum = sum(mean, na.rm = TRUE)) %>%
  data.frame()

#mais representativos em cada substrato em cada ano
#Bare rock, Chthamalus bisinuatus, Foliacea, Mytilaster solisianus, Tetraclita stalactifera, Coriacea, Crostosa, Cyanophyceae
coverhigh<- cover_perday %>% 
  filter(tideHeight == 'high')

coverhigh %>%
  filter(!mean < 10) %>% 
  ggplot(aes(x = year, y = mean, fill = type_cover)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis_d() + 
  facet_grid(locality ~ season, scales = "free_y") +
  theme_bw()

covermid<- cover_perday %>% 
  filter(tideHeight == 'mid')

covermid %>%
  filter(!mean < 10) %>% 
  ggplot(aes(x = year, y = mean, fill = type_cover)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis_d() + 
  facet_grid(locality ~ season, scales = "free_y") +
  theme_bw()

coverlow<- cover_perday %>% 
  filter(tideHeight == 'low')

coverlow %>%
  filter(!mean < 10) %>% 
  ggplot(aes(x = year, y = mean, fill = type_cover)) + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis_d() + 
  facet_grid(locality ~ season, scales = "free_y") +
  theme_bw()

cover_perday %>% 
  filter(type_cover == c('Bare rock', 'Chthamalus bisinuatus', 'Foliacea', 'Mytilaster solisianus', 'Tetraclita stalactifera', 'Coriacea', 'Crostosa', 'Cyanophyceae')) %>% 
  ggplot(aes(x = year, y = mean, fill = type_cover)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()




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

entremares %>% 
  filter(motile == c('Lottia subrugosa','Fissurella rosea','Stramonita haemastoma','Echinolittorina lineolata')) %>%
  ggplot(aes(x = year, fill = motile)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

# sésseis
plotcov <- mean_cover %>%
  filter(mean >= 25) %>% #filtrando por geral! tem que separar por estratos!!!!!! Fazer individualmente.
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

#entremares %>% 
#  ggplot(aes(x = year, fill = type_cover)) + 
#  geom_bar(position = "stack") + 
#  scale_fill_viridis_d() + 
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

entremares %>% 
  filter(type_cover == c('ACA', 'Chthamalus bisinuatus', 'Bare rock', 'Tetraclita stalactifera', 'Mytilaster solisianus')) %>%
  ggplot(aes(x = year, fill = type_cover)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

#plotando os dois juntos
plotcov + plotmov  #colocar as algas mais representativas

### Dados populacionais
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
  theme(axis.text.y = element_text(vjust = 0.5, hjust = 1, face = "italic"))

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
  theme(axis.text.y = element_text(vjust = 0.5, hjust = 1, face = "italic"))

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
  a <- entremares %>% 
    filter(!relative_cover %in% c("?", "X"),
           !relative_cover %in% NA,
           type_cover == i) %>% 
    mutate(relative_cover = as.numeric(relative_cover),
           data = gsub("_", "-", data) %>% 
             as.POSIXct(format="%d-%m-%Y") %>% 
             as.Date(format = "%m-%Y"),
           tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
    ggplot(aes(x=data, y=relative_cover, group = data)) +
    geom_boxplot(shape=21, color="black", fill="#69b3a2") +
    ggtitle(i) +
    xlab("data") +
    ylab("cobertura relativa")+
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) 
  print(a)
}


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

mean_cover %>% 
    filter(type_cover == 'Tetraclita stalactifera') %>% #parece que tem menos amostragem e continua em linha
    ggplot(aes(x=eventDate, y=mean)) +
    geom_bar(stat = "identity") +
    xlab("ano") +
    ylab("cobertura (%)")+
    facet_grid(tideHeight ~ locality )


## juntos 
mean_density %>%
  filter(mean >= 4) %>%
  ggplot( aes(x=eventDate, y=mean, group=motile, color=motile)) +
  geom_line() +
  geom_point() +
  xlab("ano") +
  ylab("densidade (ind.0,25"~ m^-2~ ")")+
  facet_grid(locality ~ tideHeight)

mean_cover %>%  #acrescentar barra de erro (stat summary)
  filter(mean >= 25) %>%
  mutate(tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
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
         dia = date(date_time),
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

#temperatura$dia<- format(temperatura$dia, "%d/%m") #testando pra mexer só com dia e mês
#temperatura$dia<- as.Date(temperatura$dia, format = "%d/%m")

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
  #geom_errorbar(aes(ymin=mean_temp-desvio_temp, ymax=mean_temp+desvio_temp), width=.2,
                #position=position_dodge(0.05))+ #com desvio padrão
  facet_grid(season ~ site) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

# Temperatura por estação do ano
temp_season<- temperatura %>%
  group_by(site, season, sensor_stress, sensor) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   range = tmax-tmin) %>%
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
library(dygraphs)
library(xts) 

temp_perday<- temperatura %>%
  group_by(data, site, sensor_stress, sensor, season, ano, mes) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   lower = tmean-desvio,
                   upper = tmean+desvio,
                   trange = tmax - tmin) %>%
  data.frame()

hist(temp_perday$tmean)

# separando por sensores
# fortsun
fortsun<- temp_perday %>% 
  filter(sensor == 'FORTSUN')

fortsun<- fortsun[,-c(2,3,4,5,6,7,8,9,11)]
fortsun<- xts(x = fortsun[,-1], order.by = fortsun$data)
fsun<- dygraph(fortsun, main = "Fortsun", 
               ylab = "Temperatura média (ºC)") %>%
  dySeries(c("lower", "tmean", "upper"))   #NAO TA INTERVALO DE CONFIANÇA, TA MIN E MAX
fsun

# fortshade
fortshade<- temp_perday %>% 
  filter(sensor == 'FORTSHADE')

fortshade<- fortshade[,-c(2,3,4,5,6,7,8,9,11)]
fortshade<- xts(x = fortshade[,-1], order.by = fortshade$data)
fshade<- dygraph(fortshade, main = "Fortshade", 
               ylab = "Temperatura média (ºC)") %>%
  dySeries(c("lower", "tmean", "upper"))   #NAO TA INTERVALO DE CONFIANÇA, TA MIN E MAX
fshade

# Pgsun
pgsun<- temp_perday %>% 
  filter(sensor == 'PGSUN')

pgsun<- pgsun[,-c(2,3,4,5,6,7,8,9,11)]
pgsun<- xts(x = pgsun[,-1], order.by = pgsun$data)
p_sun<- dygraph(pgsun, main = "PGSUN", 
                 ylab = "Temperatura média (ºC)") %>%
  dySeries(c("lower", "tmean", "upper"))   #NAO TA INTERVALO DE CONFIANÇA, TA MIN E MAX
p_sun

# Pgshade
pgshade<- temp_perday %>% 
  filter(sensor == 'PGSHADE')

pgshade<- pgshade[,-c(2,3,4,5,6,7,8,9,11)]
pgshade<- xts(x = pgshade[,-1], order.by = pgshade$data)
p_shade<- dygraph(pgshade, main = "PGShade", 
                ylab = "Temperatura média (ºC)") %>%
  dySeries(c("lower", "tmean", "upper"))   #NAO TA INTERVALO DE CONFIANÇA, TA MIN E MAX
p_shade


### Análise dos dados (estatística)
library(psych)
library(nortest)
library(outliers)
library(FSA)
library(dgof)
library(multcompView)

hist(temperatura$temp) #verificando normalidade

temperatura$ln_temp <- log(temperatura$temp) #transformando em log. 
hist(temperatura$ln_temp)

lntemp_perday<- temperatura %>%
  group_by(data, site, sensor_stress, sensor, season, ano, mes) %>% 
  dplyr::summarise(ln_tmax = max(ln_temp, na.rm = TRUE),
                   ln_tmin = min(ln_temp, na.rm = TRUE),
                   ln_tmean = mean(ln_temp, na.rm = TRUE),
                   desvio_ln = sd(ln_temp, na.rm = TRUE),
                   lower_ln = ln_tmean-desvio_ln,
                   upper_ln = ln_tmean+desvio_ln,
                   ln_trange = ln_tmax - ln_tmin) %>% #ver se ta certo esse lnrange
  data.frame()

# Utilizando a média diária
# separando por estações para fazer a parte estatística
#verão
verao_perday <- lntemp_perday %>% #infos de verão por dia
  filter(season == 'verao')

plot_verao<- temperatura %>% 
  filter(season == 'verao') %>% 
  ggplot(aes(x = site, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ggtitle("Verão") +
  ylab("Temperatura (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

verao_anova <- aov(verao_perday$ln_tmean ~ verao_perday$sensor, data= verao_perday)
anova(verao_anova)
verao_tukey<- TukeyHSD(verao_anova)

verao_cld <- multcompLetters4(verao_anova, verao_tukey)

#primavera
prima_perday <- lntemp_perday %>% #infos de primavera por dia
  filter(season == 'primavera')

plot_prima<- temperatura %>% 
  filter(season == 'primavera') %>% 
  ggplot(aes(x = site, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ylab("Temperatura (ºC)")+
  ggtitle("Primavera") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

prima_anova <- aov(prima_perday$ln_tmean ~ prima_perday$sensor, data= prima_perday)
anova(prima_anova)
prima_tukey<- TukeyHSD(prima_anova)

prima_cld <- multcompLetters4(prima_anova, prima_tukey)

#outono
outono_perday <- lntemp_perday %>% #infos de outono por dia
  filter(season == 'outono')

plot_outono<- temperatura %>% 
  filter(season == 'outono') %>% 
  ggplot(aes(x = site, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ylab("Temperatura (ºC)")+
  ggtitle("Outono") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

outono_anova <- aov(outono_perday$ln_tmean ~ outono_perday$sensor, data= outono_perday)
anova(outono_anova)
outono_tukey<- TukeyHSD(outono_anova)

outono_cld <- multcompLetters4(outono_anova, outono_tukey)

#inverno
inverno_perday <- lntemp_perday %>% #infos de inverno por dia
  filter(season == 'inverno')

plot_inverno<- temperatura %>% 
  filter(season == 'inverno') %>% 
  ggplot(aes(x = site, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ylab("Temperatura (ºC)")+
  ggtitle("Inverno") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

inverno_anova <- aov(inverno_perday$ln_tmean ~ inverno_perday$sensor, data= inverno_perday)
anova(inverno_anova)
inverno_tukey<- TukeyHSD(inverno_anova)

inverno_cld <- multcompLetters4(inverno_anova, inverno_tukey)

plot_verao + plot_prima + plot_inverno + plot_outono #plotando todos juntos
print(verao_cld)
print(prima_cld)
print(inverno_cld)
print(outono_cld)

# Utilizando o range diário
#verão
plot_verao_range<- temp_perday %>% 
  filter(season == 'verao') %>% 
  ggplot(aes(x = site, y = trange, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ggtitle("Verão") +
  ylab("Amplitude térmica (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

verao_anova_range <- aov(verao_perday$ln_trange ~ verao_perday$sensor, data= verao_perday)
anova(verao_anova_range)
verao_tukey_range<- TukeyHSD(verao_anova_range)

verao_cld_range <- multcompLetters4(verao_anova_range, verao_tukey_range)

#primavera
plot_prima_range<- temp_perday %>% 
  filter(season == 'primavera') %>% 
  ggplot(aes(x = site, y = trange, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ggtitle("Primavera") +
  ylab("Amplitude térmica (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

prima_anova_range <- aov(prima_perday$ln_trange ~ prima_perday$sensor, data= prima_perday)
anova(prima_anova_range)
prima_tukey_range<- TukeyHSD(prima_anova_range)

prima_cld_range <- multcompLetters4(prima_anova_range, prima_tukey_range)

#inverno
plot_inverno_range<- temp_perday %>% 
  filter(season == 'inverno') %>% 
  ggplot(aes(x = site, y = trange, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ggtitle("Inverno") +
  ylab("Amplitude térmica (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

inverno_anova_range <- aov(inverno_perday$ln_trange ~ inverno_perday$sensor, data= inverno_perday)
anova(inverno_anova_range)
inverno_tukey_range<- TukeyHSD(inverno_anova_range)

inverno_cld_range <- multcompLetters4(inverno_anova_range, inverno_tukey_range)

#outono
plot_outono_range<- temp_perday %>% 
  filter(season == 'outono') %>% 
  ggplot(aes(x = site, y = trange, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  theme_classic() +
  ggtitle("Outono") +
  ylab("Amplitude térmica (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

outono_anova_range <- aov(outono_perday$ln_trange ~ outono_perday$sensor, data= outono_perday)
anova(outono_anova_range)
outono_tukey_range<- TukeyHSD(outono_anova_range)

outono_cld_range <- multcompLetters4(outono_anova_range, outono_tukey_range)

plot_verao_range + plot_prima_range + plot_inverno_range + plot_outono_range #plotando todos juntos

print(verao_cld_range)
print(prima_cld_range)
print(inverno_cld_range)
print(outono_cld_range)

# como trabalhar com os máximos? outliers?
# relevância biológica disso?

############## testando um trem
# média por dia (geral - dia 24/06 por ex, não dia esp)
teste<- temperatura %>%
  group_by(dia, site, sensor_stress, sensor, season) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   lower = tmean-desvio,
                   upper = tmean+desvio,
                   trange = tmax - tmin) %>%
  data.frame()

teste<- teste %>% # tirando os NA's
  filter(!dia %in% NA)

teste2<- teste %>% 
  filter(sensor == 'FORTSHADE')

teste2<- teste2[,-c(2:5)]

data <- as.matrix(teste2)
heatmap(data)

# como pegar essas médias e comparar um por um com os dados dos dias específicos para fazer um heatmap?