library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(patchwork)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto/')

#chamando dataframe
entremares <- readxl::read_xlsx("mbon_p2p_out2022.xlsx")

### GRUPOS MAIS REPRESENTATIVOS ###

#verificando se não há repetições/erros de digitação
unique(entremares$type_cover) #sésseis
unique(entremares$motile) #móveis

entremares$relative_cover<- as.numeric(entremares$relative_cover) #transformando em numérico
str(entremares$relative_cover)

entremares$density_025m2<- as.numeric(entremares$density_025m2) #transformando em numérico
str(entremares$density_025m2)


#sésseis
plotcov <- entremares %>%
  mutate(tideHeight = recode(tideHeight, "high" = "superior", "mid" = "intermediário", "low" = "inferior") %>% 
           factor(., levels = c("superior","intermediário","inferior"))) %>% 
  filter(relative_cover >= 50) %>% ##coloquei 50 mas no gráfico o eixo y ta estranho
  ggplot(aes(x=type_cover, y=relative_cover)) + 
  geom_boxplot() +
  # geom_bar(stat = "identity") +
  ggtitle("a) organismos sésseis")+
  xlab("tipo de cobertura") +
  ylab("cobertura relativa (%)")+
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic")) 

plotcov #plotando as coberturas mais representativas 

#móveis
plotmot <- entremares %>% 
  mutate(tideHeight = recode(tideHeight, "high" = "superior", "mid" = "intermediário", "low" = "inferior") %>% 
           factor(., levels = c("superior","intermediário","inferior"))) %>%
  filter(density_025m2 >= 10, ##coloquei 10 mas no gráfico o eixo y ta estranho
         !density_025m2 %in% NA) %>%
  ggplot(aes(x=motile, y=density_025m2)) + 
  # geom_bar(stat = "identity") +
  geom_boxplot() +
  ggtitle("b) organismos móveis")+
  xlab("organismos") +
  ylab(expression("densidade (ind.0,25"~ m^-2~ ")")) +
  facet_grid(tideHeight ~ locality, scales = "free_y" ) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))

plotmot #plotando os móveis mais representativos

#both
plotcov + plotmot #unificando plots em um só

#guardando os mais abundantes
cover <- c("Amphiroa", "Bare rock", "Chthamalus bisinuatus", "Jania", "Lithophyllum", "Mytilaster solisianus", "Tetraclita stalactifera", "Ulva fasciata", "Willeella brachyclados")
motile <- c("Echinolittorina lineolata", "Fissurella rosea", "Lottia subrugosa", "Stramonita haemastoma")

### DENSIDADE/COBERTURA AO LONGO DO TEMPO ###
#cobertura: sésseis
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

#densidade: móveis
for(e in motile) {
  b <- entremares %>% 
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
    xlab("ano") +
    ylab("densidade (ind.0,25"~ m^-2~ ")")+
    facet_grid(tideHeight ~ locality ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5)) 
  print(b)
}

### TEMPERATURA AO LONGO DO TEMPO ###
library(lubridate)
library(scales)
library(heatwaveR)

#Juntando dados faltantes
#faltante<- read.csv('Fortshade_ago2021.csv', sep = ',') %>% 
 # mutate(date_time = as.POSIXct(date_time), 
        # sensor = "FORTSHADE",
         #site = "Fortaleza") 

#faltante<- faltante[ ,c(3,1,2,4)]

#temp_incom <- read.csv('temp_2019-set2022.csv', sep = ',') %>% 
 # mutate(date_time = as.POSIXct(date_time))

#temp_arraial_jan2023 <- bind_rows(faltante, temp_incom)

#write.csv(temp_arraial_jan2023, "temp_arraial_jan2023.csv", row.names = F)

#Dataframe com edições
temperatura <- read.csv('temp_arraial_jan2023.csv', sep = ',') %>% 
  mutate(date_time = as.POSIXct(date_time), 
         mes = month(date_time),
         hora = lubridate::hour(date_time),
         ano = year(date_time),
         sensor = recode(sensor, " Fortshade" = "sombra",
                         " Fortsun" = "sol",
                         " PG_SHADE" = "sombra",
                         " PG_SUN" = "sol",
                         "PG_SHADE" = "sombra",
                         "PG_SUN" = "sol")) %>% 
  mutate(season = case_when(mes %in% 1:3 ~ "verao",
                            mes %in% 4:6 ~ "outono",
                            mes %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno"))) 

# Calculando o 90º percentil de cada mês
maxis <- temperatura %>% 
  group_by(mes) %>% 
  dplyr::summarise(extremo = quantile(temp, probs = 0.9, na.rm = T))

maxis

# dias acima do 90º percentil
temperatura %>% 
  filter(!is.na(temp)) %>% 
  left_join(maxis) %>% 
  mutate(acima = ifelse(temp >= extremo, 1, 0)) %>% 
  group_by(season, mes) %>% 
  dplyr::summarise(d = sum(acima)/length(temp)*100)

#gráfico de barras: temperatura ao longo do dia 
temperatura %>% 
  ggplot(aes(x = hora, y = temp, group = hora)) +
  geom_point(aes(color = sensor)) +
  facet_grid(site + season ~ sensor) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        legend.position = "") +
  labs(y = expression("temperatura ("~ degree~ "C)"),
       x = "hora do dia")

#boxplot: temperatura ao longo do dia
temperatura %>% 
  ggplot(aes(x = hora, y = temp, group = hora)) +
  geom_boxplot(aes(color = sensor)) +
  facet_grid(site + season ~ sensor) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))


# valores dos sensores
temperatura %>%
  group_by(site, season, sensor) %>% 
  dplyr::summarise(varicao = range(temp, na.rm =T)) %>% 
  data.frame()

# unificando em sol e sombra
temperatura$sensor_stress[temperatura$sensor == 'FORTSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'FORTSUN'] <- 'sol'
temperatura$sensor_stress[temperatura$sensor == 'PGSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'PGSUN'] <- 'sol'

temperatura %>%
  group_by(site, season, sensor, sensor_stress) %>% 
  dplyr::summarise(varicao = range(temp, na.rm =T)) %>% 
  data.frame()

# temperatura ao longo do dia sol X sombra
temperatura %>% 
  ggplot(aes(x = hora, y = temp, color = sensor_stress, group = hora)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  facet_grid(site + season ~ sensor_stress) +
  xlab("Hora do Dia") +
  ylab("Temperatura (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#temperatura estação do ano sol X sombra
temperatura %>% 
  ggplot(aes(x = season, y = temp, color = sensor_stress)) +
  geom_boxplot(shape=21) + #, outlier.shape = NA) + 
  theme_classic() +
  facet_grid(~ site) +
  xlab("Estação do Ano") +
  ylab("Temperatura (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#queria separar por estações para ver o 90º percentil apenas durante o verão. Ou não?
#tem que ver se em algum momento teve acima do 90º percentil por mais de 5 dias.

#### CONFERINDO SE FALTAM DADOS #### 
### NAO APAGAR, USAR SEMPRE PARA CONFERIR ###
## PRAIA GRANDE
#sombra
#pg_shade<- temperatura %>% 
#  filter(temperatura$sensor == 'PGSHADE')

#pg_shade$date_time<- as.Date(pg_shade$date_time)

#unique(pg_shade$date_time)

#sol
#pg_sun<- temperatura %>% 
#  filter(temperatura$sensor == 'PGSUN')

#pg_sun$date_time<- as.Date(pg_sun$date_time)

#unique(pg_sun$date_time)

## FORTALEZA 
#sol
#ft_sun<- temperatura %>% 
#  filter(temperatura$sensor == 'FORTSUN')

#ft_sun$date_time<- as.Date(ft_sun$date_time)

#unique(ft_sun$date_time)

#sombra
#ft_shade<- temperatura %>% 
#  filter(temperatura$sensor == 'FORTSHADE')

#ft_shade$date_time<- as.Date(ft_shade$date_time)

#unique(ft_sun$date_time)

############ 01/03/23
#### separar por site? nao separar sol e sombra?
#### Testando o heat wave package

## PRAIA GRANDE
pg<- temperatura %>% 
  filter(temperatura$site == 'Praia Grande') %>% 
  mutate(date_time =  as.Date(date_time))

## FORTALEZA
ft<- temperatura %>% 
  filter(temperatura$site == 'Fortaleza') %>% 
  mutate(date_time = as.Date(date_time))

#sazonalidade
#Fortaleza
ft %>% 
  filter(date_time == min(date_time))

ft %>% 
  filter(date_time == max(date_time))

str(ft$date_time)

ts2_ft<- ts2clm(
  ft,
  x = date_time,
  y = temp,
  climatologyPeriod = c("2019-06-16", "2022-07-30"),
  robust = FALSE,
  maxPadLength = FALSE,
  windowHalfWidth = 5,
  pctile = 90,
  smoothPercentile = TRUE,
  smoothPercentileWidth = 31,
  clmOnly = FALSE,
  var = FALSE,
  roundClm = 4
) 

ggplot(ts2_ft, aes(x=date_time, y=temp)) +
  geom_line()

#Praia Grande
# nao temos 3 anos de dados
#pg %>% 
#  filter(date_time == min(date_time))

#pg %>% 
#  filter(date_time == max(date_time))

#ts2_pg<- ts2clm(
#  pg,
#  x = date_time,
#  y = temp,
#  climatologyPeriod = c("2021-12-06", "2022-09-08"),
#  robust = FALSE,
#  maxPadLength = FALSE,
#  windowHalfWidth = 5,
#  pctile = 90,
#  smoothPercentile = TRUE,
#  smoothPercentileWidth = 31,
#  clmOnly = FALSE,
#  var = FALSE,
#  roundClm = 4
#) 

#ggplot(ts2_pg, aes(x=date_time, y=temp)) +
#  geom_line()

# detecção de ondas de calor
# eventos sobrepostos... esquisito. Por que? Como resolver?
detect_ft<- detect_event(
  ts2_ft,
  x = date_time,
  y = temp,
  seasClim = seas,
  threshClim = thresh,
  threshClim2 = NA,
  minDuration = 5,
  minDuration2 = minDuration,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxGap2 = maxGap,
  coldSpells = FALSE,
  protoEvents = FALSE,
  categories = FALSE,
  roundRes = 4)

event_ft<- detect_ft$event
climatology_ft<- detect_ft$climatology

# Calculando médias anuais
yearmean_ft<- block_average(detect_ft, x = date_time, y = temp, report = "full")

summary(glm(count ~ year, yearmean_ft, family = "poisson")) #preciso entender melhor
glm(formula = count ~ year, family = "poisson", data = yearmean_ft) #preciso entender melhor

ggplot(data = yearmean_ft, aes(x = year, y = count)) +  ##número de eventos
  geom_point(colour = "blue") +
  geom_line() +
  labs(x = NULL, y = "Número de eventos")

ggplot(data = yearmean_ft, aes(x = year, y = intensity_cumulative)) +   ### intensidade cumulativa
  geom_point(colour = "salmon") +
  geom_line() +
  labs(x = NULL, y = "Intensidade cumulativa")

### Categorizando e contabilizando os heatwaves
#### ver como fazer separado por anos
cat_ft<- category(
  detect_ft,
  y = temp,
  S = TRUE,
  name = "Event",
  climatology = FALSE,
  MCScorrect = F,
  season = "range",
  roundVal = 4
)

freq_cat<- count(cat_ft, category) #contando frequência das categorias

freq_cat %>%
  ggplot(aes(x=category, y=n)) + 
  geom_bar(stat = "identity") 

# dias consecutivos acima do limiar
consecday_ft<- exceedance(
  ts2_ft,
  x = date_time,
  y = temp,
  threshold = 25.5,
  below = FALSE,
  minDuration = 5,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxPadLength = FALSE
)

consecthres_ft<- consecday_ft$threshold
consecexcee_ft<- consecday_ft$exceedance

consecexcee %>%
  ggplot(aes(x=exceedance_no, y=duration)) + 
  geom_bar(stat = "identity") 

# Gráfico climatologia
#erro, não vai
event_line(
  detect_ft,
  x = date_time,
  y = temp,
  metric = duration,
  min_duration = 5,
  spread = 150,
  start_date = '2019-06-16',
  end_date = '2022-07-30',
  category = FALSE,
  x_axis_title = NULL,
  x_axis_text_angle = NULL,
  y_axis_title = NULL,
  y_axis_range = NULL
)

# Gráfico de pirulito
ggplot(event_ft, aes(x= date_peak, y= intensity_max)) +
  geom_point() + 
  geom_segment( aes(x=date_peak, xend= date_peak, y=0, yend=intensity_max))

ggplot(event_ft, aes(x = date_peak, y = duration)) +      #duração e intensidade cumulativa
  geom_lolli(aes(colour = intensity_cumulative)) +
  scale_color_distiller(palette = "Spectral", name = "Cumulative \nintensity") +
  xlab("Date") + ylab("Event duration [days]")

#ggplot(event_ft, aes(x = date_peak, y = duration)) +      #os 3 mais longos
#  geom_lolli(n = 3, colour_n = "red") +
#  scale_color_distiller(palette = "Spectral") +
#  xlab("Peak date") + ylab("Event duration [days]")

#ggplot(event_ft, aes(x = date_peak, y = intensity_max)) +      #mais intensos
#  geom_lolli(n = 10, colour_n = "green") +
#  scale_color_distiller(palette = "Spectral") +
#  xlab("Peak date") + ylab("Intensidade máxima")

ggplot(event_ft, aes(x = date_peak, y = intensity_max)) +      #duração e intensidade cumulativa
  geom_lolli(aes(colour = intensity_mean)) +
  scale_color_distiller(palette = "Spectral", name = "intensity mean") +
  xlab("Date") + ylab("intensity_max")


###28/02/2023 - separado sol e sombra
### Sazonalidade
serie_ftsun<- ts2clm(
  ft_sun,
  x = date_time,
  y = temp,
  climatologyPeriod = c("2019-06-16", "2022-07-30"),
  robust = FALSE,
  maxPadLength = FALSE,
  windowHalfWidth = 5,
  pctile = 90,
  smoothPercentile = TRUE,
  smoothPercentileWidth = 31,
  clmOnly = FALSE,
  var = FALSE,
  roundClm = 4
) 

ggplot(serie_ftsun, aes(x=date_time, y=temp)) +
  geom_line()

### Detectando heatwaves
# esquisito, pois tem eventos sobrepostos..... mais de um dado por dia. O que usar?
detect_ftsun<- detect_event(
  serie_ftsun,
  x = date_time,
  y = temp,
  seasClim = seas,
  threshClim = thresh,
  threshClim2 = NA,
  minDuration = 5,
  minDuration2 = minDuration,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxGap2 = maxGap,
  coldSpells = FALSE,
  protoEvents = FALSE,
  categories = FALSE,
  roundRes = 4)

out<- detect_ftsun$event

# Calculando médias anuais
year_mean<- block_average(detect_ftsun, x = date_time, y = temp, report = "full")

summary(glm(count ~ year, year_mean, family = "poisson")) #preciso entender melhor
glm(formula = count ~ year, family = "poisson", data = year_mean) #preciso entender melhor

ggplot(data = year_mean, aes(x = year, y = count)) +  ##número de eventos
  geom_point(colour = "blue") +
  geom_line() +
  labs(x = NULL, y = "Número de eventos")

ggplot(data = year_mean, aes(x = year, y = intensity_cumulative)) +   ### intensidade cumulativa
  geom_point(colour = "salmon") +
  geom_line() +
  labs(x = NULL, y = "Intensidade cumulativa")

### Categorizando e contabilizando os heatwaves
#### ver como fazer separado por anos
cat_ftsun<- category(
  detect_ftsun,
  y = temp,
  S = TRUE,
  name = "Event",
  climatology = FALSE,
  MCScorrect = F,
  season = "range",
  roundVal = 4
)

freq_cat<- count(cat_ftsun, category) #contando frequência das categorias

freq_cat %>%
  ggplot(aes(x=category, y=n)) + 
  geom_bar(stat = "identity") 

# dias consecutivos acima do limiar
consecday_ftsun<- exceedance(
  serie_ftsun,
  x = date_time,
  y = temp,
  threshold = 25.5,
  below = FALSE,
  minDuration = 5,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxPadLength = FALSE
)

consecthres<- consecday_ftsun$threshold
consecexcee<- consecday_ftsun$exceedance

consecexcee %>%
  ggplot(aes(x=exceedance_no, y=duration)) + 
  geom_bar(stat = "identity") 

### gráfico
# não to conseguindo
# ERROOOO
event_line(
  detect_ftsun,
  x = date_time,
  y = temp,
  metric = duration,
  min_duration = 5,
  spread = 150,
  start_date = '2019-06-16',
  end_date = '2022-07-30',
  category = FALSE,
  x_axis_title = NULL,
  x_axis_text_angle = NULL,
  y_axis_title = NULL,
  y_axis_range = NULL
)

event_line(detect_ftsun, x= climatology$date_time, spread = 100, metric = duration,
           start_date = "2019-06-16", end_date = "2022-07-30")

# Gráfico de pirulito
ggplot(out, aes(x= date_peak, y= intensity_max)) +
  geom_point() + 
  geom_segment( aes(x=date_peak, xend= date_peak, y=0, yend=intensity_max))

 ggplot(out, aes(x = date_peak, y = duration)) +      #duração e intensidade cumulativa
  geom_lolli(aes(colour = intensity_cumulative)) +
  scale_color_distiller(palette = "Spectral", name = "Cumulative \nintensity") +
  xlab("Date") + ylab("Event duration [days]")

 ggplot(out, aes(x = date_peak, y = duration)) +      #os 3 mais longos
   geom_lolli(n = 3, colour_n = "red") +
   scale_color_distiller(palette = "Spectral") +
   xlab("Peak date") + ylab("Event duration [days]")
 
 ggplot(out, aes(x = date_peak, y = intensity_max)) +      #mais intensos
   geom_lolli(n = 10, colour_n = "green") +
   scale_color_distiller(palette = "Spectral") +
   xlab("Peak date") + ylab("Intensidade máxima")
 
 ggplot(out, aes(x = date_peak, y = intensity_max)) +      #duração e intensidade cumulativa
   geom_lolli(aes(colour = intensity_mean)) +
   scale_color_distiller(palette = "Spectral", name = "intensity mean") +
   xlab("Date") + ylab("intensity_max")
