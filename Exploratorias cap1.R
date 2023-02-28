library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(patchwork)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto/')

<<<<<<< HEAD
#chamando dataframe
entremares <- readxl::read_xlsx("mbon_p2p_out2022.xlsx")
=======
entremares <- readxl::read_xlsx("mbon_p2p_Aug2021.xlsx") #chamando dataframe
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84

### GRUPOS MAIS REPRESENTATIVOS ###

#verificando se não há repetições/erros de digitação
unique(entremares$type_cover) #sésseis
unique(entremares$motile) #móveis

entremares$relative_cover<- as.numeric(entremares$relative_cover) #transformando em numérico
str(entremares$relative_cover)

entremares$density_025m2<- as.numeric(entremares$density_025m2) #transformando em numérico
str(entremares$density_025m2)

<<<<<<< HEAD
#sésseis
=======
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84
plotcov <- entremares %>%
  mutate(tideHeight = recode(tideHeight, "high" = "superior", "mid" = "intermediário", "low" = "inferior") %>% 
           factor(., levels = c("superior","intermediário","inferior"))) %>% 
  filter(relative_cover >= 50) %>% ##coloquei 50 mas no gráfico o eixo y ta estranho
  ggplot(aes(x=type_cover, y=relative_cover)) + 
<<<<<<< HEAD
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
=======
    geom_boxplot() +
    # geom_bar(stat = "identity") +
    ggtitle("a) organismos sésseis")+
    xlab("tipo de cobertura") +
    ylab("cobertura relativa (%)")+
    facet_grid(tideHeight ~ locality, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic")) 

plotcov #plotando as coberturas mais representativas 

>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84
plotmot <- entremares %>% 
  mutate(tideHeight = recode(tideHeight, "high" = "superior", "mid" = "intermediário", "low" = "inferior") %>% 
           factor(., levels = c("superior","intermediário","inferior"))) %>%
  filter(density_025m2 >= 10, ##coloquei 10 mas no gráfico o eixo y ta estranho
         !density_025m2 %in% NA) %>%
  ggplot(aes(x=motile, y=density_025m2)) + 
<<<<<<< HEAD
  # geom_bar(stat = "identity") +
  geom_boxplot() +
  ggtitle("b) organismos móveis")+
  xlab("organismos") +
  ylab(expression("densidade (ind.0,25"~ m^-2~ ")")) +
  facet_grid(tideHeight ~ locality, scales = "free_y" ) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))

=======
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    ggtitle("b) organismos móveis")+
    xlab("organismos") +
    ylab(expression("densidade (ind.0,25"~ m^-2~ ")")) +
    facet_grid(tideHeight ~ locality, scales = "free_y" ) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic"))
    
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84
plotmot #plotando os móveis mais representativos

#both
plotcov + plotmot #unificando plots em um só


#guardando os mais abundantes
cover <- c("Amphiroa", "Bare rock", "Chthamalus bisinuatus", "Jania", "Lithophyllum", "Mytilaster solisianus", "Tetraclita stalactifera", "Ulva fasciata", "Willeella brachyclados")
motile <- c("Echinolittorina lineolata", "Fissurella rosea", "Lottia subrugosa", "Stramonita haemastoma")

### DENSIDADE/COBERTURA AO LONGO DO TEMPO ###
#cobertura: sésseis
########### ta dando erro ###########
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

<<<<<<< HEAD
#Dataframe com edições
temperatura <- read.csv('temp_2019-set2022.csv', sep = ',') %>% 
  mutate(date_time = as.POSIXct(date_time), 
         mes = month(date_time),
         hora = lubridate::hour(date_time),
         ano = year(date_time),
=======
temperatura <- read.csv("temp_arraial_entremares_out2022.csv", sep = ',') %>% 
  dplyr::rename(dia_hora = 'time') %>% 
  mutate(dia_hora = as.POSIXct(dia_hora), 
         mes = month(dia_hora),
         hora = lubridate::hour(dia_hora),
         ano = year(dia_hora),
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84
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
## PRAIA GRANDE
#sombra
pg_shade<- temperatura %>% 
  filter(temperatura$sensor == 'PGSHADE')

pg_shade$date_time<- as.Date(pg_shade$date_time)

<<<<<<< HEAD
unique(pg_shade$date_time)

#sol
pg_sun<- temperatura %>% 
  filter(temperatura$sensor == 'PGSUN')

pg_sun$date_time<- as.Date(pg_sun$date_time)

unique(pg_sun$date_time)

## FORTALEZA 
#sol
ft_sun<- temperatura %>% 
  filter(temperatura$sensor == 'FORTSUN')

ft_sun$date_time<- as.Date(ft_sun$date_time)

unique(ft_sun$date_time)

#sombra
ft_shade<- temperatura %>% 
  filter(temperatura$sensor == 'FORTSHADE')

ft_shade$date_time<- as.Date(ft_shade$date_time)

unique(ft_sun$date_time)

############ 28/02/23
#### separar por site? nao separar sol e sombra?
#### Testando o heat wave package

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

=======
temp %>% 
  mutate(dia = as.Date(dia),
         sensor = factor(sensor, levels = c("sol", "sombra"))) %>%
  ggplot(aes(x= dia, y = temp, group = dia)) +
    geom_point(aes(color = sensor)) +
    facet_wrap(site ~ sensor) +
    geom_boxplot(shape=21, outlier.shape = NA) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84

# Gráfico de pirulito
ggplot(out, aes(x= date_peak, y= intensity_max)) +
  geom_point() + 
  geom_segment( aes(x=date_peak, xend= date_peak, y=0, yend=intensity_max))

<<<<<<< HEAD
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
 
=======
## ERROO
temp %>% 
  mutate(dia_hora = as.Date(dia_hora),
         sensor = factor(sensor, levels = c("sol", "sombra"))) %>% 
  ggplot(aes(x = dia_hora, y = temp, group = dia_hora)) +
    geom_point(aes(color = sensor)) +
    facet_wrap(sensor ~. ) +
    geom_boxplot(shape=21, outlier.shape = NA) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))


# MEDIA POR HORA INDEPENDENTE DO MES OU DIA

temp %>%
  rename(time = "dia_hora") %>% 
  mutate(mes = month(dia_hora),
         hora = lubridate::hour(dia_hora),
         ano = year(dia_hora)) %>%
  # mutate(season = case_when(mes %in% 1:3 ~ "verao",
  #                           mes %in% 4:6 ~ "outono",
  #                           mes %in% 7:9 ~ "inverno",
  #                           TRUE ~ "primavera")) %>% 
  ggplot(aes(x = hora, y = temp, group = hora)) +
    geom_point(aes(color = sensor)) +
    facet_grid(season + ano ~ sensor) +
    # geom_boxplot(shape=21, outlier.shape = NA) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

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

#
temperatura %>% 
  ggplot(aes(x = hora, y = temp, group = hora)) +
    # geom_point(aes(color = sensor)) +
    facet_grid(sensor + site ~ season) +
    geom_boxplot(shape=21, aes(color = sensor), outlier.size = 0.1) +
    theme_classic() +
    theme(legend.position = "") +
    labs(y = expression("temperatura ("~ degree~"C)"),
         x = "hora do dia")

#
temperatura %>% 
  ggplot(aes(x = season, y = temp, color = sensor)) +
    geom_boxplot(shape=21) + 
    theme_classic() +
    facet_grid(~ site) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          legend.title = element_blank()) +
    labs(y = expression("temperatura ("~ degree~ "C)"),
       x = "estação do ano")
  

# valores
temperatura %>%
  group_by(site, season, sensor) %>% 
  dplyr::summarise(varicao = range(temp, na.rm =T)) %>% 
  data.frame()
>>>>>>> 1fd1e038e6345dd6684aee1af99a207202a56c84
