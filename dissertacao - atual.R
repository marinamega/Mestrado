library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(patchwork)
library(tidyverse)
library(lubridate)
library(vegan)
library(reshape2)
library(scales)
library(heatwaveR)
library(dygraphs)
library(xts) 
library(reshape)
library(RmarineHeatWaves)
library(multcompView)

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto/')

############# DADOS DE TEMPERATURA #################

# chamando dataframe
temperatura <- read.csv('temp_arraial_out2023.csv', sep = ',') %>% 
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
           factor(., levels = c("primavera", "verao", "outono", "inverno")),
         agua = case_when(mes %in% 4:9 ~ 'época sem ressurgencia',
                          TRUE ~ 'época ressurgencia') %>% 
           factor(., levels = c('época sem ressurgencia', 'época ressurgencia')))

# tirando NA
temperatura<- temperatura %>% # tirando os NA's
  filter(!temp %in% NA)

# unificando sol e sombra
temperatura$sensor_stress[temperatura$sensor == 'FORTSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'FORTSUN'] <- 'sol'
temperatura$sensor_stress[temperatura$sensor == 'PGSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'PGSUN'] <- 'sol'

# localização sites em relação a ressurgência
temperatura$influencia[temperatura$site == 'Fortaleza'] <- 'abrigado'
temperatura$influencia[temperatura$site == 'Praia Grande'] <- 'exposto'

#juntando a temperatura do ar. Perto do IEAPM
#temp_ar <- read.csv('TempAr_nov2023.csv', sep = ';') %>% 
 # mutate(data = dmy(data),
  #       mes = month(data),
   #      ano = year(data)) %>% 
  #mutate(season = case_when(mes %in% 1:3 ~ "verao",
   #                         mes %in% 4:6 ~ "outono",
    #                        mes %in% 7:9 ~ "inverno",
     #                       TRUE ~ "primavera") %>% 
      #     factor(., levels = c("primavera", "verao", "outono", "inverno")),
       #  agua = case_when(mes %in% 4:9 ~ 'sem ressurgencia',
        #                  TRUE ~ 'ressurgencia') %>% 
         #  factor(., levels = c('sem ressurgencia', 'ressurgencia')))

#temp_ar$temp <- as.numeric(temp_ar$temp)
#temp_ar$hora <- as.numeric(temp_ar$hora)

#temp_ar<- temp_ar %>% # tirando os NA's
 # filter(!temp %in% NA)

#temp_ar$sensor<- "ar"
#temp_ar$site<- "ar"
#temp_ar$sensor_stress<- "ar"

#temperatura<- merge(temperatura, temp_ar, by = c("sensor", "sensor_stress", "data", "hora", "site", "mes", "ano", "season", "temp", 'agua'), all = TRUE)

temperatura<- temperatura[,c(1,3,4,6,7,8,9,10, 11, 12, 13)]

# verificando
temperatura %>% 
  group_by(sensor, ano) %>% 
  slice(which.min(data))

temperatura %>% 
  group_by(sensor, ano) %>% 
  slice(which.max(data))

temperatura %>% 
  group_by(sensor, season) %>% 
  slice(which.min(data))

temperatura %>% 
  group_by(sensor, season) %>% 
  slice(which.max(data))

######## 1. SOMBRA X SOL E INFLUÊNCIA DA RESSURGÊNCIA
#### PARÂMETROS DA TEMPERATURA
CV <- function(x){
  (sd(x)/mean(x))*100
}

t_extremos_dia<- temperatura %>%
  group_by(data, sensor, sensor_stress, site, agua, ano, influencia) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE)) %>%
  data.frame()

t_extremos<- t_extremos_dia %>%
  group_by(sensor, sensor_stress, site, agua, influencia) %>% 
  dplyr::summarise(tmax_mean = mean(tmax, na.rm = TRUE),
                   tmax_sd = sd(tmax, na.rm = TRUE),
                   tmin_mean = mean(tmin, na.rm = TRUE),
                   tmin_sd = sd(tmin, na.rm = TRUE)) %>%
  data.frame()

t_mean_dia<- temperatura %>%
  group_by(data, sensor, sensor_stress, site, agua, ano, influencia) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   cv = (desvio/tmean)*100) %>%
  data.frame()

t_mean<- temperatura %>%
  group_by(sensor, sensor_stress, site, agua, influencia) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   tmean_sd = sd(temp, na.rm = TRUE),
                   cv = (tmean_sd/tmean)*100) %>%
  data.frame()

temp_infos <- merge(t_extremos, t_mean, by = c("sensor", "sensor_stress", "site", "agua", 'influencia'), all = TRUE)
temp_infos_dia <- merge(t_extremos_dia, t_mean_dia, by = c("sensor", "sensor_stress", "site", "data", "agua", "ano", 'influencia'), all = TRUE)

# TEMPERATURA MÉDIA
temp_infos_dia %>% #temperatura média
  ggplot(aes(x = influencia, y = tmean, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1), alpha = 0.4) +
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ agua) +
  xlab("") +
  ylab("Temperatura média (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$tmean) #normal

#estação quente - com ressurgência
v_temp_infos_dia<- temp_infos_dia %>% 
  filter(agua == 'época ressurgencia')

v_tmean_anova<- aov(v_temp_infos_dia$tmean ~ v_temp_infos_dia$sensor, data= v_temp_infos_dia)
anova(v_tmean_anova)
v_tmean_tukey<- TukeyHSD(v_tmean_anova)
v_tmean_cld <- multcompLetters4(v_tmean_anova, v_tmean_tukey)
v_tmean_cld

#estação fria - sem ressurgência
i_temp_infos_dia<- temp_infos_dia %>%
  filter(agua == 'época sem ressurgencia')

i_tmean_anova<- aov(i_temp_infos_dia$tmean ~ i_temp_infos_dia$sensor, data= i_temp_infos_dia)
anova(i_tmean_anova)
i_tmean_tukey<- TukeyHSD(i_tmean_anova)
i_tmean_cld <- multcompLetters4(i_tmean_anova, i_tmean_tukey)
i_tmean_cld

# TEMPERATURA MÁXIMA
temp_infos_dia %>% #temperatura máxima
  ggplot(aes(x = influencia, y = tmax, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1), alpha = 0.4) +
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ agua) +
  xlab("") +
  ylab("Temperatura máxima (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$tmax) #não normal

#estação quente - com ressurgência
kruskal.test(tmax~sensor, data = v_temp_infos_dia)  
v_tmax_KW <- pairwise.wilcox.test(v_temp_infos_dia$tmax,
                                  v_temp_infos_dia$sensor,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
v_tmax_KW$p.value
(v_tmax_a <- melt(v_tmax_KW$p.value))
v_tmax.cc  <-  na.omit(v_tmax_a)
v_tmax.pvals  <-  v_tmax.cc[, 3]
names(v_tmax.pvals)  <-  paste(v_tmax.cc[, 1], v_tmax.cc[, 2], sep="-")
multcompLetters(v_tmax.pvals)

#estação fria - sem ressurgência
kruskal.test(tmax~sensor, data = i_temp_infos_dia)  
i_tmax_KW <- pairwise.wilcox.test(i_temp_infos_dia$tmax,
                                  i_temp_infos_dia$sensor,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
i_tmax_KW$p.value
(i_tmax_a <- melt(i_tmax_KW$p.value))
i_tmax.cc  <-  na.omit(i_tmax_a)
i_tmax.pvals  <-  i_tmax.cc[, 3]
names(i_tmax.pvals)  <-  paste(i_tmax.cc[, 1], i_tmax.cc[, 2], sep="-")
multcompLetters(i_tmax.pvals)

# AMPLITUDE TÉRMICA
temp_infos_dia %>% #amplitude térmica
  ggplot(aes(x = influencia, y = cv, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1), alpha = 0.4) +
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ agua) +
  xlab("") +
  ylab("Amplitude térmica (CV)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$trange) #não normal

#estação quente - com ressurgência
kruskal.test(cv~sensor, data = v_temp_infos_dia)  
v_trange_KW <- pairwise.wilcox.test(v_temp_infos_dia$cv,
                                    v_temp_infos_dia$sensor,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
v_trange_KW$p.value
(v_trange_a <- melt(v_trange_KW$p.value))
v_trange.cc  <-  na.omit(v_trange_a)
v_trange.pvals  <-  v_trange.cc[, 3]
names(v_trange.pvals)  <-  paste(v_trange.cc[, 1], v_trange.cc[, 2], sep="-")
multcompLetters(v_trange.pvals)

#estação fria - sem ressurgência
kruskal.test(cv~sensor, data = i_temp_infos_dia)  
i_trange_KW <- pairwise.wilcox.test(i_temp_infos_dia$cv,
                                    i_temp_infos_dia$sensor,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
i_trange_KW$p.value
(i_trange_a <- melt(i_trange_KW$p.value))
i_trange.cc  <-  na.omit(i_trange_a)
i_trange.pvals  <-  i_trange.cc[, 3]
names(i_trange.pvals)  <-  paste(i_trange.cc[, 1], i_trange.cc[, 2], sep="-")
multcompLetters(i_trange.pvals)

# TEMPERATURA MÍNIMA
temp_infos_dia %>% #temperatura média
  ggplot(aes(x = influencia, y = tmin, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1), alpha = 0.4) +
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ agua) +
  xlab("") +
  ylab("Temperatura mínima (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$tmin) #normal

#estação quente - com ressurgência
v_tmin_anova<- aov(v_temp_infos_dia$tmin ~ v_temp_infos_dia$sensor, data= v_temp_infos_dia)
anova(v_tmin_anova)
v_tmin_tukey<- TukeyHSD(v_tmin_anova)
v_tmin_cld <- multcompLetters4(v_tmin_anova, v_tmin_tukey)
v_tmin_cld

#estação fria - sem ressurgência
i_tmin_anova<- aov(i_temp_infos_dia$tmin ~ i_temp_infos_dia$sensor, data= i_temp_infos_dia)
anova(i_tmin_anova)
i_tmin_tukey<- TukeyHSD(i_tmin_anova)
i_tmin_cld <- multcompLetters4(i_tmin_anova, i_tmin_tukey)
i_tmin_cld

#### Similaridade entre sensores: CLUSTER
temp_cluster <- temp_infos

# estação quente - verao e primavera
v_temp_cluster<- temp_cluster %>% 
  filter(agua == 'época ressurgencia')

rownames(v_temp_cluster) <- v_temp_cluster$sensor #amostra

v_km.clust <- v_temp_cluster %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

v_fit_temp <- v_temp_cluster[] %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

v_grupo_temp <- v_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

v_dendro <- factoextra::fviz_dend(v_km.clust, k = 1, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = 'black',
                                  main = "",
                                  ylab = "",
                                  horiz = T) +
  ggtitle('Época ressurgência')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

# estação fria - inverno e verao
i_temp_cluster<- temp_cluster %>% 
  filter(agua == 'época sem ressurgencia')

rownames(i_temp_cluster) <- i_temp_cluster$sensor #amostra

i_km.clust <- i_temp_cluster %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

i_fit_temp <- i_temp_cluster[] %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

i_grupo_temp <- i_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

i_dendro <- factoextra::fviz_dend(i_km.clust, k = 1, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = 'black',
                                  main = "",
                                  ylab = "",
                                  horiz = T) +
  ggtitle('Época sem ressurgência')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

v_dendro + i_dendro

#### CURVA DE AQUECIMENTO
# acima 50º percentil
# (temp no pico - temp h1)/nº horas entre temp no pico e temp h1

mean_temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, agua, influencia) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE)) %>%
  data.frame()

temperatura %>% 
  group_by(sensor, agua, influencia) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE),
                   max_temp = max(temp, na.rm = TRUE),
                   index = (mean_temp*1.5))%>%
  data.frame()

temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, agua, influencia) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   lower = tmean-desvio,
                   upper = tmean+desvio) %>%
  data.frame()

cores_personalizadas <- c("red", "LightSkyBlue", "DarkOrchid", "gold") #ar, fortshade, fortsun, pgshade, pgsun

temp_hora %>% #linhas com área preenchida
  ggplot(aes(x = hora, y = tmean, group=sensor, color=sensor, fill = sensor)) +
  geom_line(cex = 1.2) +
  geom_ribbon(aes(ymin = tmean - desvio,
                  ymax = tmean + desvio), alpha = 0.13, col = FALSE) +
  facet_wrap(~agua) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_color_manual(values = cores_personalizadas) +  # Define cores personalizadas para as linhas
  scale_fill_manual(values = cores_personalizadas)

# TAXA DE AQUECIMENTO
taxa_aquecimento<- temperatura

taxa_aquecimento$temperatura_normalizada<- scale(taxa_aquecimento$temp)

quantil<- taxa_aquecimento %>%  
  group_by(data, sensor, site, agua, influencia) %>% 
  dplyr::summarise(quantile50 = quantile(temperatura_normalizada, probs = 0.5)) %>%
  data.frame()

taxa_aquecimento<- left_join(taxa_aquecimento, quantil %>% select(sensor, data, agua, influencia, quantile50), by = c("data", 'sensor', 'agua', 'influencia')) #juntando em um só dataframe

temp_quantil50<- taxa_aquecimento %>% #filtrando as horas em que a temp > 50º quantil
  filter(temperatura_normalizada > quantile50)

temp_param<- taxa_aquecimento %>%  #parametros da curva de aquecimento
  group_by(data, sensor, site, agua, influencia) %>% 
  dplyr::summarise(tmax = max(temperatura_normalizada, na.rm = TRUE),
                   tmean = mean(temperatura_normalizada, na.rm = TRUE),
                   tmin = min(temperatura_normalizada, na.rm = TRUE)) %>%
  data.frame()

temp_quantil50<- left_join(temp_quantil50, temp_param %>% select(sensor, data, agua, influencia, tmax), by = c("data", 'sensor', 'agua', 'influencia')) #juntando em um só dataframe

temp_quantil_filt<- temp_quantil50 %>%
  group_by(sensor, data, agua, influencia, ano, quantile50) %>%
  filter(hora >= 6)

dados_selecionados <- temp_quantil_filt %>%
  group_by(sensor, data, agua, influencia, ano, quantile50) %>%
  filter(hora == min(hora) | temperatura_normalizada == max(temperatura_normalizada)) %>%
  ungroup()

onset_rate<- dados_selecionados %>%   
  group_by(sensor, data, agua, influencia, ano) %>%
  dplyr::summarise(t_peak = max(temperatura_normalizada),
                   t_posquantil = min(temperatura_normalizada),
                   h_peak = max(hora),
                   h_posquantil = min(hora)) %>%
  data.frame()

onset_rate$numerador<- onset_rate$t_peak - onset_rate$t_posquantil 
onset_rate$denominador<- abs(onset_rate$h_peak - onset_rate$h_posquantil)
onset_rate$onset_rate<- onset_rate$numerador/onset_rate$denominador

onset_rate <- onset_rate[complete.cases(onset_rate), ]

onset_rate<- onset_rate %>% 
  filter(!is.na(onset_rate),
         !is.nan(onset_rate),
         !is.infinite(onset_rate))

hist(onset_rate$onset_rate)

onset_season<- onset_rate %>%
  group_by(sensor, agua, influencia) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

onset_year<- onset_rate %>%
  group_by(sensor, agua, influencia, ano) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

# estatística
# Estação quente - com ressurgencia
onset_verao<- onset_rate %>% 
  filter(agua == 'época ressurgencia')

plot_on_v<- onset_verao %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  ggtitle('Época ressurgência') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_verao)
on_v<- pairwise.wilcox.test(onset_verao$onset_rate,
                            onset_verao$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_v$p.value
(on_v_a <- melt(on_v$p.value))
on_v.cc  <-  na.omit(on_v_a)
on_v.pvals  <-  on_v.cc[, 3]
names(on_v.pvals)  <-  paste(on_v.cc[, 1], on_v.cc[, 2], sep="-")
multcompLetters(on_v.pvals) 

# Estação fria - sem ressurgencia
onset_inv<- onset_rate %>% 
  filter(agua == 'época sem ressurgencia')

plot_on_i<- onset_inv %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  ggtitle('Época sem ressurgência') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_inv)
on_i<- pairwise.wilcox.test(onset_inv$onset_rate,
                            onset_inv$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_i$p.value
(on_i_a <- melt(on_i$p.value))
on_i.cc  <-  na.omit(on_i_a)
on_i.pvals  <-  on_i.cc[, 3]
names(on_i.pvals)  <-  paste(on_i.cc[, 1], on_i.cc[, 2], sep="-")
multcompLetters(on_i.pvals) 

plot_on_v + plot_on_i

##### HEATWAVE PACKAGE: DETECTANDO ONDAS DE CALOR
# APENAS FORTALEZA

# MICROHABITAT SOL
fort_sol<- temperatura %>% 
  filter(sensor == 'FORTSUN')

fort_sol_resum<- fort_sol %>% 
  group_by(data, sensor, agua, sensor_stress) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmax = max(temp, na.rm = TRUE))

names(fort_sol_resum) <- c('t', 'sensor', 'agua', 'sensor_stress', 'tmean', 
                            'tmin', 'tmax')

fort_sol_resum<- fort_sol_resum %>% 
  filter(!t < '2019-08-01')

fort_sol_resum <- fort_sol_resum %>%
  mutate(tmean = ifelse(between(t, as.Date('2021-12-15'), as.Date('2022-07-20')), NA, tmean))

#max(fort_sol_resum$t)
#min(fort_sol_resum$t)

ts2_sun<- ts2clm(  #climatologia
  fort_sol_resum,
  x = t,
  y = tmean,
  climatologyPeriod = c("2019-08-01", "2023-07-08"),  
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

heatmap <- ts2_sun %>%
  mutate(nova_coluna = tmean - seas)

plot_heat_sun<- ggplot(heatmap, aes(x = t, y = '',  fill = nova_coluna)) +
  geom_tile() +
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"))+
  labs(x = '', y = "Variação entre climatologia e temperatura registrada") +
  ggtitle("Fortaleza sol") +
  theme_classic()

detect_sun<- detect_event(
  ts2_sun,
  x = t,
  y = tmean,
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

event_sun<- detect_sun$event
climatology_sun<- detect_sun$climatology
climatology_sun$dif<- (climatology_sun$thresh - climatology_sun$seas)
climatology_sun$dif_2<- 2*climatology_sun$dif
climatology_sun$dif_3<- 3*climatology_sun$dif
climatology_sun$dif_4<- 4*climatology_sun$dif
climatology_sun$thres2<- climatology_sun$seas + climatology_sun$dif_2
climatology_sun$thres3<- climatology_sun$seas + climatology_sun$dif_3
climatology_sun$thres4<- climatology_sun$seas + climatology_sun$dif_4

df_sun <- climatology_sun %>%
  select(t , tmean, seas, thresh, thres2) %>%
  gather(key = "variable", value = "value", -t )

df_sun <- df_sun %>%
  mutate(ano = lubridate::year(t))

graph_clima_fortsun<- df_sun %>%
  ggplot(aes(x = t, y = value)) + 
    scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
    geom_line(aes(color = variable)) + 
    geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=3, color = 'DimGray') +
    ylim(18,30) +
    scale_color_manual(values = c("steelblue", "yellow", 'GoldenRod', 'black'),
                       labels = c("Climatologia", "Dobro do Threshold", 'Threshold', 'Temperatura Média')) +
    ggtitle('Fortaleza sol') +
    labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
    theme_classic()

yearmean_sun<- block_average(detect_sun, x = t, y = tmean, report = "full")
summary(glm(count ~ year, yearmean_sun, family = "poisson")) #não ta aumentando
summary(glm(temp_mean ~ year, yearmean_sun, family = "Gamma")) #não ta esquentando
summary(glm(total_days ~ year, yearmean_sun, family = "poisson")) #nao ta durando mais

loli_sun<- event_sun %>% 
  ggplot(aes(x = date_peak, y = intensity_max, col = duration)) + geom_point(size = 2) + 
  geom_segment(aes(x = date_peak, xend = date_peak, yend = intensity_max, y = 0)) + 
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20), breaks = seq(5, 20, by = 5)) +
  xlab("") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sol') +
  theme_classic()

# MICROHABITAT SOMBRA
fort_shade<- temperatura %>% 
  filter(sensor == 'FORTSHADE')

fort_shade_resum<- fort_shade %>% 
  group_by(data, sensor, agua, sensor_stress) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmax = max(temp, na.rm = TRUE))

names(fort_shade_resum) <- c('t', 'sensor', 'agua', 'sensor_stress', 'tmean', 
                           'tmin', 'tmax')

fort_shade_resum <- fort_shade_resum %>%
  mutate(tmean = ifelse(between(t, as.Date('2020-08-01'), as.Date('2021-08-20')), NA, tmean))

max(fort_shade_resum$t)
min(fort_shade_resum$t)

ts2_shade<- ts2clm(
  fort_shade_resum,
  x = t,
  y = tmean,
  climatologyPeriod = c("2019-08-05", "2023-07-08"),
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

heatmap_shade <- ts2_shade %>%
  mutate(nova_coluna = tmean - seas)

plot_heat_shade<- ggplot(heatmap_shade, aes(x = t, y = '',  fill = nova_coluna)) +
  geom_tile() +
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"))+
  labs(x = '', y = "Variação entre a climatologia e a temperatura registrada") +
  ggtitle('Fortaleza sombra')+
  theme_classic()

plot_heat_sun / plot_heat_shade

detect_shade<- detect_event(
  ts2_shade,
  x = t,
  y = tmean,
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

event_shade<- detect_shade$event
climatology_shade<- detect_shade$climatology

climatology_shade$dif<- (climatology_shade$thresh - climatology_shade$seas)
climatology_shade$dif_2<- 2*climatology_shade$dif
climatology_shade$dif_3<- 3*climatology_shade$dif
climatology_shade$dif_4<- 4*climatology_shade$dif
climatology_shade$thres2<- climatology_shade$seas + climatology_shade$dif_2
climatology_shade$thres3<- climatology_shade$seas + climatology_shade$dif_3
climatology_shade$thres4<- climatology_shade$seas + climatology_shade$dif_4

df_shade <- climatology_shade %>%
  select(t , tmean, seas, thresh, thres2) %>%
  gather(key = "variable", value = "value", -t )

df_shade <- df_shade %>%
  mutate(ano = lubridate::year(t))

graph_clima_fortshade<- df_shade %>%
  ggplot(aes(x = t, y = value)) + 
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  geom_line(aes(color = variable)) + 
  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 424, 571, 611, 665, 712, 1196, 1370)]), linetype=3, color = 'DimGray') +
  ylim(18,30) +
  scale_color_manual(values = c("steelblue", "yellow", 'GoldenRod', 'black'),
                     labels = c("Climatologia", "Dobro do Threshold", 'Threshold', 'Temperatura Média')) +
  ggtitle('Fortaleza sombra') +
  labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
  theme_classic()

graph_clima_fortsun / graph_clima_fortshade

yearmean_shade<- block_average(detect_shade, x = t, y = tmean, report = "full")
summary(glm(count ~ year, yearmean_shade, family = "poisson")) #não ta aumentando
summary(glm(temp_mean ~ year, yearmean_shade, family = "Gamma")) #não ta esquentando
summary(glm(total_days ~ year, yearmean_shade, family = "poisson")) #nao ta durando mais

loli_shade<- event_shade %>% 
  ggplot(aes(x = date_peak, y = intensity_max, col = duration)) + geom_point(size = 2) + 
  geom_segment(aes(x = date_peak, xend = date_peak, yend = intensity_max, y = 0)) + 
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20), breaks = seq(5, 20, by = 5)) +
  xlab("") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sombra') +
  theme_classic()

loli_sun / loli_shade

yearmean_shade$microhabitat<- 'sombra'
yearmean_sun$microhabitat<- 'sol'

heatwave_comparation <- merge(yearmean_shade, yearmean_sun, by = c('microhabitat', "year", "count", "total_days"), all = TRUE)
heatwave_comparation<- heatwave_comparation[,c(1,2,3,4)]

summary(glm(count ~ microhabitat, heatwave_comparation, family = "poisson"))
summary(glm(total_days ~ microhabitat, heatwave_comparation, family = "poisson"))





############# DADOS DE COMUNIDADE #################

### chamando dataframe
entremares <- readxl::read_xlsx("monit_entremares_julho2023_datasjutaselitoigual.xlsx") %>% 
  mutate(eventDate = as.POSIXct(eventDate),
         year = year(eventDate),
         month = month(eventDate),
         season = case_when(month %in% 1:3 ~ "verao",
                            month %in% 4:6 ~ "outono",
                            month %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno")),
         agua = case_when(month %in% 4:9 ~ 'época sem ressurgencia',
                          TRUE ~ 'época ressurgencia') %>% 
           factor(., levels = c('época sem ressurgencia', 'época ressurgencia')))

entremares %>% 
  ggplot(aes(x = year, fill = type_cover)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

# juntando amphiroa e jania
entremares <- entremares %>% 
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

entremares <- entremares %>% 
  mutate(type_cover = recode(type_cover, "Foliacea" = "Outra macroalga"),
         type_cover = recode(type_cover, 'Coriacea' = 'Outra macroalga'),
         type_cover = recode(type_cover, 'Filamentosa' = 'Outra macroalga'))

# colocando em português
entremares <- entremares %>%
  mutate(tideHeight = recode(tideHeight, "high" = "superior"),
         tideHeight = recode(tideHeight, "mid" = "médio"),
         tideHeight = recode(tideHeight, "low" = "inferior"))

# tirando atalaia e PG
entremares <- entremares %>%
  filter(!locality == 'Praia Grande',
         !locality == 'Atalaia')

### editando e corrigindo dados
#verificando se não há repetições/erros de digitação
unique(entremares$type_cover) #sésseis
unique(entremares$motile) #móveis

## ORGANISMOS MÓVEIS: DENSIDADE
## INCLUINDO OS ZEROS
# substituir contagens antigas de Echinolittorina (ex. 150/89)
litto <- data.frame(density_025m2 = entremares %>% 
                      select(density_025m2) %>% 
                      filter(!is.na(density_025m2),
                             nchar(density_025m2) == 6) %>% 
                      distinct() %>% pull(),
                    d2 = c(
                      (150/25)*8,
                      (150/22)*8,
                      (150/16)*8,
                      (150/38)*8,
                      (150/27)*8,
                      (150/12)*8,
                      (150/10)*8,
                      (150/53)*8,
                      (150/55)*8,
                      (150/14)*8,
                      (150/13)*8,
                      (150/23)*8,
                      (150/39)*8,
                      (150/68)*8,
                      (150/89)*8,
                      (150/79)*8,
                      (150/28)*8,
                      (150/76)*8,
                      (150/35)*8,
                      (150/82)*8,
                      (150/24)*8,
                      (150/50)*8,
                      (150/46)*8,
                      (150/69)*8,
                      (150/37)*8,
                      (150/43)*8,
                      (150/36)*8,
                      (150/21)*8,
                      (150/63)*8,
                      (150/33)*8,
                      (150/17)*8,
                      (150/40)*8,
                      (150/11)*8,
                      (150/87)*8,
                      (150/73)*8,
                      (150/34)*8,
                      (150/18)*8,
                      (150/31)*8,
                      (150/91)*8,
                      (150/42)*8,
                      (150/78)*8,
                      (150/49)*8,
                      (150/70)*8,
                      (150/26)*8,
                      (150/62)*8
                    ) %>% round(0)
)  

# 
entremares <- entremares %>%
  mutate(density_025m2 = plyr::mapvalues(density_025m2, from = litto$density_025m2, to = litto$d2) %>% as.numeric())

str(entremares$density_025m2)

entremares_zeros <- entremares %>%
  select(locality, eventDate, data, tideHeight, agua, quadrat, motile, density_025m2, year) %>% 
  filter(!is.na(density_025m2)) %>% 
  pivot_wider(names_from = motile, values_from = density_025m2) %>% 
  pivot_longer(cols = `Fissurella rosea`:`Onchidella indolens`, names_to = "motile", values_to = "density_025m2") %>%
  mutate(density_025m2 = as.numeric(density_025m2)) %>% 
  mutate_all(., ~replace_na(.,0)) 

#Tirando Prainha
entremares_zeros<- entremares_zeros %>% 
  filter(!locality == 'Prainha')

## Ta geral, nao separado por grupo
entremares_zeros %>% 
  group_by(locality, eventDate, tideHeight, agua, quadrat, year) %>% 
  summarise(density_025m2 = mean(density_025m2)) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior")),
         year = year(eventDate)) %>%  
  ggplot(aes(x = year, group = year, y = density_025m2)) + 
  geom_boxplot(outlier.shape = "") +
  labs(x = '', y = 'Densidade (ind/0.25m²)') +
  scale_x_continuous(breaks = c(2018:2023)) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

density_perday<- entremares_zeros %>%   #Por 0.25m²
  filter(!motile %in% NA) %>%
  group_by(eventDate, locality, agua, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_0.25m2 = mean(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   sd_density_0.25m2 = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

density<- entremares_zeros %>%  #Por 0.25m²
  filter(!motile %in% NA) %>%
  group_by(locality, agua, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_0.25m2 = mean(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   sd_density_0.25m2 = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

density %>% 
  filter(!mean_density_0.25m2 < 2) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  #ggplot(aes(x = year, y = densidade_m2, fill = motile)) + EU TINHA POSTO. O QUE TA AGORA FOI CESAR
  #geom_bar(stat = 'identity') + 
  #scale_fill_viridis_d() + 
  ggplot(aes(x = year, group = year, y = mean_density_0.25m2, col = motile)) + 
  geom_boxplot(width = 0.5, outlier.shape = "") +
  labs(x = '', y = 'Densidade média (ind/0.25m²)') +
  scale_x_continuous(breaks = c(2018:2023)) +
  geom_jitter(width = 0.3, alpha = 0.7) +
  facet_grid(~tideHeight, scales = "free_y") +
  theme_bw()

entremares_zeros_year <- entremares_zeros %>%
  group_by(locality, agua, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_year = mean(density_025m2, na.rm = TRUE),
                   sd_density_year = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

densidade_dom <- c('Echinolittorina lineolata', 'Fissurella rosea', 'Lottia subrugosa', 'Stramonita haemastoma')

density_fort<- density_perday %>% 
  filter(locality == 'Fortaleza',
         motile %in% densidade_dom) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x = motile, y = mean_density_0.25m2)) + 
  geom_boxplot() +
  ggtitle('Fortaleza') +
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ agua, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", angle = 90)) +
  labs(x = "Organismos móveis", y = "Densidade (ind/0.25m²)")

## Deixando apenas os mais dominantes nos dataframes para estatística
density_perday_dom<- density_perday %>% 
  filter(motile %in% densidade_dom)

density_peryear_dom<- entremares_zeros_year %>% 
  filter(motile %in% densidade_dom)

## ORGANISMOS SÉSSEIS: COBERTURA
cover_perday <- entremares %>%
  filter(!type_cover %in% NA,
         !locality == 'Praia Grande',
         !locality == 'Atalaia',
         !locality == 'Prainha') %>%
  group_by(eventDate, locality, season, tideHeight, type_cover, year) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

cover_perday <- cover_perday %>% #retirando os Na's
  filter(!type_cover %in% NA,
         !mean %in% NaN)

cover_perday %>% 
  ggplot(aes(x = year, fill = type_cover)) + 
  geom_bar(position = "stack") + 
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

cover <- entremares %>%
  filter(!type_cover %in% NA) %>%
  group_by(locality, season, tideHeight, type_cover, year) %>% 
  dplyr::summarise(mean = mean(relative_cover, na.rm = TRUE),
                   desvio = sd(relative_cover, na.rm = TRUE)) %>%
  data.frame()

a<- cover_perday %>% 
  group_by(locality, season, tideHeight) %>%
  filter(!mean < 10) 

cover_dom<- unique(a$type_cover)

cover_fort<- cover_perday %>% 
  filter(locality == 'Fortaleza',
         type_cover %in% cover_dom) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x = type_cover, y = mean)) + 
  geom_boxplot() +
  ggtitle('Fortaleza') +
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ season, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", angle = 90)) +
  labs(x = "Organismos sésseis", y = "Cobertura (%)")

cover_fort + density_fort

cover_perday %>% 
  filter(!mean < 10) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  #ggplot(aes(x = year, y = mean, fill = type_cover)) + EU TINHA POSTO. O QUE TA AGORA FOI CESAR
  #geom_bar(stat = 'identity') + 
  #scale_fill_viridis_d() + 
  ggplot(aes(x = year, group = year, y = mean, col = type_cover)) + 
  geom_boxplot(width = 0.5, outlier.shape = "") +
  labs(x = '', y = 'Cover (%)') +
  scale_x_continuous(breaks = c(2018:2023)) +
  geom_jitter(width = 0.3, alpha = 0.7) +
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

## Deixando apenas os de maior cobertura para estatística
cover_perday_dom<- cover_perday %>% 
  filter(type_cover %in% cover_dom) 

cover_peryear_dom<- cover %>% 
  filter(type_cover %in% cover_dom) 


#### DADOS POPULACIONAIS
## Para móveis: Lottia subrugosa e Echinollitorina lineolata
# Para cover: C bisinuatus, M solisianus, T stalactifera
density_perday_dom %>% 
  filter(!motile == 'Fissurella rosea',
         !motile == 'Stramonita haemastoma') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=motile, color=motile)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_grid(~tideHeight) +
  theme_classic()

cover_represent<- c('Tetraclita stalactifera', 'Chthamalus bisinuatus', 'Mytilaster solisianus')

cover_perday_dom$eventDate<- as.Date(cover_perday_dom$eventDate)

cover_perday_dom %>% 
  filter(type_cover %in% cover_represent) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean, group=type_cover, color=type_cover)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean-desvio, ymax = mean+desvio), position = position_dodge(width=0.7)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("cobertura (%)")+
  facet_grid(~tideHeight) +
  theme_classic()

library(GGally)
density_perday_dom$eventDate<- as.Date(density_perday_dom$eventDate)

temp_pop<- temp_infos_dia %>% 
  filter(!sensor == 'FORTSHADE',
         !sensor == 'PGSUN',
         !sensor == 'PGSHADE',
         !sensor == 'ar')

temp_pop <- temp_pop %>%
  filter(between(data, as.Date('2018-10-22'), as.Date('2018-11-21'))|
           between(data, as.Date('2019-01-20'), as.Date('2019-02-19'))|
           between(data, as.Date('2019-05-17'), as.Date('2019-06-16'))|
           between(data, as.Date('2019-09-22'), as.Date('2019-10-21'))|
           between(data, as.Date('2020-02-19'), as.Date('2020-03-10'))|
           between(data, as.Date('2020-07-05'), as.Date('2020-08-04'))|
           between(data, as.Date('2020-10-15'), as.Date('2020-11-14'))|
           between(data, as.Date('2021-07-22'), as.Date('2021-08-21'))|
           between(data, as.Date('2022-06-30'), as.Date('2022-07-30'))|
           between(data, as.Date('2023-06-09'), as.Date('2023-07-08'))) %>% 
  mutate(sample = cumsum(c(TRUE, diff(data) > 1)))

temp_pop <- temp_pop[-1,]

temp_pop_fill <- temp_pop %>%
  group_by(sensor, ano, sample) %>% 
  dplyr::summarise(tmean_month = mean(tmean, na.rm = TRUE),
                   mean_sd = sd(tmean, na.rm = TRUE),
                   tmax_mean_month = mean(tmax, na.rm = TRUE),
                   max_sd = sd(tmax, na.rm = TRUE),
                   cv_mean_month = mean(cv, na.rm = TRUE),
                   cv_sd = sd(cv, na.rm = TRUE)) %>%
  data.frame()

sample_date<- temp_pop %>%
  group_by(sensor, ano, sample) %>%
  summarize(eventDate = max(data))

temp_pop_fill <- left_join(temp_pop_fill, sample_date, by = c('sensor', 'ano', 'sample'))

pop_with_temp <- full_join(temp_pop_fill, density_perday_dom, by = c('eventDate'))

pop_with_temp %>% 
  ggplot(aes(x=eventDate, y=tmean_month)) +
  geom_line() +
  geom_point()+
  #geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("temperatura média mensal")+
  theme_classic()

param_graph <- pop_with_temp %>%
  select(eventDate , tmean_month, cv_mean_month, tmax_mean_month) %>%
  gather(key = "variable", value = "value", -eventDate )

param_graph %>% 
  ggplot(aes(x = eventDate, y = value)) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("steelblue", "coral", 'black'),
                     labels = c("tmean_month", "tmax_mean_month", 'cv_mean_month')) +
  ggtitle('') +
  labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
  theme_classic()

#Littorina
pop_with_temp %>% #colocar temperatura aqui em cima no plot
  filter(motile == 'Echinolittorina lineolata') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=tideHeight, color=tideHeight)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  ggtitle('Echinolittorina lineolata') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_wrap(~locality) +
  theme_classic()

e_lit_high<- pop_with_temp %>% 
  filter(motile == 'Echinolittorina lineolata',
         tideHeight == 'superior')

ggpairs(e_lit_high[,c(4,6,8,10,16)])

lit_mean<- ggplot(e_lit_high, aes(x = tmean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "E.lineolata - superior", x = "Temperatura média", y = "Densidade") +
  theme_classic()

lit_max<- ggplot(e_lit_high, aes(x = tmax_mean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "E.lineolata - superior", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

lit_cv<- ggplot(e_lit_high, aes(x = cv_mean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "E.lineolata - superior", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

lit_mean + lit_max + lit_cv

pop_with_temp %>% #colocar temperatura aqui em cima no plot
  filter(motile == 'Echinolittorina lineolata') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=tideHeight, color=tideHeight)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  ggtitle('Echinolittorina lineolata') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_wrap(~locality) +
  theme_classic()

#Lottia
density_perday_dom %>% 
  filter(motile == 'Lottia subrugosa') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=tideHeight, color=tideHeight)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  ggtitle('Lottia subrugosa') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_wrap(~locality) +
  theme_classic()

l_sub_low<- pop_with_temp %>% 
  filter(motile == 'Lottia subrugosa',
         tideHeight == 'inferior')

ggpairs(l_sub_low[,c(4,6,8,10,16)])

lsub_mean<- ggplot(l_sub_low, aes(x = tmean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "L.subrugosa - inferior", x = "Temperatura média", y = "Densidade") +
  theme_classic()

lsub_max<- ggplot(l_sub_low, aes(x = tmax_mean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "L.subrugosa - inferior", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

lsub_cv<- ggplot(l_sub_low, aes(x = cv_mean_month, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "L.subrugosa - inferior", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

lsub_mean + lsub_max + lsub_cv

modelo_mean <- lm(mean_density_0.25m2 ~ tmean_month, data = l_sub_low)
summary(modelo_mean)

modelo_max <- lm(mean_density_0.25m2 ~ tmax_mean_month, data = l_sub_low)
summary(modelo_max)

#cover
cover_with_temp <- full_join(temp_pop_fill, cover_perday_dom, by = c('eventDate'))

#T. stalactifera
tetra_low<- cover_with_temp %>% 
  filter(type_cover== 'Tetraclita stalactifera',
         tideHeight == 'inferior')

ggpairs(tetra_low[,c(4,6,8,10,16)])

tetra_mean<- ggplot(tetra_low, aes(x = tmean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tetraclita stalactifera - inferior", x = "Temperatura média", y = "Densidade") +
  theme_classic()

tetra_max<- ggplot(tetra_low, aes(x = tmax_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tetraclita stalactifera - inferior", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

tetra_cv<- ggplot(tetra_low, aes(x = cv_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tetraclita stalactifera - inferior", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

tetra_mean + tetra_max + tetra_cv

modelo_max <- lm(mean ~ tmax_mean_month, data = tetra_low)
summary(modelo_mean)

#c. BISINUATUS
chth_high<- cover_with_temp %>% 
  filter(type_cover== 'Chthamalus bisinuatus',
         tideHeight == 'superior')

ggpairs(chth_high[,c(4,6,8,10,16)])

chth_mean<- ggplot(chth_high, aes(x = tmean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Chthamalus bisinuatus - superior", x = "Temperatura média", y = "Densidade") +
  theme_classic()

chth_max<- ggplot(chth_high, aes(x = tmax_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Chthamalus bisinuatus - superior", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

chth_cv<- ggplot(chth_high, aes(x = cv_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Chthamalus bisinuatus - superior", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

chth_mean + chth_max + chth_cv

#M. SOLISIANUS
myt_mid<- cover_with_temp %>% 
  filter(type_cover== 'Mytilaster solisianus',
         tideHeight == 'médio')

ggpairs(myt_mid[,c(4,6,8,10,16)])

myt_mean<- ggplot(myt_mid, aes(x = tmean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mytilaster solisianus - médio", x = "Temperatura média", y = "Densidade") +
  theme_classic()

myt_max<- ggplot(myt_mid, aes(x = tmax_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mytilaster solisianus - médio", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

myt_cv<- ggplot(myt_mid, aes(x = cv_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mytilaster solisianus - médio", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

myt_mean + myt_max + myt_cv
