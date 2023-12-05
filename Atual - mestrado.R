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

### DIRETÓRIO ###
setwd('C:/Users/marin/Documents/Mestrado/Projeto/')

####### DADOS DE TEMPERATURA (SENSORES NA ROCHA) #######
library(scales)
library(heatwaveR)
library(dygraphs)
library(xts) 
library(reshape)
library(RmarineHeatWaves)
library(multcompView)
library(vegan)

### chamando dataframe
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
           factor(., levels = c("primavera", "verao", "outono", "inverno")))



str(temperatura$temp)
str(temperatura$data)
str(temperatura$temp) 

#ftsun <- read.csv('FORT_SUN_julho2023.csv', sep = ',')
#ftsun$sensor <- 'FORTSUN'
#ftsun$site <- 'Fortaleza'
#ftsun <- ftsun[ ,c(3,1,2,4)]

#names(ftsun) <- c('sensor', 'date_time', 'temp', 'site')

#temperatura <- merge(temperatura, ftsun, by = c("sensor", "date_time", "temp", "site"), all = TRUE)

#write.csv(temperatura, file = "temp_arraial_out2023.csv", row.names = FALSE)


### editando/corrigindo dados
# tirando NA
temperatura<- temperatura %>% # tirando os NA's
  filter(!temp %in% NA)

# unificando sol e sombra
temperatura$sensor_stress[temperatura$sensor == 'FORTSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'FORTSUN'] <- 'sol'
temperatura$sensor_stress[temperatura$sensor == 'PGSHADE'] <- 'sombra'
temperatura$sensor_stress[temperatura$sensor == 'PGSUN'] <- 'sol'

#temperatura<- temperatura[ ,c(3,1,2,4)]

#temperatura_duplicatas <- temperatura[duplicated(temperatura), ]
#print(temperatura_duplicatas)
#temperatura<- temperatura %>% 
  #distinct(data, sensor, hora, season, ano, .keep_all = TRUE)

#Pegar temp mar ponta da cabeça e molhe
#juntando a temperatura do ar. Perto do IEAPM
temp_ar <- read.csv('TempAr_nov2023.csv', sep = ';') %>% 
  mutate(data = dmy(data),
         mes = month(data),
         ano = year(data)) %>% 
  mutate(season = case_when(mes %in% 1:3 ~ "verao",
                            mes %in% 4:6 ~ "outono",
                            mes %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno")))

temp_ar$temp <- as.numeric(temp_ar$temp)
temp_ar$hora <- as.numeric(temp_ar$hora)

temp_ar<- temp_ar %>% # tirando os NA's
  filter(!temp %in% NA)

temp_ar$sensor<- "ar"
temp_ar$site<- "ar"
temp_ar$sensor_stress<- "ar"


#names(temperatura) <- c('temp', 'sensor', 'data', 'site')

temperatura<- merge(temperatura, temp_ar, by = c("sensor", "sensor_stress", "data", "hora", "site", "mes", "ano", "season", "temp"), all = TRUE)

temperatura<- temperatura[,c(1,2,3,4,5,6,7,8,9)]

# verificando dadas iniciais e finais
min_date<- temperatura %>% 
  group_by(sensor, ano) %>% 
  slice(which.min(data))

max_date<- temperatura %>% 
  group_by(sensor, ano) %>% 
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
  geom_boxplot(aes(color = sensor)) +
  facet_grid(season ~ sensor) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

######## 1. SOMBRA X SOL E RESSURGÊNCIA X NÃO-RESSURGÊNCIA
#### PARÂMETROS DA TEMPERATURA

#temperatura %>% #original, boxplot
#  filter(!site == 'ar') %>% 
#  ggplot(aes(x = site, y = temp, color = sensor_stress)) +
#  geom_boxplot(shape=21) + #, outlier.shape = NA) + 
#  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
#  theme_classic() +
#  facet_grid(~ season) +
#  xlab("Estação do Ano") +
#  ylab("Temperatura (ºC)")+
#  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))
CV <- function(x){
  (sd(x)/mean(x))*100
}

t_extremos_dia<- temperatura %>%
  group_by(data, sensor, sensor_stress, site, season, ano) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   trange = tmax - tmin) %>%
  data.frame()

t_extremos<- t_extremos_dia %>%
  group_by(sensor, sensor_stress, site, season) %>% 
  dplyr::summarise(tmax_mean = mean(tmax, na.rm = TRUE),
                   tmax_sd = sd(tmax, na.rm = TRUE),
                   tmin_mean = mean(tmin, na.rm = TRUE),
                   tmin_sd = sd(tmin, na.rm = TRUE),
                   trange_mean = mean(trange, na.rm = TRUE),
                   trange_sd = sd(trange, na.rm = TRUE)) %>%
  data.frame()

t_mean_dia<- temperatura %>%
  group_by(data, sensor, sensor_stress, site, season, ano) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   cv = (desvio/tmean)*100) %>%
  data.frame()

t_mean<- temperatura %>%
  group_by(sensor, sensor_stress, site, season) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   tmean_sd = sd(temp, na.rm = TRUE),
                   cv = (tmean_sd/tmean)*100) %>%
  data.frame()

#t_cv_dia<- temperatura %>%
 # group_by(data, sensor, sensor_stress, site, season, ano) %>% 
  #dplyr::summarise(cv = CV(temp)) %>%
  #data.frame()

#t_cv<- t_cv_dia %>%
 # group_by(sensor, sensor_stress, site, season) %>% 
  #dplyr::summarise(cv_mean = mean(cv),
   #                cv_sd = sd(cv)) %>%
  #data.frame()

temp_infos <- merge(t_extremos, t_mean, by = c("sensor", "sensor_stress", "site", "season"), all = TRUE)
#temp_infos <- merge(temp_infos, t_cv, by = c("sensor", "sensor_stress", "site", "season"), all = TRUE)

temp_infos_dia <- merge(t_extremos_dia, t_mean_dia, by = c("sensor", "sensor_stress", "site", "data", "season", "ano"), all = TRUE)
#temp_infos_dia<- merge(temp_infos_dia, t_cv_dia, by = c("sensor", "sensor_stress", "season", "data", "site", 'ano'), all = TRUE)

temp_infos_dia %>% #temperatura média
  filter(!site == 'ar') %>% 
  ggplot(aes(x = site, y = tmean, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ season) +
  xlab("") +
  ylab("Temperatura média (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

##T_MEAN
hist(temp_infos_dia$tmean) #normal

v_temp_infos_dia<- temp_infos_dia %>% #verao
  filter(season == 'verao',
         !sensor == 'ar')

v_tmean_anova<- aov(v_temp_infos_dia$tmean ~ v_temp_infos_dia$sensor, data= v_temp_infos_dia)
anova(v_tmean_anova)
v_tmean_tukey<- TukeyHSD(v_tmean_anova)
v_tmean_cld <- multcompLetters4(v_tmean_anova, v_tmean_tukey)
v_tmean_cld

p_temp_infos_dia<- temp_infos_dia %>% #primavera
  filter(season == 'primavera',
         !sensor == 'ar')

p_tmean_anova<- aov(p_temp_infos_dia$tmean ~ p_temp_infos_dia$sensor, data= p_temp_infos_dia)
anova(p_tmean_anova)
p_tmean_tukey<- TukeyHSD(p_tmean_anova)
p_tmean_cld <- multcompLetters4(p_tmean_anova, p_tmean_tukey)
p_tmean_cld

o_temp_infos_dia<- temp_infos_dia %>% #outono
  filter(season == 'outono',
         !sensor == 'ar')

o_tmean_anova<- aov(o_temp_infos_dia$tmean ~ o_temp_infos_dia$sensor, data= o_temp_infos_dia)
anova(o_tmean_anova)
o_tmean_tukey<- TukeyHSD(o_tmean_anova)
o_tmean_cld <- multcompLetters4(o_tmean_anova, o_tmean_tukey)
o_tmean_cld

i_temp_infos_dia<- temp_infos_dia %>% #inverno
  filter(season == 'inverno',
         !sensor == 'ar')

i_tmean_anova<- aov(i_temp_infos_dia$tmean ~ i_temp_infos_dia$sensor, data= i_temp_infos_dia)
anova(i_tmean_anova)
i_tmean_tukey<- TukeyHSD(i_tmean_anova)
i_tmean_cld <- multcompLetters4(i_tmean_anova, i_tmean_tukey)
i_tmean_cld

##TMAX
temp_infos_dia %>% #temperatura máxima
  filter(!site == 'ar') %>% 
  ggplot(aes(x = site, y = tmax, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ season) +
  xlab("") +
  ylab("Temperatura máxima (ºC)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$tmax) #não normal

#verao
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

#primavera
kruskal.test(tmax~sensor, data = p_temp_infos_dia)  
p_tmax_KW <- pairwise.wilcox.test(p_temp_infos_dia$tmax,
                                  p_temp_infos_dia$sensor,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
p_tmax_KW$p.value
(p_tmax_a <- melt(p_tmax_KW$p.value))
p_tmax.cc  <-  na.omit(p_tmax_a)
p_tmax.pvals  <-  p_tmax.cc[, 3]
names(p_tmax.pvals)  <-  paste(p_tmax.cc[, 1], p_tmax.cc[, 2], sep="-")
multcompLetters(p_tmax.pvals)

#outono
kruskal.test(tmax~sensor, data = o_temp_infos_dia)  
o_tmax_KW <- pairwise.wilcox.test(o_temp_infos_dia$tmax,
                                  o_temp_infos_dia$sensor,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
o_tmax_KW$p.value
(o_tmax_a <- melt(o_tmax_KW$p.value))
o_tmax.cc  <-  na.omit(o_tmax_a)
o_tmax.pvals  <-  o_tmax.cc[, 3]
names(o_tmax.pvals)  <-  paste(o_tmax.cc[, 1], o_tmax.cc[, 2], sep="-")
multcompLetters(o_tmax.pvals)

#inverno
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


## TRANGE
temp_infos_dia %>% #amplitude térmica
  filter(!site == 'ar') %>% 
  ggplot(aes(x = site, y = cv, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  scale_color_manual(values = c('Tomato', 'SteelBlue')) +
  theme_classic() +
  facet_grid(~ season) +
  xlab("") +
  ylab("Amplitude térmica (CV)")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(temp_infos_dia$trange) #não normal

v_t_cv_dia<- temp_infos_dia %>% 
  filter(season == 'verao',
         !sensor == 'ar')
p_t_cv_dia<- temp_infos_dia %>% 
  filter(season == 'primavera',
         !sensor == 'ar')
o_t_cv_dia<- temp_infos_dia %>% 
  filter(season == 'outono',
         !sensor == 'ar')
i_t_cv_dia<- temp_infos_dia %>% 
  filter(season == 'inverno',
         !sensor == 'ar')

#verao
kruskal.test(cv~sensor, data = v_t_cv_dia)  
v_trange_KW <- pairwise.wilcox.test(v_t_cv_dia$cv,
                                    v_t_cv_dia$sensor,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
v_trange_KW$p.value
(v_trange_a <- melt(v_trange_KW$p.value))
v_trange.cc  <-  na.omit(v_trange_a)
v_trange.pvals  <-  v_trange.cc[, 3]
names(v_trange.pvals)  <-  paste(v_trange.cc[, 1], v_trange.cc[, 2], sep="-")
multcompLetters(v_trange.pvals)

#primavera
kruskal.test(cv~sensor, data = p_t_cv_dia)  
p_trange_KW <- pairwise.wilcox.test(p_t_cv_dia$cv,
                                    p_t_cv_dia$sensor,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
p_trange_KW$p.value
(p_trange_a <- melt(p_trange_KW$p.value))
p_trange.cc  <-  na.omit(p_trange_a)
p_trange.pvals  <-  p_trange.cc[, 3]
names(p_trange.pvals)  <-  paste(p_trange.cc[, 1], p_trange.cc[, 2], sep="-")
multcompLetters(p_trange.pvals)

#outono
kruskal.test(cv~sensor, data = o_t_cv_dia)  
o_trange_KW <- pairwise.wilcox.test(o_t_cv_dia$cv,
                                    o_t_cv_dia$sensor,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
o_trange_KW$p.value
(o_trange_a <- melt(o_trange_KW$p.value))
o_trange.cc  <-  na.omit(o_trange_a)
o_trange.pvals  <-  o_trange.cc[, 3]
names(o_trange.pvals)  <-  paste(o_trange.cc[, 1], o_trange.cc[, 2], sep="-")
multcompLetters(o_trange.pvals)

#inverno
kruskal.test(cv~sensor, data = i_t_cv_dia)  
i_trange_KW <- pairwise.wilcox.test(i_t_cv_dia$cv,
                                    i_t_cv_dia$sensor,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
i_trange_KW$p.value
(i_trange_a <- melt(i_trange_KW$p.value))
i_trange.cc  <-  na.omit(i_trange_a)
i_trange.pvals  <-  i_trange.cc[, 3]
names(i_trange.pvals)  <-  paste(i_trange.cc[, 1], i_trange.cc[, 2], sep="-")
multcompLetters(i_trange.pvals)

####Similaridade entre sensores: CLUSTER
temp_cluster <- temp_infos %>% 
  filter(!sensor == 'ar')

temp_cluster$site_sigla[temp_cluster$site == 'Praia Grande'] <- 'PG'
temp_cluster$site_sigla[temp_cluster$site == 'Fortaleza'] <- 'FT'
#temp_cluster$site_sigla[temp_cluster$site == 'ar'] <- 'temp'

#Verão
v_temp_cluster<- temp_cluster %>% 
  filter(season == 'verao')

#v_temp_cluster$amostra <- paste(v_temp_cluster$site_sigla,v_temp_cluster$sensor_stress, sep = "_")

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
  ggtitle('Verão')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

v_dendro

#Primavera
p_temp_cluster<- temp_cluster %>% 
  filter(season == 'primavera')

#p_temp_cluster$amostra <- paste(p_temp_cluster$site_sigla,p_temp_cluster$sensor_stress, sep = "_")

rownames(p_temp_cluster) <- p_temp_cluster$sensor #amostra

p_km.clust <- p_temp_cluster %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

p_fit_temp <- p_temp_cluster[] %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

p_grupo_temp <- p_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

p_dendro <- factoextra::fviz_dend(p_km.clust, k = 1, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = 'black',
                                  main = "",
                                  ylab = "",
                                  horiz = T) +
  ggtitle('Primavera')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

p_dendro

#Inverno
i_temp_cluster<- temp_cluster %>% 
  filter(season == 'inverno')

#i_temp_cluster$amostra <- paste(i_temp_cluster$site_sigla,i_temp_cluster$sensor_stress, sep = "_")

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
  ggtitle('Inverno')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

i_dendro

#Outono
o_temp_cluster<- temp_cluster %>% 
  filter(season == 'outono')

#o_temp_cluster$amostra <- paste(o_temp_cluster$site_sigla,o_temp_cluster$sensor_stress, sep = "_")

rownames(o_temp_cluster) <- o_temp_cluster$sensor #amostra

o_km.clust <- o_temp_cluster %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

o_fit_temp <- o_temp_cluster[] %>%
  dplyr::select(tmax_mean, tmin_mean, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

o_grupo_temp <- o_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

o_dendro <- factoextra::fviz_dend(o_km.clust, k = 1, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = 'black',
                                  main = "",
                                  ylab = "",
                                  horiz = T) +
  ggtitle('Outono')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

o_dendro

v_dendro + p_dendro + i_dendro + o_dendro

#### CURVA DE AQUECIMENTO
# acima 50º percentil
# (temp no pico - temp h1)/nº horas entre temp no pico e temp h1

mean_temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE)) %>%
  data.frame()

temperatura %>% 
  group_by(sensor, season) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE),
                   max_temp = max(temp, na.rm = TRUE),
                   index = (mean_temp*1.5))%>%
  data.frame()

temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   lower = tmean-desvio,
                   upper = tmean+desvio,
                   trange = tmax - tmin) %>%
  data.frame()

resum_temp_hora<- temp_hora[,-c(6,7,12)]

cores_personalizadas <- c("darkgrey", "red", "LightSkyBlue", "DarkOrchid", "gold") #ar, fortshade, fortsun, pgshade, pgsun

resum_temp_hora %>% #linhas com área preenchida
  ggplot(aes(x = hora, y = tmean, group=sensor, color=sensor, fill = sensor)) +
  geom_line(cex = 1.2) +
  geom_ribbon(aes(ymin = tmean - desvio,
                  ymax = tmean + desvio), alpha = 0.13, col = FALSE) +
  facet_wrap(~season) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_color_manual(values = cores_personalizadas) +  # Define cores personalizadas para as linhas
  scale_fill_manual(values = cores_personalizadas)

fsun_pre_v<- temperatura %>% 
  filter(!sensor == 'ar',
         !sensor == 'FORTSHADE',
         !sensor == "PGSHADE",
         !sensor == 'PGSUN',
         season == 'verao')

unique(fsun_pre_v$ano)

quantile<- temperatura %>%  #obtendo o 50º quantil de cada dia
  group_by(data, sensor, site, season) %>% 
  dplyr::summarise(quantile50 = quantile(temp, probs = 0.5)) %>%
  data.frame()

temperatura<- left_join(temperatura, quantile %>% select(sensor, data, season, quantile50), by = c("data", 'sensor', 'season')) #juntando em um só dataframe

temp_quantil50<- temperatura %>% #filtrando as horas em que a temp > 50º quantil
  filter(temp > quantile50)

temp_param<- temperatura %>%  #parametros da curva de aquecimento
  group_by(data, sensor, site, season) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE)) %>%
  data.frame()

temp_quantil50<- left_join(temp_quantil50, temp_param %>% select(sensor, data, season, tmax), by = c("data", 'sensor', 'season')) #juntando em um só dataframe

temp_quantil_filt<- temp_quantil50 %>%
  group_by(sensor, data, season, ano, quantile50) %>%
  filter(hora >= 6)

##nao ta funcionando direito... seria melhor fazer isso que filtrar ali em cima
teste<- temp_quantil_filt %>%
  group_by(sensor, data, season, ano, quantile50) %>%
  filter(!all(diff(hora) != 1))

dados_selecionados <- temp_quantil_filt %>%
  group_by(sensor, data, season, ano, quantile50) %>%
  filter(hora == min(hora) | temp == max(temp)) %>%
  ungroup()

#onset_rate <- dados_selecionados %>%
#  group_by(sensor, data, season, ano) %>%
#  slice_min(order_by = hora, n = 2) %>%
#  ungroup()

onset_rate<- dados_selecionados %>%   
  group_by(sensor, data, season, ano) %>%
  dplyr::summarise(t_peak = max(temp),
                   t_posquantil = min(temp),
                   h_peak = max(hora),
                   h_posquantil = min(hora)) %>%
  data.frame()

#unique(onset_rate$h_posquantil)

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
  group_by(sensor, season) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

onset_year<- onset_rate %>%
  group_by(sensor, season, ano) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

# Season
#onset verao
onset_verao<- onset_rate %>% 
  filter(season == 'verao')

plot_on_v<- onset_verao %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  ggtitle('Verao') +
  theme_classic()

plot_int_v<- onset_verao %>% 
  ggplot(aes(x = sensor, y = t_posquantil)) +
  geom_boxplot(outlier.shape = NA) +
  #ggtitle('Verao') +
  theme_classic()

plot_on_v + plot_int_v

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

kruskal.test(h_posquantil~sensor, data = onset_verao)
in_v<- pairwise.wilcox.test(onset_verao$h_posquantil,
                            onset_verao$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
in_v$p.value
(in_v_a <- melt(in_v$p.value))
in_v.cc  <-  na.omit(in_v_a)
in_v.pvals  <-  in_v.cc[, 3]
names(in_v.pvals)  <-  paste(in_v.cc[, 1], in_v.cc[, 2], sep="-")
multcompLetters(in_v.pvals)

#onset primavera
onset_prima<- onset_rate %>% 
  filter(season == 'primavera')

plot_on_p<- onset_prima %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  ggtitle('Primavera') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_prima)
on_p<- pairwise.wilcox.test(onset_prima$onset_rate,
                            onset_prima$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_p$p.value
(on_p_a <- melt(on_p$p.value))
on_p.cc  <-  na.omit(on_p_a)
on_p.pvals  <-  on_p.cc[, 3]
names(on_p.pvals)  <-  paste(on_p.cc[, 1], on_p.cc[, 2], sep="-")
multcompLetters(on_p.pvals)  

#onset outono
onset_outono<- onset_rate %>% 
  filter(season == 'outono')

teste<- onset_outono %>%
  group_by(sensor, season) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

plot_on_o<- onset_outono %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  ggtitle('Outono') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_outono)
on_o<- pairwise.wilcox.test(onset_outono$onset_rate,
                            onset_outono$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_o$p.value
(on_o_a <- melt(on_o$p.value))
on_o.cc  <-  na.omit(on_o_a)
on_o.pvals  <-  on_o.cc[, 3]
names(on_o.pvals)  <-  paste(on_o.cc[, 1], on_o.cc[, 2], sep="-")
multcompLetters(on_o.pvals) 

#onset inverno
onset_inverno<- onset_rate %>% 
  filter(season == 'inverno')

plot_on_i<- onset_inverno %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot(outlier.shape = NA) +
  ggtitle('Inverno') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_inverno)
on_i<- pairwise.wilcox.test(onset_inverno$onset_rate,
                            onset_inverno$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_i$p.value
(on_i_a <- melt(on_i$p.value))
on_i.cc  <-  na.omit(on_i_a)
on_i.pvals  <-  on_i.cc[, 3]
names(on_i.pvals)  <-  paste(on_i.cc[, 1], on_i.cc[, 2], sep="-")
multcompLetters(on_i.pvals) 

plot_on_p + plot_on_v + plot_on_o + plot_on_i


#Variação da onset rate ao longo dos ano
onset_verao %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2019,2023.5)) +
  ggtitle('Taxa de Aquecimento - Verão') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_verao %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  scale_x_continuous(limits = c(2019,2023.5)) +
  ggtitle('Intercepto - Verão') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

v_ar<- onset_verao %>% 
  filter(sensor == 'ar')

kruskal.test(onset_rate~ano, data = v_ar) #sem dif onset
kruskal.test(t_posquantil~ano, data = v_ar) #com dif intercepto

k_int_v_ar<- pairwise.wilcox.test(v_ar$t_posquantil,
                                  v_ar$ano,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_v_ar$p.value
(k_ar_v <- melt(k_int_v_ar$p.value))
k_ar_v.cc  <-  na.omit(k_ar_v)
k_ar_v.pvals  <-  k_ar_v.cc[, 3]
names(k_ar_v.pvals)  <-  paste(k_ar_v.cc[, 1], k_ar_v.cc[, 2], sep="-")
multcompLetters(k_ar_v.pvals) 

v_fsh<- onset_verao %>% 
  filter(sensor == 'FORTSHADE')

kruskal.test(onset_rate~ano, data = v_fsh) #sem dif onset
kruskal.test(t_posquantil~ano, data = v_fsh) #com dif intercepto

v_fsun<- onset_verao %>% 
  filter(sensor == 'FORTSUN')

kruskal.test(onset_rate~ano, data = v_fsun) #sem dif onset
kruskal.test(t_posquantil~ano, data = v_fsun) #com dif intercepto

k_v_fsun<- pairwise.wilcox.test(v_fsun$onset_rate,
                                v_fsun$ano,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_v_fsun$p.value
(k_fsh_v <- melt(k_int_v_fsh$p.value))
k_fsh_v.cc  <-  na.omit(k_fsh_v)
k_fsh_v.pvals  <-  k_fsh_v.cc[, 3]
names(k_fsh_v.pvals)  <-  paste(k_fsh_v.cc[, 1], k_fsh_v.cc[, 2], sep="-")
multcompLetters(k_fsh_v.pvals)







onset_prima %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Taxa de Aquecimento - Primavera') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_prima %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Intercepto - Primavera') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_inverno %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Taxa de Aquecimento - Inverno') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_inverno %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Intercepto - Inverno') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_outono %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Taxa de Aquecimento - Outono') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

onset_outono %>% 
  filter(!sensor == 'PGSHADE',
         !sensor == "PGSUN") %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ sensor) +
  theme_classic() +
  scale_x_continuous(limits = c(2018.5,2023.5)) +
  ggtitle('Intercepto - Outono') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))


#Variação da onset rate ao longo dos ano
on_ar<- onset_rate %>% 
  filter(sensor == "ar")

plot_on_ar<- on_ar %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Ar') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

a<- on_ar %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Ar') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#sem diferença ao longo dos anos para nenhuma das estações do ano
on_ar_v<- onset_verao %>% 
  filter(sensor == 'ar')

kruskal.test(onset_rate~ano, data = on_ar_v) #sem dif onset
kruskal.test(t_posquantil~ano, data = on_ar_v) #com dif intercepto

k_int_ar_v<- pairwise.wilcox.test(on_ar_v$t_posquantil,
                                  on_ar_v$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_ar_v$p.value
(ar_v <- melt(k_int_ar_v$p.value))
ar_v.cc  <-  na.omit(ar_v)
ar_v.pvals  <-  ar_v.cc[, 3]
names(ar_v.pvals)  <-  paste(ar_v.cc[, 1], ar_v.cc[, 2], sep="-")
multcompLetters(ar_v.pvals) 

on_ar_v %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))


on_ar_p<- onset_prima %>% 
  filter(sensor == 'ar')

kruskal.test(onset_rate~ano, data = on_ar_p) #sem dif onset
kruskal.test(t_posquantil~ano, data = on_ar_p) #com dif intercepto

k_int_ar_p<- pairwise.wilcox.test(on_ar_p$t_posquantil,
                                  on_ar_p$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_ar_p$p.value
(ar_p <- melt(k_int_ar_p$p.value))
ar_p.cc  <-  na.omit(ar_p)
ar_p.pvals  <-  ar_p.cc[, 3]
names(ar_p.pvals)  <-  paste(ar_p.cc[, 1], ar_p.cc[, 2], sep="-")
multcompLetters(ar_p.pvals) 

on_ar_p %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))


on_ar_i<- onset_inverno %>% 
  filter(sensor == 'ar')

kruskal.test(onset_rate~ano, data = on_ar_i) #sem dif onset
kruskal.test(t_posquantil~ano, data = on_ar_i) #com dif intercepto

k_int_ar_i<- pairwise.wilcox.test(on_ar_i$t_posquantil,
                                  on_ar_i$ano,
                                  p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_ar_i$p.value
(ar_i <- melt(k_int_ar_i$p.value))
ar_i.cc  <-  na.omit(ar_i)
ar_i.pvals  <-  ar_i.cc[, 3]
names(ar_i.pvals)  <-  paste(ar_i.cc[, 1], ar_i.cc[, 2], sep="-")
multcompLetters(ar_i.pvals) 

on_ar_i %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

on_ar_o<- onset_outono %>% 
  filter(sensor == 'ar')

kruskal.test(onset_rate~ano, data = on_ar_o) #com dif onset rate
kruskal.test(t_posquantil~ano, data = on_ar_o) # sem dif intercepto

k_on_ar_o<- pairwise.wilcox.test(on_ar_o$onset_rate,   ###### ta dando errado! como se nao tivesse diferença significativa
                                 on_ar_o$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_on_ar_o$p.value
(on_o <- melt(k_on_ar_o$p.value))
on_o.cc  <-  na.omit(on_o)
on_o.pvals  <-  on_o.cc[, 3]
names(on_o.pvals)  <-  paste(on_o.cc[, 1], on_o.cc[, 2], sep="-")
multcompLetters(on_o.pvals) 

on_ar_o %>% 
  group_by(ano) %>% 
  summarise(min = mean(onset_rate))






teste<- onset_rate %>% 
  filter(sensor == 'FORTSHADE',
         season == 'verao')

unique(teste$ano)

unique(onset_rate$sensor)


#Fortshade
on_fortshade<- onset_rate %>% 
  filter(sensor == "FORTSHADE")

plot_on_fortshade<- on_fortshade %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Fortaleza Sombra') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

on_fortshade %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Fortaleza Sombra') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#verao
on_fshade_v<- onset_verao %>% 
  filter(sensor == 'FORTSHADE')


on_fshade_v %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

kruskal.test(onset_rate~ano, data = on_fshade_v) #sem dif onset
kruskal.test(t_posquantil~ano, data = on_fshade_v) #com dif intercepto

k_int_fsh_v<- pairwise.wilcox.test(on_fshade_v$t_posquantil,
                                    on_fshade_v$ano,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsh_v$p.value
(i_v <- melt(k_int_fsh_v$p.value))
i_v.cc  <-  na.omit(i_v)
i_v.pvals  <-  i_v.cc[, 3]
names(i_v.pvals)  <-  paste(i_v.cc[, 1], i_v.cc[, 2], sep="-")
multcompLetters(i_v.pvals) 

#primavera
on_fshade_p<- onset_prima %>% 
  filter(sensor == 'FORTSHADE')

on_fshade_p %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

unique(on_fshade_p$ano)

kruskal.test(onset_rate~ano, data = on_fshade_p) #onset
kruskal.test(t_posquantil~ano, data = on_fshade_p) #intercepto

k_int_fsh_p<- pairwise.wilcox.test(on_fshade_p$t_posquantil,
                                    on_fshade_p$ano,
                                    p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsh_p$p.value
(i_p <- melt(k_int_fsh_p$p.value))
i_p.cc  <-  na.omit(i_p)
i_p.pvals  <-  i_p.cc[, 3]
names(i_p.pvals)  <-  paste(i_p.cc[, 1], i_p.cc[, 2], sep="-")
multcompLetters(i_p.pvals) 

#outono
on_fshade_o<- onset_outono %>% 
  filter(sensor == 'FORTSHADE')

on_fshade_o %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

unique(on_fshade_o$ano)

kruskal.test(onset_rate~ano, data = on_fshade_o) #onset rate
kruskal.test(t_posquantil~ano, data = on_fshade_o) # dif intercepto

k_int_fsh_o<- pairwise.wilcox.test(on_fshade_o$t_posquantil,
                                   on_fshade_o$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsh_o$p.value
(i_o <- melt(k_int_fsh_o$p.value))
i_o.cc  <-  na.omit(i_o)
i_o.pvals  <-  i_o.cc[, 3]
names(i_o.pvals)  <-  paste(i_o.cc[, 1], i_o.cc[, 2], sep="-")
multcompLetters(i_o.pvals) 

#inverno
on_fshade_i<- onset_inverno %>% 
  filter(sensor == 'FORTSHADE')

unique(on_fshade_i$ano)

kruskal.test(onset_rate~ano, data = on_fshade_i) #onset rate
kruskal.test(t_posquantil~ano, data = on_fshade_i) #intercepto
k_int_fsh_i<- pairwise.wilcox.test(on_fshade_i$t_posquantil,
                                    on_fshade_i$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsh_i$p.value
(i_i <- melt(k_int_fsh_i$p.value))
i_i.cc  <-  na.omit(i_i)
i_i.pvals  <-  i_i.cc[, 3]
names(i_i.pvals)  <-  paste(i_i.cc[, 1], i_i.cc[, 2], sep="-")
multcompLetters(i_i.pvals) 

on_fshade_i %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

#Fortsun
on_fortsun<- onset_rate %>% 
  filter(sensor == 'FORTSUN')

plot_on_fortsun<- on_fortsun %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Fortaleza Sol') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

on_fortsun %>% 
  ggplot(aes(x = ano, y = t_posquantil, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Fortaleza Sol') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))


#todos tiveram alteração ao longo dos anos
#verao
on_fsun_v<- onset_verao %>% 
  filter(sensor == 'FORTSUN')

unique(on_fsun_v$ano)

kruskal.test(onset_rate~ano, data = on_fsun_v) #onset rate

k_on_fsun_v<- pairwise.wilcox.test(on_fsun_v$onset_rate,
                                     on_fsun_v$ano,
                                     p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_on_fsun_v$p.value
(y_v <- melt(k_on_fsun_v$p.value))
y_v.cc  <-  na.omit(y_v)
y_v.pvals  <-  y_v.cc[, 3]
names(y_v.pvals)  <-  paste(y_v.cc[, 1], y_v.cc[, 2], sep="-")
multcompLetters(y_v.pvals) 

on_fsun_v %>% 
  group_by(ano) %>% 
  summarise(min = mean(onset_rate))

kruskal.test(t_posquantil~ano, data = on_fsun_v) #sem dif intercepto

#prima
on_fsun_p<- onset_prima %>% 
  filter(sensor == 'FORTSUN')

unique(on_fsun_p$ano)

on_fsun_p %>% 
  group_by(ano) %>% 
  summarise(min = mean(onset_rate))

kruskal.test(onset_rate~ano, data = on_fsun_p) #dif onset 
k_on_fsun_p<- pairwise.wilcox.test(on_fsun_p$onset_rate,
                                   on_fsun_p$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_on_fsun_p$p.value
(y_p <- melt(k_on_fsun_p$p.value))
y_p.cc  <-  na.omit(y_p)
y_p.pvals  <-  y_p.cc[, 3]
names(y_p.pvals)  <-  paste(y_p.cc[, 1], y_p.cc[, 2], sep="-")
multcompLetters(y_p.pvals) 

kruskal.test(t_posquantil~ano, data = on_fsun_p) #intercepto
k_int_fsun_p<- pairwise.wilcox.test(on_fsun_p$t_posquantil,
                                   on_fsun_p$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsun_p$p.value
(i_p <- melt(k_int_fsun_p$p.value))
i_p.cc  <-  na.omit(i_p)
i_p.pvals  <-  i_p.cc[, 3]
names(i_p.pvals)  <-  paste(i_p.cc[, 1], i_p.cc[, 2], sep="-")
multcompLetters(i_p.pvals)

#outono
on_fsun_o<- onset_outono %>% 
  filter(sensor == 'FORTSUN')

unique(on_fsun_o$ano)

kruskal.test(onset_rate~ano, data = on_fsun_o) #onset 

k_on_fsun_o<- pairwise.wilcox.test(on_fsun_o$onset_rate,
                                   on_fsun_o$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_on_fsun_o$p.value
(y_o <- melt(k_on_fsun_o$p.value))
y_o.cc  <-  na.omit(y_o)
y_o.pvals  <-  y_o.cc[, 3]
names(y_o.pvals)  <-  paste(y_o.cc[, 1], y_o.cc[, 2], sep="-")
multcompLetters(y_o.pvals)

kruskal.test(t_posquantil~ano, data = on_fsun_o) #sem dif intercepto

#inverno
on_fsun_i<- onset_inverno %>% 
  filter(sensor == 'FORTSUN')

unique(on_fsun_i$ano)

kruskal.test(onset_rate~ano, data = on_fsun_i)

on_fsun_i %>% 
  group_by(ano) %>% 
  summarise(min = mean(onset_rate))

k_on_fsun_i<- pairwise.wilcox.test(on_fsun_i$onset_rate,
                                   on_fsun_i$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_on_fsun_i$p.value
(y_i <- melt(k_on_fsun_i$p.value))
y_i.cc  <-  na.omit(y_i)
y_i.pvals  <-  y_i.cc[, 3]
names(y_i.pvals)  <-  paste(y_i.cc[, 1], y_i.cc[, 2], sep="-")
multcompLetters(y_i.pvals)

kruskal.test(t_posquantil~ano, data = on_fsun_i)
k_int_fsun_i<- pairwise.wilcox.test(on_fsun_i$t_posquantil,
                                   on_fsun_i$ano,
                                   p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
k_int_fsun_i$p.value
(y_int_i <- melt(k_int_fsun_i$p.value))
y_int_i.cc  <-  na.omit(y_int_i)
y_int_i.pvals  <-  y_int_i.cc[, 3]
names(y_int_i.pvals)  <-  paste(y_int_i.cc[, 1], y_int_i.cc[, 2], sep="-")
multcompLetters(y_int_i.pvals)

on_fsun_i %>% 
  group_by(ano) %>% 
  summarise(min = mean(t_posquantil))

plot_on_fortsun + plot_on_fortshade #sem dif intercepto

#PGshade
on_pgshade<- onset_rate %>% 
  filter(sensor == "PGSHADE")

plot_on_pgshade<- on_pgshade %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Praia Grande Sombra') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#sem diferença ao longo dos anos para nenhuma das estações do ano
on_pgshade_v<- onset_verao %>% 
  filter(sensor == 'PGSHADE')

unique(on_pgshade_v$ano)

kruskal.test(onset_rate~interaction(ano), data = on_pgshade_v)

on_pgshade_p<- onset_prima %>% 
  filter(sensor == 'PGSHADE')

unique(on_pgshade_p$ano)

kruskal.test(onset_rate~interaction(ano), data = on_pgshade_p) 

on_pghade_o<- onset_outono %>% 
  filter(sensor == 'PGSHADE')

unique(on_pghade_o$ano)

kruskal.test(onset_rate~interaction(ano), data = on_pghade_o) 

on_pghade_i<- onset_inverno %>% 
  filter(sensor == 'PGSHADE')

unique(on_pghade_i$ano)

kruskal.test(onset_rate~interaction(ano), data = on_pghade_i) 

#pgsun
on_pgsun<- onset_rate %>% 
  filter(sensor == 'PGSUN')

plot_on_pgsun<- on_pgsun %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  #ylim(0,3.5) +
  ggtitle('Praia Grande Sol') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#sem anos suficientes para comparação
on_pgsun_v<- onset_verao %>% 
  filter(sensor == 'PGSUN')

unique(on_pgsun_v$ano)

on_pgsun_p<- onset_prima %>% 
  filter(sensor == 'PGSUN')

unique(on_pgsun_p$ano)

on_pgsun_o<- onset_outono %>% 
  filter(sensor == 'PGSUN')

unique(on_pgsun_o$ano)

on_pgsun_i<- onset_inverno %>% 
  filter(sensor == 'PGSUN')

unique(on_pgsun_i$ano)

#fazer para PG mas acho que nao tem ano suficiente








mean_temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE)) %>%
  data.frame()

temperatura %>% 
  group_by(sensor, season) %>% 
  dplyr::summarise(mean_temp = mean(temp, na.rm = TRUE),
                   desvio_temp = sd(temp, na.rm = TRUE),
                   max_temp = max(temp, na.rm = TRUE),
                   index = (mean_temp*1.5))%>%
  data.frame()


temp_hora<- temperatura %>%
  group_by(sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   lower = tmean-desvio,
                   upper = tmean+desvio,
                   trange = tmax - tmin) %>%
  data.frame()

resum_temp_hora<- temp_hora[,-c(6,7,12)]

cores_personalizadas <- c("darkgreen", "red", "LightSkyBlue", "DarkOrchid", "gold") #ar, fortshade, fortsun, pgshade, pgsun

resum_temp_hora %>% #linhas com área preenchida
  ggplot(aes(x = hora, y = tmean, group=sensor, color=sensor, fill = sensor)) +
  geom_line(cex = 1.2) +
  geom_ribbon(aes(ymin = tmean - desvio,
                  ymax = tmean + desvio), alpha = 0.13, col = FALSE) +
  facet_wrap(~season) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  scale_color_manual(values = cores_personalizadas) +  # Define cores personalizadas para as linhas
  scale_fill_manual(values = cores_personalizadas)

## tava fazneod intervalo entre min e max temp
#temp_hora %>% #novo em linhas 
#  ggplot(aes(x = hora, y = mean_temp, group=sensor_stress, color=sensor_stress)) +
#  geom_line() +
#  geom_point() +
#  geom_errorbar(aes(ymin=mean_temp-desvio_temp, ymax=mean_temp+desvio_temp), width=.2,
#                position=position_dodge(0.05)) + #com desvio padrão
#  facet_grid(season ~ site) +
#  theme_classic() +
#  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

temperatura$dias_do_ano <- yday(temperatura$data)


temp_hora_days<- temperatura %>%
  group_by(dias_do_ano, sensor, sensor_stress, hora, site, season) %>% 
  dplyr::summarise(tmax = max(temp,na.rm = TRUE),
                   tmin = min(temp,na.rm = TRUE),
                   tmean = mean(temp,na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   trange = tmax - tmin) %>%
  data.frame()

dias_do_ano <- temperatura %>% 
  filter(site == 'Fortaleza')

dias_do_ano<- dias_do_ano[,c(-2, -6, -8)]

temp_hora_days <- temp_hora_days %>% 
  filter(site == 'Fortaleza')

resultado <- dias_do_ano %>%
  left_join(temp_hora_days, by = c("dias_do_ano", "hora", "sensor", 'season', 'site', 'sensor_stress')) %>%
  mutate(nova_coluna = temp - tmean)

min(resultado$nova_coluna)
max(resultado$nova_coluna)

ggplot(resultado, aes(x = hora, y = ano,  fill = nova_coluna, group = hora)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = 'Dia do ano', y = "Ano") +
  facet_grid(~ sensor) +
  theme_classic()


### Temperatura por estação do ano
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

#Tentando plotar todos juntos (erro)
#temp média
resum_temp_perday<- temp_perday[,-c(2,3,5,6,7,8,9,14)]

fortsun<- resum_temp_perday %>% 
  filter(sensor == 'FORTSUN')
fortsun<- fortsun[,-c(2,4)]
names(fortsun) <- c('data', 'tmean_fortsun', 'lower_fortsun', 'upper_fortsun')

fortshade<- resum_temp_perday %>% 
  filter(sensor == 'FORTSHADE')
fortshade<- fortshade[,-c(2,4)]
names(fortshade) <- c('data', 'tmean_fortshade', 'lower_fortshade', 'upper_fortshade')

pgsun<- resum_temp_perday %>% 
  filter(sensor == 'PGSUN')
pgsun<- pgsun[,-c(2,4)]
names(pgsun) <- c('data', 'tmean_pgsun', 'lower_pgsun', 'upper_pgsun')

pgshade<- resum_temp_perday %>% 
  filter(sensor == 'PGSHADE')
pgshade<- pgshade[,-c(2,4)]
names(pgshade) <- c('data', 'tmean_pgshade', 'lower_pgshade', 'upper_pgshade')

fort<- fortshade %>% 
  full_join(fortsun)

pg<- pgshade %>% 
  full_join(pgsun)

graph_temp_perday<- fort %>% 
  full_join(pg)

graph_temp_perday<- xts(x = graph_temp_perday[,-1], order.by = graph_temp_perday$data)
graph_temp_perday<- dygraph(graph_temp_perday, main = "Sensores", 
                            ylab = "Temperatura média (ºC)") #%>%
#dySeries(c("lower", "tmean", "upper"))   #NAO TA INTERVALO DE CONFIANÇA, TA MIN E MAX
graph_temp_perday 

teste<- graph_temp_perday[,-c(3,4,6,7,9,10,12,13)]
teste<- xts(x = teste[,-1], order.by = teste$data)
teste<- dygraph(teste, main = "Sensores", 
                ylab = "Temperatura média (ºC)") 
teste
#falta ic de cada sensor

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

# como trabalhar com os máximos? outliers?
# relevância biológica disso?

############## testando um trem
# média por dia (geral - dia 24/06 por ex, não dia esp)

##### HEATWAVE PACKAGE: DETECTANDO ONDAS DE CALOR
# APENAS FORTALEZA
temp_fort<- temperatura %>% 
  filter(site == 'Fortaleza')

temp_fort_resum<- temp_fort %>% 
  group_by(data, sensor, site, season, sensor_stress) %>% 
  dplyr::summarise(tmean = mean(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmax = max(temp, na.rm = TRUE))

names(temp_fort_resum) <- c('t', 'sensor', 'site', 'season', 'sensor_stress', 'tmean', 
                            'tmin', 'tmax')


#write.csv(temp_fort_resum, "teamu", row.names = F) #salvando como csv

ft_sun<- temp_fort_resum %>% 
  filter(sensor == 'FORTSUN')

ft_sun<- ft_sun %>% 
  filter(!t < '2019-08-01')

ft_sun <- ft_sun %>%
  mutate(tmean = ifelse(between(t, as.Date('2021-12-15'), as.Date('2022-07-20')), NA, tmean))

max(ft_sun$t)
min(ft_sun$t)

# CLIMATOLOGIA
ts2_sun<- ts2clm(
  ft_sun,
  x = t,
  y = tmean,
  climatologyPeriod = c("2019-08-01", "2023-07-08"),  #ta dando como se a data final fosse em 2022
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

ggplot(heatmap, aes(x = t, y = '',  fill = nova_coluna)) +
  geom_tile() +
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"))+
  labs(x = '', y = "Variação entre climatologia e temperatura registrada") +
  ggtitle("Fortaleza sol") +
  theme_classic()

#clim_sun<- ggplot(ts2_sun, aes(x=t, y=tmean)) +
#  geom_line() +
#  ylim(17, 30) +
#  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
#  geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=4, color = 'orange') +
#  xlab("Data") +
#  ylab("Temperatura média (ºC)") +
#  ggtitle("Fortaleza Sol") +
#  theme_classic()


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

climatology_sun$preenchimento <- ifelse(climatology_sun$tmean > climatology_sun$thresh & climatology_sun$tmean < climatology_sun$thres2, "preenchimento_condicional", "nao_preencher")

teste_sun <- ft_sun %>%
  select(t , tmean) 

teste_sun$sensor<- 'FORTSUN'

#Ver como adc legenda!! azul: climatologia, preto: tmean, dourado grosso: threshold, dourado pontilhado: 2x threshold
climsun<- ggplot(climatology_sun, aes(x=t)) +
  geom_line(aes(y = tmean), color = "black", lwd=0.5) +
  geom_line(aes(y = seas), color="steelblue", linetype=1, lwd=0.5) +
  geom_line(aes(y = thresh), color='GoldenRod', linetype = 1, lwd=0.5) +
  geom_line(aes(y = thres2), color='GoldenRod', linetype = 2, lwd = 0.3) +
  geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=3, color = 'DimGray') +
  #geom_ribbon(aes(ymin = thresh, ymax = tmean, fill = preenchimento), alpha = 0.5,) +
  #scale_fill_manual(values = c("preenchimento_condicional" = "yellow", "nao_preencher" = "white")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  ylim(18,30) +
  #geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=3, color = 'DimGray') +
  xlab("Data") +
  ylab("Temperatura média (ºC)") +
  ggtitle("Fortaleza Sol") +
  theme_classic()

df_sun <- climatology_sun %>%
  select(t , tmean, seas, thresh, thres2) %>%
  gather(key = "variable", value = "value", -t )

df_sun <- df_sun %>%
  mutate(ano = lubridate::year(t))

years<- unique(df_sun$ano)

for(i in years) {
  yFortsun <- df_sun %>% 
    filter(ano == i) %>% ggplot(aes(x = t, y = value)) + 
    scale_x_date(date_breaks = "1 months", date_labels = "%b/%y") +
    geom_line(aes(color = variable)) + 
    geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=3, color = 'DimGray') +
    ylim(18,30) +
    scale_color_manual(values = c("steelblue", "yellow", 'GoldenRod', 'black'),
                       labels = c("Climatologia", "Dobro do Threshold", 'Threshold', 'Temperatura Média')) +
    ggtitle('Fortaleza sol', i) +
    labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
    theme_classic()
  print(yFortsun)
}

yearmean_sun<- block_average(detect_sun, x = t, y = tmean, report = "full")
summary(glm(count ~ year, yearmean_sun, family = "poisson"))

cat_sun<- category(
  detect_sun,
  y = tmean,
  S = TRUE,
  name = "Event",
  climatology = FALSE,
  MCScorrect = F,
  season = "range",
  roundVal = 4
)

freq_cat_sun<- count(cat_sun, category)

loli_sun<- event_sun %>% 
  ggplot(aes(x = date_peak, y = intensity_max, col = duration)) + geom_point(size = 2) + 
  geom_segment(aes(x = date_peak, xend = date_peak, yend = intensity_max, y = 0)) + 
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20), breaks = seq(5, 20, by = 5)) +
  xlab("") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sol') +
  theme_classic()


ggplot(event_sun, aes(x = date_peak, y = duration)) +    
  geom_lolli(aes(colour = intensity_max)) +
  scale_color_distiller(palette = "Spectral", name = "Intensidade Máxima") +
  xlab("Data") + ylab("Duração") +
  ggtitle('Fortaleza sol') +
  theme_classic()

ggplot(event_sun, aes(x = date_peak, y = intensity_max)) +    
  geom_lolli(aes(colour = duration)) +
  scale_color_distiller(palette = "Spectral", name = "Duração") +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sol') +
  theme_classic()

ggplot(event_shade, aes(x = date_peak, y = duration)) +    
  geom_lolli(aes(colour = intensity_max)) +
  scale_color_distiller(palette = "Spectral", name = "Intensidade Máxima") +
  xlab("Data") + ylab("Duração") +
  ggtitle('Fortaleza sombra') +
  theme_classic()

ggplot(event_shade, aes(x = date_peak, y = intensity_max)) +    
  geom_lolli(aes(colour = duration)) +
  scale_color_distiller(palette = "Spectral", name = "Duração") +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sombra') +
  theme_classic()

ft_shade<- temp_fort_resum %>% 
  filter(sensor == 'FORTSHADE')

ft_shade <- ft_shade %>%
  mutate(tmean = ifelse(between(t, as.Date('2020-08-01'), as.Date('2021-08-20')), NA, tmean))

max(ft_shade$t)
min(ft_shade$t)

ts2_shade<- ts2clm(
  ft_shade,
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

ggplot(heatmap_shade, aes(x = t, y = '',  fill = nova_coluna)) +
  geom_tile() +
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"))+
  labs(x = '', y = "Variação entre a climatologia e a temperatura registrada") +
  ggtitle('Fortaleza sombra')+
  theme_classic()

#clim_shade<- ggplot(ts2_shade, aes(x=t, y=tmean)) +
#  geom_line() +
#  ylim(17, 30) +
#  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
#  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 424, 571, 611, 665, 712, 1196, 1370)]), linetype=4, color = 'orange') +
#  xlab("Data") +
#  ylab("Temperatura média (ºC)") +
#  ggtitle("Fortaleza Sombra") +
#  theme_classic()

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

teste_shade <- ft_shade %>%
  select(t , tmean)

teste_shade$sensor<- 'FORTSHADE'

climatology_shade$dif<- (climatology_shade$thresh - climatology_shade$seas)
climatology_shade$dif_2<- 2*climatology_shade$dif
climatology_shade$dif_3<- 3*climatology_shade$dif
climatology_shade$dif_4<- 4*climatology_shade$dif
climatology_shade$thres2<- climatology_shade$seas + climatology_shade$dif_2
climatology_shade$thres3<- climatology_shade$seas + climatology_shade$dif_3
climatology_shade$thres4<- climatology_shade$seas + climatology_shade$dif_4

climatology_shade$preenchimento <- ifelse(climatology_shade$tmean > climatology_shade$thresh & climatology_shade$tmean < climatology_shade$thres2, "preenchimento_condicional", "nao_preencher")


#Ver como adc legenda!! azul: climatologia, preto: tmean, dourado grosso: threshold, dourado pontilhado: 2x threshold
climshade<- ggplot(climatology_shade, aes(x=t)) +
  geom_line(aes(y = tmean), color = "black", lwd=0.5) +
  geom_line(aes(y = seas), color="steelblue", linetype=1, lwd=0.5) +
  geom_line(aes(y = thresh), color='GoldenRod', linetype = 1, lwd=0.5) +
  geom_line(aes(y = thres2), color='GoldenRod', linetype = 2, lwd = 0.3) +
  #geom_ribbon(aes(ymin = thresh, ymax = tmean, fill = preenchimento), alpha = 0.5,) +
  #scale_fill_manual(values = c("preenchimento_condicional" = "yellow", "nao_preencher" = "white")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 424, 571, 611, 665, 712, 1196, 1370)]), linetype=3, color = 'DimGray') +
  ylim(18,30) +
  xlab("Data") +
  ylab("Temperatura média (ºC)") +
  ggtitle("Fortaleza Sombra") +
  theme_classic()

df_shade <- climatology_shade %>%
  select(t , tmean, seas, thresh, thres2) %>%
  gather(key = "variable", value = "value", -t )

df_shade <- df_shade %>%
  mutate(ano = lubridate::year(t))

for(i in years) {
  yFortshade <- df_shade %>% 
    filter(ano == i) %>% ggplot(aes(x = t, y = value)) + 
    scale_x_date(date_breaks = "1 months", date_labels = "%b/%y") +
    geom_line(aes(color = variable)) + 
    geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 424, 571, 611, 665, 712, 1196, 1370)]), linetype=3, color = 'DimGray') +
    ylim(18,30) +
    scale_color_manual(values = c("steelblue", "yellow", 'GoldenRod', 'black'),
                       labels = c("Climatologia", "Dobro do Threshold", 'Threshold', 'Temperatura Média')) +
    ggtitle('Fortaleza sombra', i) +
    labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
    theme_classic()
  print(yFortshade)
}

#"black", lwd=0.5) +
 # geom_line(aes(y = seas), color="steelblue", linetype=1, lwd=0.5) +
  #geom_line(aes(y = thresh), color='GoldenRod', linetype = 1, lwd=0.5) +
  #geom_line(aes(y = thres2), color='GoldenRod', linetype = 2, lwd = 0.3)

yearmean_shade<- block_average(detect_shade, x = t, y = tmean, report = "full")

cat_shade<- category(
  detect_shade,
  y = tmean,
  S = TRUE,
  name = "Event",
  climatology = FALSE,
  MCScorrect = F,
  season = "range",
  roundVal = 4
)

freq_cat_shade<- count(cat_shade, category)

loli_shade<- event_shade %>% 
  ggplot(aes(x = date_peak, y = intensity_max, col = duration)) + geom_point(size = 2) + 
  geom_segment(aes(x = date_peak, xend = date_peak, yend = intensity_max, y = 0)) + 
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20), breaks = seq(5, 20, by = 5)) +
  ylim(0,4) +
  xlab("") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sombra') +
  theme_classic()


loli_sun + loli_shade


#filtrado!
#sun<- climatology_sun %>% 
#  filter(!t < '2019-08-01')

#sun<- climatology_sun %>% 
#  filter(!between(t, as.Date('2021-12-01'), as.Date('2022-07-20')))

#sun <- sun %>%
#  mutate(tmean = ifelse(between(t, as.Date('2021-12-15'), as.Date('2022-07-20')), NA, tmean))

#teste_sun <- sun %>%
 # select(t , tmean) 

#teste_sun$sensor<- 'FORTSUN'

#shade<- climatology_shade %>% 
#  filter(!between(t, as.Date('2020-07-01'), as.Date('2021-08-01')))

#shade <- climatology_shade %>%
 # mutate(tmean = ifelse(between(t, as.Date('2020-08-01'), as.Date('2021-08-20')), NA, tmean))

#teste_shade <- shade %>%
 # select(t , tmean)

#teste_shade$sensor<- 'FORTSHADE'

climatology_general<- teste_sun %>% 
  full_join(teste_shade)

todas_as_datas <- data.frame(t = seq(min(climatology_general$t), max(climatology_general$t), by = "1 day"))

df_completado <- todas_as_datas %>%
  left_join(climatology_general, by = "t")

#df_completado <- df_completado %>%
 # pivot_wider(names_from = sensor, values_from = tmean)

#colnames(df_completado) <- c("t", "temperatura_sol", "temperatura_sombra", "site", "season")

df_completado %>% 
  ggplot(aes(x = t, y = tmean, group = sensor, color = sensor)) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  geom_line(aes(color = sensor)) + 
  geom_vline(xintercept=as.numeric(ts2_sun$t[c(96, 209, 404, 1229, 1256, 1379, 1421)]), linetype=4, color = 'OrangeRed') +
  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 1196, 1370)]), linetype=4, color = 'BlueViolet') +
  ylim(18,30) +
  scale_color_manual(values = c("CadetBlue", "Chocolate"),
                     labels = c("Sombra", "Sol")) +
  #ggtitle('Fortaleza sol') +
  labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
  theme_classic()






# erro
event_line(detect_sun, x = t, y = tmean, spread = 150, metric = intensity_cumulative,
           start_date = '2019-06-16', end_date = '2023-07-08')


str(teste$t)
teste<- make_whole(ts2_sun, x = t, y = tmean)

ggplot(climatology_sun, aes(x = t, y = tmean)) +
  geom_flame(aes(y2 = thresh)) +
  geom_text(aes(x = as.Date("2019-06-16"), y = 28,
                label = "That's not a heatwave.\nThis, is a heatwave.")) +
  xlab("Date") + ylab(expression(paste("Temperature [", degree, "C]")))






#########################################################################
# nao usados mas nao quero perder
library (installr)
updateR() 

ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res <- detect_event(ts)
event_line(res, spread = 100, metric = duration,
           start_date = "2010-12-01", end_date = "2011-06-30")


tmean<- xyplot(tmean ~ t, climatology_sun, ylim = c(15,17.5,20,22.5,25,27.5,30,32.5), ylab = 'Temperatura média (ºC)', type = "l" , lwd=1, col="black")
clim<- xyplot(seas ~ t, climatology_sun, ylim = c(15,17.5,20,22.5,25,27.5,30,32.5), type = "l" , lwd=0.3, col="blue")
thres<- xyplot(thresh ~ t, climatology_sun, ylim = c(15,17.5,20,22.5,25,27.5,30,32.5), type = "l" , lwd=0.3, col="orange")

a<- doubleYScale(tmean, clim, add.ylab2 = TRUE, use.style=FALSE )

doubleYScale(a, thres, text = c("Climatologia", "Threshold"), col = c("blue", "orange"), add.ylab2 = FALSE, use.style=FALSE )


#library
install.packages('latticeExtra')
library(latticeExtra)

set.seed(1)
x <- 1:100
var1 <- cumsum(rnorm(100))
var2 <- var1^2
data <- data.frame(x,var1,var2)

# --> construct separate plots for each series
obj1 <- xyplot(var1 ~ x, data, type = "l" , lwd=2, col="steelblue")
obj2 <- xyplot(var2 ~ x, data, type = "l", lwd=2, col="#69b3a2")

# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )

############################################################

sun<- climatology_sun %>% 
  filter(!t < '2019-08-01')

#sun<- climatology_sun %>% 
#  filter(!between(t, as.Date('2021-12-01'), as.Date('2022-07-20')))

sun <- sun %>%
   mutate(tmean = ifelse(between(t, as.Date('2021-12-15'), as.Date('2022-07-20')), NA, tmean))

teste_sun <- sun %>%
  select(t , tmean) 

teste_sun$sensor<- 'FORTSUN'

#shade<- climatology_shade %>% 
#  filter(!between(t, as.Date('2020-07-01'), as.Date('2021-08-01')))

shade <- climatology_shade %>%
  mutate(tmean = ifelse(between(t, as.Date('2020-08-01'), as.Date('2021-08-20')), NA, tmean))

teste_shade <- shade %>%
  select(t , tmean)
  
teste_shade$sensor<- 'FORTSHADE'

teste<-  teste_sun %>% 
  full_join(teste_shade)

teste %>% 
  ggplot(aes(x = t, y = tmean)) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  geom_line(aes(color = sensor)) + 
  geom_vline(xintercept=as.numeric(ts2_sun$t[c(96, 209, 404, 1229, 1256, 1379, 1421)]), linetype=4, color = 'OrangeRed') +
  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 1196, 1370)]), linetype=4, color = 'BlueViolet') +
  ylim(18,30) +
  scale_color_manual(values = c("LightBlue", "IndianRed"),
                     labels = c("Sombra", "Sol")) +
  #ggtitle('Fortaleza sol') +
  labs(x = '', y = 'Temperatura ºC', color = 'Legenda:') +
  theme_classic()


temp_ar <- read.csv('TempAr_mar2023.csv', sep = ';') %>% 
  mutate(data = as.Date(data), 
         mes = month(data)) %>% 
  mutate(season = case_when(mes %in% 1:3 ~ "verao",
                            mes %in% 4:6 ~ "outono",
                            mes %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno"))) %>% 
  mutate(tmax = as.numeric(tmax),
         tmin = as.numeric(tmin),
         tmean = as.numeric(tmean))



#TEMP AR
temp_ar <- read.csv('TempAr_nov2023.csv', sep = ';') %>% 
  mutate(data = dmy(data),
         mes = month(data),
         ano = year(data)) %>% 
  mutate(season = case_when(mes %in% 1:3 ~ "verao",
                            mes %in% 4:6 ~ "outono",
                            mes %in% 7:9 ~ "inverno",
                            TRUE ~ "primavera") %>% 
           factor(., levels = c("primavera", "verao", "outono", "inverno")))

temp_ar$temp <- as.numeric(temp_ar$temp)
temp_ar$hora <- as.numeric(temp_ar$hora)

temp_ar<- temp_ar %>% # tirando os NA's
  filter(!temp %in% NA)

temp_ar$sensor<- "ar"
temp_ar$site<- "ar"

a <- merge(temperatura, temp_ar, by = c("sensor", "temp", "data", "hora", "site", "mes", "ano", "season"), all = TRUE)

### Plotando gráficos
# Temperatura ao longo do dia
temp_ar %>% #original em boxplot
  ggplot(aes(x = hora, y = temperatura, group = hora)) +
  geom_boxplot() +
  facet_grid(~season) +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

### ONSET RATE
# como definir?
# acima 90º percentil? 50º percentil?
# olhômetro?
# (temp no pico - temp h1)/nº horas entre temp no pico e temp h1

quantile_ar<- temp_ar %>%  #obtendo o 50º quantil de cada dia
  group_by(data, season) %>% 
  dplyr::summarise(quantile50 = quantile(temperatura, probs = 0.5)) %>%
  data.frame()

temp_ar<- left_join(temp_ar, quantile_ar %>% select(data, season, quantile50), by = c("data", 'season')) #juntando em um só dataframe

temp_quantil50_ar<- temp_ar %>% #filtrando as horas em que a temp > 50º quantil
  filter(temperatura > quantile50)

temp_param_ar<- temp_ar %>%  #parametros da temp
  group_by(data, season) %>% 
  dplyr::summarise(tmax = max(temperatura, na.rm = TRUE),
                   tmean = mean(temperatura, na.rm = TRUE),
                   tmin = min(temperatura, na.rm = TRUE)) %>%
  data.frame()

temp_quantil50_ar<- left_join(temp_quantil50_ar, temp_param_ar %>% select(data, season, tmax), by = c("data", 'season')) #juntando em um só dataframe

dados_selecionados_ar <- temp_quantil50_ar %>%
  group_by(data, season, ano, quantile50) %>%
  filter(hora == min(hora) | temperatura == max(temperatura)) %>%
  ungroup()

onset_rate_ar <- dados_selecionados_ar %>%
  group_by(data, season, ano) %>%
  slice_min(order_by = hora, n = 2) %>%
  ungroup()

onset_rate_ar<- onset_rate_ar %>%
  group_by(data, season, ano) %>%
  dplyr::summarise(t_peak = max(temperatura),
                   t_posquantil = min(temperatura),
                   h_peak = max(hora),
                   h_posquantil = min(hora)) %>%
  data.frame()

onset_rate_ar$numerador<- onset_rate_ar$t_peak - onset_rate_ar$t_posquantil 
onset_rate_ar$denominador<- abs(onset_rate_ar$h_peak - onset_rate_ar$h_posquantil)
onset_rate_ar$onset_rate<- onset_rate_ar$numerador/onset_rate_ar$denominador

onset_year_ar<- onset_rate_ar %>%
  group_by(ano, season) %>% 
  dplyr::summarise(onset_mean = mean(onset_rate, na.rm = TRUE),
                   onset_desvio = sd(onset_rate, na.rm = TRUE)) %>%
  data.frame()

# Season
#onset verao
onset_verao_ar<- onset_rate_ar %>% 
  filter(season == 'verao')

plot_on_v_ar<- onset_verao_ar %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot() +
  ggtitle('Verao - ar') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_verao_ar)
on_v_ar<- pairwise.wilcox.test(onset_verao_ar$onset_rate,
                            onset_verao_ar$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_v$p.value
(on_v_a <- melt(on_v$p.value))
on_v.cc  <-  na.omit(on_v_a)
on_v.pvals  <-  on_v.cc[, 3]
names(on_v.pvals)  <-  paste(on_v.cc[, 1], on_v.cc[, 2], sep="-")
multcompLetters(on_v.pvals)  

#onset primavera
onset_prima<- onset_rate %>% 
  filter(season == 'primavera')

plot_on_p<- onset_prima %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot() +
  ggtitle('Primavera') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_prima)
on_p<- pairwise.wilcox.test(onset_prima$onset_rate,
                            onset_prima$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_p$p.value
(on_p_a <- melt(on_p$p.value))
on_p.cc  <-  na.omit(on_p_a)
on_p.pvals  <-  on_p.cc[, 3]
names(on_p.pvals)  <-  paste(on_p.cc[, 1], on_p.cc[, 2], sep="-")
multcompLetters(on_p.pvals)  

#onset outono
onset_outono<- onset_rate %>% 
  filter(season == 'outono')

plot_on_o<- onset_outono %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot() +
  ggtitle('Outono') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_outono)
on_o<- pairwise.wilcox.test(onset_outono$onset_rate,
                            onset_outono$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_o$p.value
(on_o_a <- melt(on_o$p.value))
on_o.cc  <-  na.omit(on_o_a)
on_o.pvals  <-  on_o.cc[, 3]
names(on_o.pvals)  <-  paste(on_o.cc[, 1], on_o.cc[, 2], sep="-")
multcompLetters(on_o.pvals) 

#onset inverno
onset_inverno<- onset_rate %>% 
  filter(season == 'inverno')

plot_on_i<- onset_inverno %>% 
  ggplot(aes(x = sensor, y = onset_rate)) +
  geom_boxplot() +
  ggtitle('Inverno') +
  theme_classic()

kruskal.test(onset_rate~sensor, data = onset_inverno)
on_i<- pairwise.wilcox.test(onset_inverno$onset_rate,
                            onset_inverno$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
on_i$p.value
(on_i_a <- melt(on_i$p.value))
on_i.cc  <-  na.omit(on_i_a)
on_i.pvals  <-  on_i.cc[, 3]
names(on_i.pvals)  <-  paste(on_i.cc[, 1], on_i.cc[, 2], sep="-")
multcompLetters(on_i.pvals) 

plot_on_p + plot_on_v + plot_on_o + plot_on_i




ts2_ar<- ts2clm(
  temp_ar,
  x = data,
  y = tmax,
  climatologyPeriod = c("2019-06-16", "2023-03-13"),
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

Clima_ar<- ggplot(ts2_ar, aes(x=data, y=tmax)) +
  geom_line() +
  ylim(15, 50) +
  xlab("Data") +
  ylab("Temperatura (ºC)") +
  ggtitle("Climatologia Ar")

Clima_ar

Clima_ar + Clima_rocha


ggplot() +
  geom_line(data=ts2_ar, aes(x=data, y=tmax), color= "pink") + 
  geom_line(data=ts2_ft2, aes(x=data, y=tmax), color = "brown")


####### MONITORAMENTO DA COMUNIDADE #######

### chamando dataframe
entremares <- readxl::read_xlsx("monit_entremares_julho2023_datasjutaselitoigual.xlsx") %>% 
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

# entremares$relative_cover<- as.numeric(entremares$relative_cover) #transformando em numérico
# str(entremares$relative_cover)

# entremares$density_025m2<- as.numeric(entremares$density_025m2) #transformando em numérico
# str(entremares$density_025m2)

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
  select(locality, eventDate, data, tideHeight, season, quadrat, motile, density_025m2, year) %>% 
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
  group_by(locality, eventDate, tideHeight, season, quadrat, year) %>% 
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
# fazer anova com isso?

densidade_dom <- c('Echinolittorina lineolata', 'Fissurella rosea', 'Lottia subrugosa', 'Stramonita haemastoma')

### obtendo as médias para boxplot
# móveis
density_perday<- entremares_zeros %>%   #Por 0.25m²
  filter(!motile %in% NA) %>%
  group_by(eventDate, locality, season, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_0.25m2 = mean(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   sd_density_0.25m2 = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

#pro primer
wide_entremares_zeros <- dcast(entremares_zeros, eventDate + year + season + tideHeight + quadrat ~ motile, value.var="density_025m2", sum)

wide_entremares_zeros$amostra <- paste(wide_entremares_zeros$eventDate,wide_entremares_zeros$season,wide_entremares_zeros$tideHeight,wide_entremares_zeros$quadrat, sep = "_")

rownames(wide_entremares_zeros) <- wide_entremares_zeros$amostra #amostra

wide_entremares_zeros<- wide_entremares_zeros[,-c(1,2,3,4,5,16)]

write.csv(wide_entremares_zeros, file = "densidade_primer.csv", row.names = TRUE)

density<- entremares_zeros %>%  #Por 0.25m²
  filter(!motile %in% NA) %>%
  group_by(locality, season, tideHeight, motile, year) %>% 
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
  facet_grid(tideHeight ~ locality, scales = "free_y") +
  theme_bw()

density_fort<- density_perday %>% 
  filter(locality == 'Fortaleza',
         motile %in% densidade_dom) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x = motile, y = mean_density_0.25m2)) + 
  geom_boxplot() + 
  ggtitle('Fortaleza') +
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ season, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", angle = 90)) +
  labs(x = "Organismos móveis", y = "Densidade por 0.25m²")

unique(entremares$motile)

entremares_zeros_year <- entremares_zeros %>%
  group_by(locality, season, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_year = mean(density_025m2, na.rm = TRUE),
                   sd_density_year = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

## Deixando apenas os mais dominantes nos dataframes para estatística
density_perday_dom<- density_perday %>% 
  filter(motile %in% densidade_dom)

density_peryear_dom<- entremares_zeros_year %>% 
  filter(motile %in% densidade_dom)

# sésseis
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


#pro primer
entremares_cover<- entremares %>% 
  filter(!locality == 'Prainha')

entremares_cover<- entremares_cover[,c(11,13,14,18,19,22,24)]

wide_entremares_cover <- dcast(entremares_cover, eventDate + year + season + tideHeight + quadrat ~ type_cover, value.var="relative_cover", sum)
wide_entremares_cover[is.na(wide_entremares_cover)] <- 0
wide_entremares_cover$amostra <- paste(wide_entremares_cover$eventDate,wide_entremares_cover$season,wide_entremares_cover$tideHeight,wide_entremares_cover$quadrat, sep = "_")

rownames(wide_entremares_cover) <- wide_entremares_cover$amostra #amostra

wide_entremares_cover<- wide_entremares_cover[,-c(1,2,3,4,5,16,22,29)]

write.csv(wide_entremares_cover, file = "cobertura_primer.csv", row.names = TRUE)

#voltando pro R
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

unique(cover_peryear_dom$type_cover)

#### MULTIVARIADA COMUNIDADE
#### ENTENDER MELHOR A MULTIVARIADA
#library(vegan)
#library(reshape2)
# COBERTURA
#transformando em wide
wide_cover <- dcast(cover_perday_dom, eventDate + year + locality + season + tideHeight ~ type_cover, value.var="mean", sum)

names(wide_cover) <- c('eventDate', 'year', 'locality', 'season', 'tideHeight', 'ACA', 'bare_rock', 
                       'C_bisinuatus', 'crostosa', 'Cyanophyceae', 'M_solisianus', 'outra_macroalga', 'T_stalactifera')


## PERMDISP
#CONFERIR
dis <- vegdist(wide_cover %>% data.frame() %>% select(ACA:T_stalactifera), method = "euclidian", na.rm = TRUE)
#sites <- wide_cover$locality
#dates<- wide_cover$eventDate
season<- wide_cover$season
anos<- wide_cover$year

## Calculate multivariate dispersions
mod <- betadisper(dis, season)
mod1 <- betadisper(dis, anos)

## Perform test
anova(mod)
anova(mod1)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 999)
permutest(mod1, pairwise = TRUE, permutations = 999)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

(mod1.HSD <- TukeyHSD(mod1))
plot(mod1.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)
plot(mod1)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

plot(mod1, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod1, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

plot(mod1, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

boxplot(mod1)

## Using group centroids
mod3 <- betadisper(dis, season, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))


mod2 <- betadisper(dis, anos, type = "centroid")
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))

# SEM DISPERSAO MULTIVARIADA, PASSEMOS A PERMANOVA
library(GGally)

## Euclidian distances between samples (?) é mesmo? melhor bray! botei bray agr

#tirando raiz quadrada
wide_cover[,6:ncol(wide_cover)]<- sqrt(wide_cover[,6:ncol(wide_cover)])

ggpairs(wide_cover[,1:5]) #ver correlações

adonis2(wide_cover[,6:ncol(wide_cover)] ~ year * season, wide_cover, strata = wide_cover$tideHeight, perm=4999, method = "bray", na.rm = TRUE)
pairwiseAdonis::pairwise.adonis2(wide_cover[,6:ncol(wide_cover)] ~ year * season, data = wide_cover, strata = 'tideHeight', na.rm = TRUE)
#wide_cover <- na.omit(wide_cover)
simper(wide_cover[,6:ncol(wide_cover)], wide_cover$year)
simper(wide_cover[,6:ncol(wide_cover)], wide_cover$season)

# DENSIDADE
#transformando em wide
#ver dps de pairwise ta usando bray ou euclidian
wide_den <- dcast(density_perday_dom, eventDate + year + locality + season + tideHeight ~ motile, value.var="mean_density_0.25m2", sum)

names(wide_den) <- c('eventDate', 'year', 'locality', 'season', 'tideHeight', 'E_lineolata', 'F_rosea', "L_subrugosa", 'S_haemastoma')

wide_den[,6:ncol(wide_den)]<- sqrt(wide_den[,6:ncol(wide_den)])

library(GGally)

#ggpairs(wide_den[,1:5]) #ver correlações

adonis2(wide_den[,6:ncol(wide_den)] ~ year * season, wide_den, strata = wide_den$tideHeight, method = 'bray', perm=4999)
pairwiseAdonis::pairwise.adonis2(wide_den[,6:ncol(wide_den)] ~ year * season, data = wide_den, strata = 'tideHeight', method = 'bray', na.rm = TRUE)
simper(wide_den[,6:ncol(wide_den)], wide_den$season)
#simper(wide_den[,6:ncol(wide_den)], wide_den$locality)
simper(wide_den[,6:ncol(wide_den)], wide_den$year)

#tentando fazer nMDS
unique(wide_den$E_lineolata)
unique(wide_den$F_rosea)
unique(wide_den$L_subrugosa)
unique(wide_den$S_haemastoma)

info_bio_den<- (wide_den)[,6:ncol(wide_den)]
info_env_den<- (wide_den)[,1:5]
info_env_den$year <- as.factor(info_env_den$year)

nome_linha <- paste(info_bio_den$eventDate)

dissim<- vegdist(info_bio_den, method = 'bray')
str(dissim)

nmds_result<- metaMDS(dissim, distance = 'bray', k = 2, trymax = 100)

nmds_result_with_info <- scores(nmds_result, display = "sites")

cores <- rainbow(length(levels(info_env_den$season)))
simbolos <- 1:length(levels(info_env_den$year))

ordiplot(nmds_result_with_info, type = "n")
points(
  nmds_result_with_info,
  col = cores[as.factor(info_env_den$season)],
  pch = simbolos[as.factor(info_env_den$year)],
  cex = 0.8)

text(
  nmds_result_with_info,
  labels = info_env_den$eventDate,  # Substitua pelo seu vetor real de eventDate
  col = cores[as.factor(info_env_den$season)],
  pos = 3,  # Ajuste conforme necessário para posicionar os rótulos
  cex = 0.8)

legend(
  "topleft",  # Ajuste conforme necessário para a posição da legenda
  legend = levels(as.factor(info_env_den$year)),
  #col = cores,
  pch = simbolos,
  title = "Season and Year"
)

legend(
  "topright",  # Ajuste conforme necessário para a posição da legenda
  legend = levels(as.factor(info_env_den$season)),
  col = cores,
  pch = simbolos,
  title = "Season and Year"
)

#### DADOS POPULACIONAIS
## Para móveis: Lottia subrugosa e Echinollitorina lineolata
# Para cover: C bisinuatus, M solisianus, T stalactifera
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

modelo_mean <- lm(mean_density_0.25m2 ~ tmean_month, data = e_lit_high)
summary(modelo_mean)

#modelo_mean <- lm(mean_density_0.25m2 ~ tmean_month + I(tmean_month^2), data = e_lit_high)
#summary(modelo_mean)

modelo_max <- lm(mean_density_0.25m2 ~ tmax_mean_month, data = e_lit_high)
summary(modelo_max)

#modelo_max <- lm(mean_density_0.25m2 ~ tmax_mean_month + I(tmax_mean_month^2), data = e_lit_high)
#summary(modelo_max)

modelo_cv <- lm(mean_density_0.25m2 ~ cv_mean_month, data = e_lit_high)
summary(modelo_cv)

#modelo_cv <- lm(mean_density_0.25m2 ~ cv_mean_month + I(cv_mean_month^2), data = e_lit_high)
#summary(modelo_cv)

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

ggplot(l_sub_low, aes(x = tideHeight, y = mean_density_0.25m2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "L.subrugosa - inferior", x = "Temperatura", y = "Densidade") +
  theme_classic()

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
  facet_grid(tideHeight ~ locality) +
  theme_classic()

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

modelo_mean <- lm(mean ~ tmean_month, data = tetra_low)
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

#ROCHA
rock_low<- cover_with_temp %>% 
  filter(type_cover== 'Bare rock',
         tideHeight == 'inferior')

ggpairs(rock_low[,c(4,6,8,10,16)])

rock_mid<- cover_with_temp %>% 
  filter(type_cover== 'Bare rock',
         tideHeight == 'médio')

ggpairs(rock_mid[,c(4,6,8,10,16)])

rock_high<- cover_with_temp %>% 
  filter(type_cover== 'Bare rock',
         tideHeight == 'superior')

ggpairs(rock_high[,c(4,6,8,10,16)])

rock_mean<- ggplot(rock_low, aes(x = tmean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rocha - inferior", x = "Temperatura média", y = "Densidade") +
  theme_classic()

rock_max<- ggplot(rock_low, aes(x = tmax_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rocha - inferior", x = "Temperatura máxima", y = "Densidade") +
  theme_classic()

rock_cv<- ggplot(rock_low, aes(x = cv_mean_month, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rocha - inferior", x = "Amplitude térmica (CV)", y = "Densidade") +
  theme_classic()

rock_mean + rock_max + rock_cv

modelo_mean <- lm(mean ~ tmean_month, data = rock_low)
summary(modelo_mean)

## funcionando mas meu R ta bugado e nao consigo ver todos os plots
for(i in densidade_dom) {
  bFort <- density_peryear_dom %>% 
    filter(motile == i,
           locality == 'Fortaleza') %>% 
    ggplot(aes(x=year, y=mean_density_year)) +
    geom_line() +
    geom_point()+
    ggtitle(i) +
    xlab("ano") +
    ylab("densidade (ind.0,25"~ m^-2~ ")")+
    facet_grid(tideHeight ~ locality + season )
  print(bFort)
}

## Cobertura
cover_perday_dom$eventDate<- as.Date(cover_perday_dom$eventDate)

cover_perday_dom %>% #BAGUNÇA!!!!
  filter(!type_cover == 'ACA', 
         !type_cover == 'Crostosa', 
         !type_cover == 'Cyanophyceae', 
         !type_cover == 'Outra macroalga') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean, group=type_cover, color=type_cover, shape = type_cover)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean-desvio, ymax = mean+desvio), position = position_dodge(width=0.7)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("Cobertura (%)")+
  facet_grid(tideHeight ~ locality) +
  theme_classic()

#### Populações - estatística circular
library(Directional)
library(circular)
library(CircMLE)

l_sub_low<- entremares_zeros %>% 
  filter(motile == 'Lottia subrugosa',
         locality == 'Fortaleza',
         tideHeight == 'inferior')

#por season
l_sub_low_season<- l_sub_fort_low %>%  
  group_by(season, tideHeight, motile) %>% 
  dplyr::summarise(mean_density_025m2 = mean(density_025m2, na.rm = TRUE), 
                   sd_density_025m2 = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

360/4 #4 estações

l_sub_low_season$angulo[l_sub_low_season$season == 'verao'] <- '0'
l_sub_low_season$angulo[l_sub_low_season$season == 'outono'] <- '90'
l_sub_low_season$angulo[l_sub_low_season$season == 'inverno'] <- '180'
l_sub_low_season$angulo[l_sub_low_season$season == 'primavera'] <- '270'

l_sub_low_season$angulo<- as.numeric(l_sub_low_season$angulo)
str(l_sub_low_season$angulo)

#transformando em rad
l_sub_low_season$angulos_radians <- rad(l_sub_low_season$angulo)
str(l_sub_low_season$angulos_radians)

#l_sub_fort_low<- l_sub_fort_low[,c(5,6,7,9,10,11)]

#transformando em circular
l_sub_low_season$angulo <- circular(l_sub_low_season$angulo, units = "degrees", template = "none")
str(l_sub_low_season$angulo) #em graus

l_sub_low_season$angulos_radians <- circular(l_sub_low_season$angulos_radians, units = "rad", template = "none")
str(l_sub_low_season$angulos_radians) #em rad

ang_l_sub_low_season <- l_sub_low_season %>%
  rowwise() %>%
  do(data.frame(angulos = rep(.$angulos_radians, .$mean_density_025m2)))

plot.circular(ang_l_sub_low_season,
              axes=F,
              ticks = F,
              stack = T,
              main="Lottia subrugosa",
              col.main = 'black',
              sep=0.08,
              shrink = 2,
              col='black',
              zero = 2*pi)

rose.diag(ang_l_sub_low_season, 
          bins = 8,
          axes = F,
          ticks = F,
          rotation = 'clock',
          border = 'olivedrab',
          col = 'yellowgreen',
          zero = 2*pi,
          add = T)

mean.circular(ang_l_sub_low_season)
rho.circular(ang_l_sub_low_season)
sd.circular(ang_l_sub_low_season)
