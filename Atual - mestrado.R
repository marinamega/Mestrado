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

entremares_zeros <- entremares %>%
  select(locality, eventDate, data, tideHeight, season, quadrat, motile, density_025m2, year) %>% 
  filter(!is.na(density_025m2)) %>%
  pivot_wider(names_from = motile, values_from = density_025m2) %>% 
  pivot_longer(cols = `Fissurella rosea`:`Onchidella indolens`, names_to = "motile", values_to = "density_025m2") %>%
  mutate(density_025m2 = as.numeric(density_025m2)) %>% 
  mutate_all(., ~replace_na(.,0)) 

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


### obtendo as médias para boxplot
# móveis
density_perday<- entremares_zeros %>%   #Por 0.25m²
  filter(!motile %in% NA) %>%
  group_by(eventDate, locality, season, tideHeight, motile, year) %>% 
  dplyr::summarise(mean_density_0.25m2 = mean(density_025m2, na.rm = TRUE), #por soma dividir por 10 quadrados pra ter a densidade e depois usar
                   sd_density_0.25m2 = sd(density_025m2, na.rm = TRUE)) %>%
  data.frame()

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

densidade_dom <- c('Echinolittorina lineolata', 'Fissurella rosea', 'Lottia subrugosa', 'Stramonita haemastoma')

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

density_prainha<- density_perday %>% 
  filter(locality == 'Prainha',
         motile %in% densidade_dom) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x = motile, y = mean_density_0.25m2)) + 
  geom_boxplot() + 
  ggtitle('Prainha') +
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ season, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", angle = 90)) +
  labs(x = "Organismos móveis", y = "Densidade por 0.25m²")

density_fort + density_prainha

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

cover_prainha<- cover_perday %>% 
  filter(locality == 'Prainha',
         type_cover %in% cover_dom) %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x = type_cover, y = mean)) + 
  geom_boxplot() +
  ggtitle('Prainha') +
  scale_fill_viridis_d() + 
  facet_grid(tideHeight ~ season, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", angle = 90)) +
  labs(x = "Organismos sésseis", y = "Cobertura (%)")

cover_fort + cover_prainha

## Deixando apenas os de maior cobertura para estatística
cover_perday_dom<- cover_perday %>% 
  filter(type_cover %in% cover_dom) 

cover_peryear_dom<- cover %>% 
  filter(type_cover %in% cover_dom) 

unique(cover_peryear_dom$type_cover)

#### MULTIVARIADA COMUNIDADE
#### ENTENDER MELHOR A MULTIVARIADA
library(vegan)
library(reshape2)

# COBERTURA
#transformando em wide
wide_cover <- dcast(cover_perday_dom, eventDate + year + locality + season + tideHeight ~ type_cover, value.var="mean", sum)

names(wide_cover) <- c('eventDate', 'year', 'locality', 'season', 'tideHeight', 'ACA', 'bare_rock', 
                     'C_bisinuatus', 'crostosa', 'Cyanophyceae', 'M_solisianus', 'outra_macroalga', 'T_stalactifera')


## PERMDISP
dis <- vegdist(wide_cover %>% data.frame() %>% select(ACA:T_stalactifera), method = "euclidian", na.rm = TRUE)
sites <- wide_cover$locality
dates<- wide_cover$eventDate
season<- wide_cover$season

## Calculate multivariate dispersions
mod <- betadisper(dis, dates)

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 999)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## Using group centroids
mod3 <- betadisper(dis, dates, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))

# SEM DISPERSAO MULTIVARIADA, PASSEMOS A PERMANOVA
## Euclidian distances between samples

adonis2(wide_cover[,5:ncol(wide_cover)] ~ season * tideHeight, wide_cover, strata = wide_cover$locality, perm=4999, na.rm = TRUE)
pairwise.adonis2(wide_cover[,5:ncol(wide_cover)] ~ season * tideHeight, data = wide_cover, na.rm = TRUE)

adonis2(wide_cover[,6:ncol(wide_cover)] ~ year * tideHeight, wide_cover, strata = wide_cover$locality, perm=4999, na.rm = TRUE)

# DENSIDAE
#transformando em wide
wide_den <- dcast(density_perday_dom, eventDate + year + locality + season + tideHeight ~ motile, value.var="mean_density_0.25m2", sum)

names(wide_den) <- c('eventDate', 'year', 'locality', 'season', 'tideHeight', 'E_lineolata', 'F_rosea', "L_subrugosa", 'S_haemastoma')

adonis2(wide_den[,6:ncol(wide_den)] ~ season * tideHeight, wide_den, strata = wide_den$locality, perm=4999)
pairwise.adonis2(wide_den[,6:ncol(wide_den)] ~ season * tideHeight, data = wide_den, na.rm = TRUE)

adonis2(wide_den[,6:ncol(wide_den)] ~ year * tideHeight, wide_den, strata = wide_den$locality, perm=4999)
pairwise.adonis2(wide_den[,6:ncol(wide_den)] ~ year * tideHeight, data = wide_den, na.rm = TRUE)

simper(wide_den[,6:ncol(wide_den)], wide_den$year)
simper(wide_den[,6:ncol(wide_den)], wide_den$tideHeight)


# simper
simper(wide_den[,5:ncol(wide_den)], wide_den$season)
simper(wide_den[,5:ncol(wide_den)], wide_den$tideHeight)


#### DADOS POPULACIONAIS
density_perday_dom$eventDate<- as.Date(density_perday_dom$eventDate)

density_perday_dom %>% 
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
  filter(motile == 'Fissurella rosea') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=tideHeight, color=tideHeight)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  ggtitle('Fissurella rosea') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_wrap(~locality) +
  theme_classic()

density_perday_dom %>% 
  filter(motile == 'Stramonita haemastoma') %>% 
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean_density_0.25m2, group=tideHeight, color=tideHeight)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = mean_density_0.25m2-sd_density_0.25m2, ymax = mean_density_0.25m2+sd_density_0.25m2), position = position_dodge(width=0.7)) +
  ggtitle('Stramonita haemastoma') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("densidade (ind/0.25m2)")+
  facet_wrap(~locality) +
  theme_classic()

density_perday_dom %>% 
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

cover_perday_dom %>% #linhas com área preenchida
  filter(type_cover == 'Chthamalus bisinuatus') %>%
  mutate(tideHeight = factor(tideHeight, levels = c("superior", "médio", "inferior"))) %>% 
  ggplot(aes(x=eventDate, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean-desvio, ymax = mean+desvio), alpha = 0.2, col = FALSE) +
  ggtitle('Chthamalus bisinuatus') +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  xlab("") +
  ylab("Cobertura (%)")+
  facet_grid(tideHeight~ locality) +
  theme_classic()

####### DADOS DE TEMPERATURA (SENSORES NA ROCHA) #######
library(scales)
library(heatwaveR)
library(dygraphs)
library(xts) 
library(reshape)
library(RmarineHeatWaves)
library(multcompView)

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

resum_temp_hora %>% #linhas com área preenchida
  ggplot(aes(x = hora, y = tmean, group=sensor, color=sensor, fill = sensor)) +
  geom_line() +
  geom_ribbon(aes(ymin = tmean - desvio,
                  ymax = tmean + desvio), alpha = 0.2, col = FALSE) +
  facet_wrap(~season) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

min_temp<- temperatura %>% 
  filter(hora < 13) %>% 
  group_by(dia, sensor, season) %>% 
  slice(which.min(temp))
min_temp<- min_temp[,-c(2,5,6,9,11)]
names(min_temp) <- c('sensor', 'tmin', 'site', 'hora_tmin', 'data', 'season')

max_temp<- temperatura %>% 
  filter(hora > 10) %>% 
  group_by(dia, sensor, season) %>% 
  slice(which.max(temp))
max_temp<- max_temp[,-c(2,5,6,9,11)]
names(max_temp) <- c('sensor', 'tmax', 'site', 'hora_tmax', 'data', 'season')

heat_interval<- min_temp %>% 
  full_join(max_temp)

heat_interval<- heat_interval %>%
  group_by(data, sensor, tmax, site, hora_tmax, season, tmin, hora_tmin) %>% 
  dplyr::summarise(heat_interval = abs(hora_tmin - hora_tmax),
                   onset_rate = (tmax- tmin)/(hora_tmax - hora_tmin)) %>%
  data.frame()

heat_interval<- heat_interval %>% 
  filter(!is.na(onset_rate),
         onset_rate >= 0)

unique(heat_interval$onset_rate)

#Onset Rate
heat_interval$sensor_stress[heat_interval$sensor == 'FORTSHADE'] <- 'sombra'
heat_interval$sensor_stress[heat_interval$sensor == 'FORTSUN'] <- 'sol'
heat_interval$sensor_stress[heat_interval$sensor == 'PGSHADE'] <- 'sombra'
heat_interval$sensor_stress[heat_interval$sensor == 'PGSUN'] <- 'sol'

heat_interval<- heat_interval %>% 
mutate(ano = year(data))

onset_rate<- heat_interval %>%
  group_by(sensor, site, season, ano) %>% 
  dplyr::summarise(mean_onset_rate = mean(onset_rate, na.rm = TRUE)) %>%
  data.frame()
#write.csv(onset_rate, "onset_rate_set2023.csv", row.names = F) #salvando como csv

#onset verao
verao_heat_interval<- heat_interval %>% 
  filter(season == 'verao')

on_verao<- verao_heat_interval %>% #original em boxplot
  ggplot(aes(x = sensor_stress, y = onset_rate, group = sensor_stress)) +
  geom_boxplot(aes(color = sensor_stress)) +
  ggtitle('Verão') +
  facet_grid(~site) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

hist(verao_heat_interval$onset_rate)

kruskal.test(onset_rate~sensor, data = verao_heat_interval)
v_on<- pairwise.wilcox.test(verao_heat_interval$onset_rate,
                          verao_heat_interval$sensor,
                          p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
v_on$p.value
(v_on_a <- melt(v_on$p.value))
v_on_a.cc  <-  na.omit(v_on_a)
v_on_a.pvals  <-  v_on_a.cc[, 3]
names(v_on_a.pvals)  <-  paste(v_on_a.cc[, 1], v_on_a.cc[, 2], sep="-")
multcompLetters(v_on_a.pvals)

#onset primavera
prima_heat_interval<- heat_interval %>% 
  filter(season == 'primavera')

on_prima<-prima_heat_interval %>% #original em boxplot
  ggplot(aes(x = sensor_stress, y = onset_rate, group = sensor_stress)) +
  geom_boxplot(aes(color = sensor_stress)) +
  ggtitle('Primavera') +
  facet_grid(~site) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

kruskal.test(onset_rate~sensor, data = prima_heat_interval)
p_on<- pairwise.wilcox.test(prima_heat_interval$onset_rate,
                            prima_heat_interval$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
p_on$p.value
(p_on_a <- melt(p_on$p.value))
p_on_a.cc  <-  na.omit(p_on_a)
p_on_a.pvals  <-  p_on_a.cc[, 3]
names(p_on_a.pvals)  <-  paste(p_on_a.cc[, 1], p_on_a.cc[, 2], sep="-")
multcompLetters(p_on_a.pvals)

#onset inverno
inverno_heat_interval<- heat_interval %>% 
  filter(season == 'inverno')

on_inverno<-inverno_heat_interval %>% #original em boxplot
  ggplot(aes(x = sensor_stress, y = onset_rate, group = sensor_stress)) +
  geom_boxplot(aes(color = sensor_stress)) +
  ggtitle('Inverno') +
  facet_grid(~site) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

kruskal.test(onset_rate~sensor, data = inverno_heat_interval)
i_on<- pairwise.wilcox.test(inverno_heat_interval$onset_rate,
                            inverno_heat_interval$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
i_on$p.value
(i_on_a <- melt(i_on$p.value))
i_on_a.cc  <-  na.omit(i_on_a)
i_on_a.pvals  <-  i_on_a.cc[, 3]
names(i_on_a.pvals)  <-  paste(i_on_a.cc[, 1], i_on_a.cc[, 2], sep="-")
multcompLetters(i_on_a.pvals)

#onset outono
outono_heat_interval<- heat_interval %>% 
  filter(season == 'outono')

on_outono<-outono_heat_interval %>% #original em boxplot
  ggplot(aes(x = sensor_stress, y = onset_rate, group = sensor_stress)) +
  geom_boxplot(aes(color = sensor_stress)) +
  ggtitle('Outono') +
  facet_grid(~site) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

kruskal.test(onset_rate~sensor, data = outono_heat_interval)
o_on<- pairwise.wilcox.test(outono_heat_interval$onset_rate,
                            outono_heat_interval$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
o_on$p.value
(o_on_a <- melt(o_on$p.value))
o_on_a.cc  <-  na.omit(o_on_a)
o_on_a.pvals  <-  o_on_a.cc[, 3]
names(o_on_a.pvals)  <-  paste(o_on_a.cc[, 1], o_on_a.cc[, 2], sep="-")
multcompLetters(o_on_a.pvals)

multcompLetters(v_on_a.pvals)
multcompLetters(p_on_a.pvals)
multcompLetters(i_on_a.pvals)
multcompLetters(o_on_a.pvals)

on_verao + on_prima + on_inverno + on_outono + plot_layout(guides = "collect")#plotando todos juntos

#onset rate sensor ao longo anos
heat_interval %>% 
  filter(sensor == 'FORTSUN') %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  ylim(0,3.5) +
  ggtitle('Fortaleza Sol') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#FORTSUN
#verao
FS_v_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSUN',
         season == 'verao') 

kruskal.test(onset_rate~ano, data = FS_v_heat_interval)
fs_v_on<- pairwise.wilcox.test(FS_v_heat_interval$onset_rate,
                               FS_v_heat_interval$ano,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
fs_v_on$p.value
(fs_v_on_a <- melt(fs_v_on$p.value))
fs_v_on_a.cc  <-  na.omit(fs_v_on_a)
fs_v_on_a.pvals  <-  fs_v_on_a.cc[, 3]
names(fs_v_on_a.pvals)  <-  paste(fs_v_on_a.cc[, 1], fs_v_on_a.cc[, 2], sep="-")
multcompLetters(fs_v_on_a.pvals)

#primavera
FS_p_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSUN',
         season == 'primavera') 

kruskal.test(onset_rate~ano, data = FS_p_heat_interval)
fs_p_on<- pairwise.wilcox.test(FS_p_heat_interval$onset_rate,
                               FS_p_heat_interval$ano,
                               p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
fs_p_on$p.value
(fs_p_on_a <- melt(fs_p_on$p.value))
fs_p_on_a.cc  <-  na.omit(fs_p_on_a)
fs_p_on_a.pvals  <-  fs_p_on_a.cc[, 3]
names(fs_p_on_a.pvals)  <-  paste(fs_p_on_a.cc[, 1], fs_p_on_a.cc[, 2], sep="-")
multcompLetters(fs_p_on_a.pvals)

#inverno
FS_i_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSUN',
         season == 'inverno') 

kruskal.test(onset_rate~ano, data = FS_i_heat_interval)
fs_i_on<- pairwise.wilcox.test(FS_i_heat_interval$onset_rate,
                               FS_i_heat_interval$ano,
                               p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
fs_i_on$p.value
(fs_i_on_a <- melt(fs_i_on$p.value))
fs_i_on_a.cc  <-  na.omit(fs_i_on_a)
fs_i_on_a.pvals  <-  fs_i_on_a.cc[, 3]
names(fs_i_on_a.pvals)  <-  paste(fs_i_on_a.cc[, 1], fs_i_on_a.cc[, 2], sep="-")
multcompLetters(fs_i_on_a.pvals)

#outono
FS_o_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSUN',
         season == 'outono') 

kruskal.test(onset_rate~ano, data = FS_o_heat_interval)
fs_o_on<- pairwise.wilcox.test(FS_o_heat_interval$onset_rate,
                               FS_o_heat_interval$ano,
                               p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
fs_o_on$p.value
(fs_o_on_a <- melt(fs_o_on$p.value))
fs_o_on_a.cc  <-  na.omit(fs_o_on_a)
fs_o_on_a.pvals  <-  fs_o_on_a.cc[, 3]
names(fs_o_on_a.pvals)  <-  paste(fs_o_on_a.cc[, 1], fs_o_on_a.cc[, 2], sep="-")
multcompLetters(fs_o_on_a.pvals)

multcompLetters(fs_v_on_a.pvals)
multcompLetters(fs_p_on_a.pvals)
multcompLetters(fs_i_on_a.pvals)
multcompLetters(fs_o_on_a.pvals)

#FORTSHADE
heat_interval %>% 
  filter(sensor == 'FORTSHADE') %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  ylim(0,3.5) +
  ggtitle('Fortaleza Sombra') +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#verao
FSh_v_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSHADE',
         season == 'verao') 

kruskal.test(onset_rate~ano, data = FSh_v_heat_interval)

#primavera
FSh_p_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSHADE',
         season == 'primavera') 

kruskal.test(onset_rate~ano, data = FSh_p_heat_interval)

#inverno
FSh_i_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSHADE',
         season == 'inverno') 

kruskal.test(onset_rate~ano, data = FSh_i_heat_interval)

#outono
FSh_o_heat_interval<- heat_interval %>% 
  filter(sensor == 'FORTSHADE',
         season == 'outono') 

kruskal.test(onset_rate~ano, data = FSh_o_heat_interval)

#PGShade
heat_interval %>% 
  filter(sensor == 'PGSHADE') %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2020,2023.5)) +
  ggtitle('Praia Grande Sombra') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

#verao
PSh_v_heat_interval<- heat_interval %>% 
  filter(sensor == 'PGSHADE',
         season == 'verao') 

kruskal.test(onset_rate~ano, data = PSh_v_heat_interval)

#primavera
PSh_p_heat_interval<- heat_interval %>% 
  filter(sensor == 'PGSHADE',
         season == 'primavera') 

kruskal.test(onset_rate~ano, data = PSh_p_heat_interval)

#inverno
PSh_i_heat_interval<- heat_interval %>% 
  filter(sensor == 'PGSHADE',
         season == 'inverno') 

kruskal.test(onset_rate~ano, data = PSh_i_heat_interval)

#outono
PSh_o_heat_interval<- heat_interval %>% 
  filter(sensor == 'PGSHADE',
         season == 'outono') 

kruskal.test(onset_rate~ano, data = PSh_o_heat_interval)

#PGSun
heat_interval %>% 
  filter(sensor == 'PGSUN') %>% 
  ggplot(aes(x = ano, y = onset_rate, group = ano)) +
  geom_boxplot(shape=21, outlier.shape = NA) + 
  facet_wrap(~ season) +
  theme_classic() +
  ylim(0,5) +
  scale_x_continuous(limits = c(2020,2023.5)) +
  ggtitle('Praia Grande Sol') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

## Quero fazer multivariada comparando onset_rate dos sensores ao longo do tempo




v<- heat_interval %>% 
  filter(season == 'verao') %>% 
  ggplot(aes(x = site, y = onset_rate, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  facet_wrap(~ ano) +
  theme_classic() +
  labs(col = "microhabitat") +
  ggtitle('Verão') +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

p<- heat_interval %>% 
  filter(season == 'primavera') %>% 
  ggplot(aes(x = site, y = onset_rate, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  facet_wrap(~ ano) +
  theme_classic() +
  labs(col = "microhabitat") +
  ggtitle('Primavera') +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))


o<- heat_interval %>% 
  filter(season == 'outono') %>% 
  ggplot(aes(x = site, y = onset_rate, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  facet_wrap(~ ano) +
  theme_classic() +
  labs(col = "microhabitat") +
  ggtitle('Outono') +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))


i<-heat_interval %>% 
  filter(season == 'inverno') %>% 
  ggplot(aes(x = site, y = onset_rate, color = sensor_stress)) +
  geom_boxplot(shape=21, outlier.shape = TRUE) + 
  facet_wrap(~ ano) +
  theme_classic() +
  labs(col = "microhabitat") +
  ggtitle('Inverno') +
  # geom_boxplot(shape=21, outlier.shape = NA) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))

v + p + o + i  #ajeitar, colocar nome dos sensores e colocar inves de site o ano

v_onset_rate <- heat_interval %>% 
  filter(season == 'verao')

hist(v_onset_rate$onset_rate)

v_onset_rate_anova<- aov(v_onset_rate$onset_rate ~ v_onset_rate$sensor, data= v_onset_rate)
anova(v_onset_rate_anova)
v_onset_rate_tukey<- TukeyHSD(v_onset_rate_anova)
v_onset_rate_cld <- multcompLetters4(v_onset_rate_anova, v_onset_rate_tukey)
v_onset_rate_cld


p_onset_rate <- heat_interval %>% 
  filter(season == 'primavera')

p_onset_rate_anova<- aov(p_onset_rate$onset_rate ~ p_onset_rate$sensor, data= p_onset_rate)
anova(p_onset_rate_anova)
p_onset_rate_tukey<- TukeyHSD(p_onset_rate_anova)
p_onset_rate_cld <- multcompLetters4(p_onset_rate_anova, p_onset_rate_tukey)


o_onset_rate <- heat_interval %>% 
  filter(season == 'outono')

o_onset_rate_anova<- aov(o_onset_rate$onset_rate ~ o_onset_rate$sensor, data= o_onset_rate)
anova(o_onset_rate_anova)
o_onset_rate_tukey<- TukeyHSD(o_onset_rate_anova)
o_onset_rate_cld <- multcompLetters4(o_onset_rate_anova, o_onset_rate_tukey)


i_onset_rate <- heat_interval %>% 
  filter(season == 'inverno')

i_onset_rate_anova<- aov(i_onset_rate$onset_rate ~ i_onset_rate$sensor, data= i_onset_rate)
anova(i_onset_rate_anova)
i_onset_rate_tukey<- TukeyHSD(i_onset_rate_anova)
i_onset_rate_cld <- multcompLetters4(i_onset_rate_anova, i_onset_rate_tukey)

v_onset_rate_cld
p_onset_rate_cld
o_onset_rate_cld
i_onset_rate_cld







verao_in<- heat_interval %>%
  filter(season == 'verao')

prima_in<- heat_interval %>%
  filter(season == 'primavera')

outono_in<- heat_interval %>%
  filter(season == 'outono')

inverno_in<- heat_interval %>%
  filter(season == 'inverno')

# Valor do pico de aquecimento
hist(max_temp$tmax)
max_temp %>% 
  group_by(sensor, season) %>% 
  slice(which.max(tmax))

kruskal.test(tmax~sensor, data = verao_in)
m<- pairwise.wilcox.test(verao_in$tmax,
                         verao_in$sensor,
                         p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni

m$p.value
(ma <- melt(m$p.value))
ma.cc  <-  na.omit(ma)
ma.pvals  <-  ma.cc[, 3]
names(ma.pvals)  <-  paste(ma.cc[, 1], ma.cc[, 2], sep="-")
ma.pvals
multcompLetters(ma.pvals)


kruskal.test(tmax~sensor, data = prima_in)
mm<- pairwise.wilcox.test(prima_in$tmax,
                          prima_in$sensor,
                          p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
mm$p.value
(mma <- melt(mm$p.value))
mma.cc  <-  na.omit(mma)
mma.pvals  <-  mma.cc[, 3]
names(mma.pvals)  <-  paste(mma.cc[, 1], mma.cc[, 2], sep="-")
mma.pvals
multcompLetters(mma.pvals)

kruskal.test(tmax~sensor, data = inverno_in)
mmm<- pairwise.wilcox.test(inverno_in$tmax,
                           inverno_in$sensor,
                           p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
mmm$p.value
(mmma <- melt(mmm$p.value))
mmma.cc  <-  na.omit(mmma)
mmma.pvals  <-  mmma.cc[, 3]
names(mmma.pvals)  <-  paste(mmma.cc[, 1], mmma.cc[, 2], sep="-")
mmma.pvals
multcompLetters(mmma.pvals)

kruskal.test(tmax~sensor, data = outono_in)
mmmm<- pairwise.wilcox.test(outono_in$tmax,
                            outono_in$sensor,
                            p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni
mmmm$p.value
(mmmma <- melt(mmmm$p.value))
mmmma.cc  <-  na.omit(mmmma)
mmmma.pvals  <-  mmmma.cc[, 3]
names(mmmma.pvals)  <-  paste(mmmma.cc[, 1], mmmma.cc[, 2], sep="-")
mmmma.pvals
multcompLetters(mmmma.pvals)

# hora do pico de aquecimento
pico<- heat_interval %>% 
  group_by(sensor, site, season) %>% 
  dplyr::summarise(hora_tmax_mean = mean(hora_tmax, na.rm = TRUE)) %>%
  data.frame()

hist(heat_interval$hora_tmax) #verificando normalidade. Não é normal! Fazer não-paramétrico
kruskal.test(hora_tmax~sensor, data = verao_in)
p<- pairwise.wilcox.test(verao_in$hora_tmax,
                         verao_in$sensor,
                         p.adjust.method="bonferroni") #ver se esse é o melhor metodo Bonferroni

p$p.value
(a <- melt(p$p.value))
a.cc  <-  na.omit(a)
a.pvals  <-  a.cc[, 3]
names(a.pvals)  <-  paste(a.cc[, 1], a.cc[, 2], sep="-")
a.pvals
multcompLetters(a.pvals)


kruskal.test(hora_tmax~sensor, data = prima_in)
pp<- pairwise.wilcox.test(prima_in$hora_tmax,
                          prima_in$sensor,
                          p.adjust.method="bonferroni")
pp$p.value
(b <- melt(pp$p.value))
b.cc  <-  na.omit(b)
b.pvals  <-  b.cc[, 3]
names(b.pvals)  <-  paste(b.cc[, 1], b.cc[, 2], sep="-")
b.pvals
multcompLetters(b.pvals)

kruskal.test(hora_tmax~sensor, data = inverno_in)
ppp<- pairwise.wilcox.test(inverno_in$hora_tmax,
                           inverno_in$sensor,
                           p.adjust.method="bonferroni")
ppp$p.value
(c <- melt(ppp$p.value))
c.cc  <-  na.omit(c)
c.pvals  <-  c.cc[, 3]
names(c.pvals)  <-  paste(c.cc[, 1], c.cc[, 2], sep="-")
c.pvals
multcompLetters(c.pvals)


kruskal.test(hora_tmax~sensor, data = outono_in)
pppp<- pairwise.wilcox.test(outono_in$hora_tmax,
                            outono_in$sensor,
                            p.adjust.method="bonferroni")

pppp$p.value
(d <- melt(pppp$p.value))
d.cc  <-  na.omit(d)
d.pvals  <-  d.cc[, 3]
names(d.pvals)  <-  paste(d.cc[, 1], d.cc[, 2], sep="-")
d.pvals
multcompLetters(d.pvals)


# curva de aquecimento
resum_heat_interval<- heat_interval %>%
  group_by(sensor, site, season) %>% 
  dplyr::summarise(mean_interval = mean(heat_interval, na.rm = TRUE)) %>%
  data.frame()

hist(heat_interval$heat_interval) #verificando normalidade

v_i_anova<- aov(verao_in$heat_interval ~ verao_in$sensor, data= verao_in)
anova(v_i_anova)
v_i_tukey<- TukeyHSD(v_i_anova)
v_i_cld <- multcompLetters4(v_i_anova, v_i_tukey)
v_i_cld


p_i_anova<- aov(prima_in$heat_interval ~ prima_in$sensor, data= prima_in)
anova(p_i_anova)
p_i_tukey<- TukeyHSD(p_i_anova)
p_i_cld <- multcompLetters4(p_i_anova, p_i_tukey)
p_i_cld


i_i_anova<- aov(inverno_in$heat_interval ~ inverno_in$sensor, data= inverno_in)
anova(i_i_anova)
i_i_tukey<- TukeyHSD(i_i_anova)
i_i_cld <- multcompLetters4(i_i_anova, i_i_tukey)
i_i_cld


o_i_anova<- aov(outono_in$heat_interval ~ outono_in$sensor, data= outono_in)
anova(o_i_anova)
o_i_tukey<- TukeyHSD(o_i_anova)
o_i_cld <- multcompLetters4(o_i_anova, o_i_tukey)
o_i_cld


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

#### Multivariada

#Similaridade entre sensores: CLUSTER

library(vegan)

CV <- function(x){
  (sd(x)/mean(x))*100
}

temp_cluster<- temperatura %>%
  group_by(sensor, sensor_stress, site, season) %>% 
  dplyr::summarise(tmax = max(temp, na.rm = TRUE),
                   tmin = min(temp, na.rm = TRUE),
                   tmean = mean(temp, na.rm = TRUE),
                   desvio = sd(temp, na.rm = TRUE),
                   cv = CV(temp)) %>%
  data.frame()

temp_cluster$site_sigla[temp_cluster$site == 'Praia Grande'] <- 'PG'
temp_cluster$site_sigla[temp_cluster$site == 'Fortaleza'] <- 'FT'

#Verão
v_temp_cluster<- temp_cluster %>% 
  filter(season == 'verao')

v_temp_cluster$amostra <- paste(v_temp_cluster$site_sigla,v_temp_cluster$sensor_stress, sep = "_")

rownames(v_temp_cluster) <- v_temp_cluster$amostra

v_km.clust <- v_temp_cluster %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

v_fit_temp <- v_temp_cluster[] %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

v_grupo_temp <- v_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

v_dendro <- factoextra::fviz_dend(v_km.clust, k = 2, # Cut in four groups
                                cex = 0.7, # label size
                                k_colors = c("#2E9FDF", "#FC4E07", "grey", "purple"),
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

p_temp_cluster$amostra <- paste(p_temp_cluster$site_sigla,p_temp_cluster$sensor_stress, sep = "_")

rownames(p_temp_cluster) <- p_temp_cluster$amostra

p_km.clust <- p_temp_cluster %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

p_fit_temp <- p_temp_cluster[] %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

p_grupo_temp <- p_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

p_dendro <- factoextra::fviz_dend(p_km.clust, k = 2, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = c("#2E9FDF", "#FC4E07", "grey", "purple"),
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

i_temp_cluster$amostra <- paste(i_temp_cluster$site_sigla,i_temp_cluster$sensor_stress, sep = "_")

rownames(i_temp_cluster) <- i_temp_cluster$amostra

i_km.clust <- i_temp_cluster %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

i_fit_temp <- i_temp_cluster[] %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

i_grupo_temp <- i_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

i_dendro <- factoextra::fviz_dend(i_km.clust, k = 2, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = c("#2E9FDF", "#FC4E07", "grey", "purple"),
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

o_temp_cluster$amostra <- paste(o_temp_cluster$site_sigla,o_temp_cluster$sensor_stress, sep = "_")

rownames(o_temp_cluster) <- o_temp_cluster$amostra

o_km.clust <- o_temp_cluster %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>%
  hclust(method = "ward.D2") # Compute hierachical clustering

o_fit_temp <- o_temp_cluster[] %>%
  dplyr::select(tmax, tmin, tmean, cv) %>% 
  scale() %>% 
  vegdist(method = "euclidian") %>% 
  data.matrix() %>%
  kmeans(2)

o_grupo_temp <- o_fit_temp$cluster %>% 
  data.frame() %>% 
  dplyr::rename(., grupo_temp = `.`) %>%
  tibble::rownames_to_column(., "meses")

o_dendro <- factoextra::fviz_dend(o_km.clust, k = 2, # Cut in four groups
                                  cex = 0.7, # label size
                                  k_colors = c("#2E9FDF", "#FC4E07", "grey", "purple"),
                                  main = "",
                                  ylab = "",
                                  horiz = T) +
  ggtitle('Outono')+
  theme(axis.ticks = element_blank(),
        axis.text.x=element_blank())

o_dendro

v_dendro + p_dendro + i_dendro + o_dendro



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
  labs(col = "microhabitat") +
  ggtitle("Verão") +
  #ylab("Temperatura (ºC)")+
  #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  labs(x = "", y = "Temperatura (ºC)")  + labs(color='') 

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
  labs(col = "microhabitat") +
  #ylab("Temperatura (ºC)")+
  ggtitle("Primavera") +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  labs(x = "", y = "Temperatura (ºC)")  + labs(color='') 

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
  labs(col = "microhabitat") +
  #ylab("Temperatura (ºC)")+
  ggtitle("Outono") +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  labs(x = "", y = "Temperatura (ºC)")  + labs(color='') 

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
  labs(col = "microhabitat") +
  #ylab("Temperatura (ºC)")+
  ggtitle("Inverno") +
  #theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5))
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  labs(x = "", y = "Temperatura (ºC)") + labs(color='')

inverno_anova <- aov(inverno_perday$ln_tmean ~ inverno_perday$sensor, data= inverno_perday)
anova(inverno_anova)
inverno_tukey<- TukeyHSD(inverno_anova)

inverno_cld <- multcompLetters4(inverno_anova, inverno_tukey)

plot_verao + plot_prima + plot_inverno + plot_outono + plot_layout(guides = "collect")#plotando todos juntos
#plot_verao + plot_prima + plot_inverno + plot_outono #plotando todos juntos
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

####################################################################################


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

max(ft_sun$t)
min(ft_sun$t)

ft_shade<- temp_fort_resum %>% 
  filter(sensor == 'FORTSHADE')

max(ft_shade$t)
min(ft_shade$t)

# CLIMATOLOGIA
ts2_sun<- ts2clm(
  ft_sun,
  x = t,
  y = tmean,
  climatologyPeriod = c("2019-06-16", "2023-07-08"),
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


ts2<- ts2clm(
  temp_fort_resum,
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



#datas_sun<- unique(event_sun$date_peak)

#ts2_sun %>% filter_all(any_vars(. %in% datas_sun))

clim_sun<- ggplot(ts2_sun, aes(x=t, y=tmean)) +
  geom_line() +
  ylim(17, 30) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  geom_vline(xintercept=as.numeric(ts2_sun$t[c(5, 96, 209, 404, 918, 952, 1028, 1055, 1229, 1256, 1379, 1421)]), linetype=4, color = 'orange') +
  xlab("Data") +
  ylab("Temperatura média (ºC)") +
  ggtitle("Fortaleza Sol") +
  theme_classic()

#datas_shade<- unique(event_shade$date_peak)

#ts2_shade %>% filter_all(any_vars(. %in% datas_shade))

clim_shade<- ggplot(ts2_shade, aes(x=t, y=tmean)) +
  geom_line() +
  ylim(17, 30) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y") +
  geom_vline(xintercept=as.numeric(ts2_shade$t[c(149, 161, 179, 224, 424, 571, 611, 665, 712, 1196, 1370)]), linetype=4, color = 'orange') +
  xlab("Data") +
  ylab("Temperatura média (ºC)") +
  ggtitle("Fortaleza Sombra") +
  theme_classic()

clim_sun + clim_shade

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

yearmean_sun<- block_average(detect_sun, x = t, y = tmean, report = "full")

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

yearmean_shade<- block_average(detect_shade, x = t, y = tmean, report = "full")











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

loli_sun<- ggplot(event_sun, aes(x = date_peak, y = intensity_max)) +    
  geom_lolli(aes(colour = duration)) +
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20)) +
  ylim(0.2,4) +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sol') +
  theme_classic()


ggplot(event_sun, aes(x = date_peak, y = intensity_max)) +    
  geom_lolli(aes(colour = duration)) +
  scale_color_distiller(palette = "Spectral", name = "Duração") +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sol') +
  theme_classic()



loli_shade<- ggplot(event_shade, aes(x = date_peak, y = intensity_max)) +    
  geom_lolli(aes(colour = duration)) +
  scale_color_distiller(palette = "Spectral", name = "Duração", limits = c(5,20)) +
  ylim(0.2,4) +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sombra') +
  theme_classic()

ggplot(cat_shade, aes(x = peak_date, y = i_max)) +    
  geom_lolli(aes(colour = category)) +
  scale_color_discrete(name = "Categoria") +
  ylim(0.2,4) +
  xlab("Data") + ylab("Intensidade Máxima") +
  ggtitle('Fortaleza sombra') +
  theme_classic()

loli_sun + loli_shade  #pq tem alguns pretos?

ggplot(event_sun, aes(x = date_peak, y = duration)) +    
  geom_lolli(aes(colour = intensity_max)) +
  scale_color_distiller(palette = "Spectral", name = "Intensidade Máxima") +
  xlab("Data") + ylab("Duração") +
  ggtitle('Fortaleza sol') +
  theme_classic()

ggplot(event_shade, aes(x = date_peak, y = duration)) +    
  geom_lolli(aes(colour = intensity_max)) +
  scale_color_distiller(palette = "Spectral", name = "Intensidade Máxima") +
  xlab("Data") + ylab("Duração") +
  ggtitle('Fortaleza sombra') +
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
## GRAFICOS DE BARRA, MELHOR BOXPLOT
#bar_cover<- entremares %>%
#  filter(!is.na(type_cover),
#         type_cover != "NA") %>%
#  mutate(type_cover = plyr::mapvalues(type_cover,
#                                      from = c("Filamentosa", "Corticada", "Crostosa",  "Foliacea", "ACA", "Coriacea",
#                                               "Bunodosoma caissarum", "Tedania ignis", "Petaloconchus varians",
#                                               "Isognomon bicolor", "Perna perna", "Crassostrea brasiliana", "Ostra",
#                                               "Echinometra lucunter", "Lottia subrugosa", "Fissurella rosea", "Echinolittorina lineolata",
#                                               "Stramonita haemastoma", "Eriphia gonagra", "Onchidella indolens", "Claremontiella nodulosa"),
#                                      to = c(rep("macroalgae", 6),
#                                             rep("other sessile", 3),
#                                             rep("other bivalve", 4),
#                                             rep("motile", 8))) %>% 
#           factor(., levels = c("Chthamalus bisinuatus", "Bare rock", "Mytilaster solisianus", 
#                                "Tetraclita stalactifera", "macroalgae", "other bivalve",
#                                "motile", "other sessile",  "Cyanophyceae", "CCA")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  group_by(locality, season, tideHeight, type_cover, year, quadrat) %>%
#  dplyr::summarise(soma = sum(relative_cover, na.rm = TRUE)) %>%
#  ggplot(aes(x = year, y = soma, fill = type_cover)) + 
#  geom_bar(position = "fill", stat = "identity") +
#  labs(x = '', y='Cobertura')+
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality + season, scales = "free_y") +
#  theme_bw()

#bar_density
#bar_cover

#inv_bar_cover<- entremares %>%
#  filter(!is.na(type_cover),
#         type_cover != "NA",
#         season == 'inverno') %>%
#  mutate(type_cover = plyr::mapvalues(type_cover,
#                                      from = c("Filamentosa", "Corticada", "Crostosa",  "Foliacea", "ACA", "Coriacea",
#                                               "Bunodosoma caissarum", "Tedania ignis", "Petaloconchus varians",
#                                               "Isognomon bicolor", "Perna perna", "Crassostrea brasiliana", "Ostra",
#                                               "Echinometra lucunter", "Lottia subrugosa", "Fissurella rosea", "Echinolittorina lineolata",
#                                               "Stramonita haemastoma", "Eriphia gonagra", "Onchidella indolens", "Claremontiella nodulosa"),
#                                      to = c(rep("macroalgae", 6),
#                                             rep("other sessile", 3),
#                                             rep("other bivalve", 4),
#                                             rep("motile", 8))) %>% 
#           factor(., levels = c("Chthamalus bisinuatus", "Bare rock", "Mytilaster solisianus", 
#                                "Tetraclita stalactifera", "macroalgae", "other bivalve",
#                                "motile", "other sessile",  "Cyanophyceae", "CCA")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  group_by(locality, season, tideHeight, type_cover, year, quadrat) %>%
#  dplyr::summarise(soma = sum(relative_cover, na.rm = TRUE)) %>%
#  ggplot(aes(x = year, y = soma, fill = type_cover)) + 
#  geom_bar(position = "fill", stat = "identity") +
#  ggtitle('Inverno') +
#  labs(x = '', y='Cobertura')+
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#prima_bar_cover<- entremares %>%
#  filter(!is.na(type_cover),
#         type_cover != "NA",
#         season == 'primavera') %>%
#  mutate(type_cover = plyr::mapvalues(type_cover,
#                                      from = c("Filamentosa", "Corticada", "Crostosa",  "Foliacea", "ACA", "Coriacea",
#                                               "Bunodosoma caissarum", "Tedania ignis", "Petaloconchus varians",
#                                               "Isognomon bicolor", "Perna perna", "Crassostrea brasiliana", "Ostra",
#                                               "Echinometra lucunter", "Lottia subrugosa", "Fissurella rosea", "Echinolittorina lineolata",
#                                               "Stramonita haemastoma", "Eriphia gonagra", "Onchidella indolens", "Claremontiella nodulosa"),
#                                      to = c(rep("macroalgae", 6),
#                                             rep("other sessile", 3),
#                                             rep("other bivalve", 4),
#                                             rep("motile", 8))) %>% 
#           factor(., levels = c("Chthamalus bisinuatus", "Bare rock", "Mytilaster solisianus", 
#                                "Tetraclita stalactifera", "macroalgae", "other bivalve",
#                                "motile", "other sessile",  "Cyanophyceae", "CCA")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  group_by(locality, season, tideHeight, type_cover, year, quadrat) %>%
#  dplyr::summarise(soma = sum(relative_cover, na.rm = TRUE)) %>%
#  ggplot(aes(x = year, y = soma, fill = type_cover)) + 
#  geom_bar(position = "fill", stat = "identity") +
#  ggtitle('Primavera') +
#  labs(x = '', y='Cobertura')+
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#prima_bar_cover + verao_bar_cover + outo_bar_cover + inv_bar_cover


#bar_density<- entremares_zeros_year %>%
#  filter(!is.na(motile),
#         motile != "NA") %>%
#  mutate(motile = plyr::mapvalues(motile,
#                                      from = c("Claremontiella nodulosa", "Echinometra lucunter", "Morula",  "Onchidella indolens", "Turbelaria", "Pachygrapsus transversus",
#                                               "Amphipoda"),
#                                  to = c(rep("other motile", 7))) %>% 
#           factor(., levels = c("Echinolittorina lineolata", "Lottia subrugosa", "Fissurella rosea", 
#                                "Stramonita haemastoma", "other motile")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  ggplot(aes(x = year, y = mean_density, fill = motile)) + 
#  geom_bar(position = "stack", stat = "identity") +
#  labs(y = 'Densidade por 0,25m²', x = '') +
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality + season, scales = "free_y") +
#  theme_bw()

# GRAFICOS DE BARRA: MELHOR DEIXAR BOXPLOT
#v_bar_density<- entremares_zeros_year %>%
#  filter(!is.na(motile),
#         motile != "NA",
#         season == 'verao') %>%
#  mutate(motile = plyr::mapvalues(motile,
#                                  from = c("Claremontiella nodulosa", "Echinometra lucunter", "Morula",  "Onchidella indolens", "Turbelaria", "Pachygrapsus transversus",
#                                           "Amphipoda"),
#                                  to = c(rep("other motile", 7))) %>% 
#           factor(., levels = c("Echinolittorina lineolata", "Lottia subrugosa", "Fissurella rosea", 
#                                "Stramonita haemastoma", "other motile")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  ggplot(aes(x = year, y = mean_density, fill = motile)) + 
#  geom_bar(position = "stack", stat = "identity") +
#  ggtitle('Verão') +
#  labs(y = 'Densidade por 0,25m²', x = '') +
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#prima_bar_density<- entremares_zeros_year %>%
#  filter(!is.na(motile),
#         motile != "NA",
#         season == 'primavera') %>%
#  mutate(motile = plyr::mapvalues(motile,
#                                  from = c("Claremontiella nodulosa", "Echinometra lucunter", "Morula",  "Onchidella indolens", "Turbelaria", "Pachygrapsus transversus",
#                                           "Amphipoda"),
#                                  to = c(rep("other motile", 7))) %>% 
#           factor(., levels = c("Echinolittorina lineolata", "Lottia subrugosa", "Fissurella rosea", 
#                                "Stramonita haemastoma", "other motile")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  ggplot(aes(x = year, y = mean_density, fill = motile)) + 
#  geom_bar(position = "stack", stat = "identity") +
#  ggtitle('Primavera') +
#  labs(y = 'Densidade por 0,25m²', x = '') +
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#outo_bar_density<- entremares_zeros_year %>%
#  filter(!is.na(motile),
#         motile != "NA",
#         season == 'outono') %>%
#  mutate(motile = plyr::mapvalues(motile,
#                                  from = c("Claremontiella nodulosa", "Echinometra lucunter", "Morula",  "Onchidella indolens", "Turbelaria", "Pachygrapsus transversus",
#                                           "Amphipoda"),
#                                  to = c(rep("other motile", 7))) %>% 
#           factor(., levels = c("Echinolittorina lineolata", "Lottia subrugosa", "Fissurella rosea", 
#                                "Stramonita haemastoma", "other motile")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  ggplot(aes(x = year, y = mean_density, fill = motile)) + 
#  geom_bar(position = "stack", stat = "identity") +
#  ggtitle('Outono') +
#  labs(y = 'Densidade por 0,25m²', x = '') +
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#inv_bar_density<- entremares_zeros_year %>%
#  filter(!is.na(motile),
#         motile != "NA",
#         season == 'inverno') %>%
#  mutate(motile = plyr::mapvalues(motile,
#                                  from = c("Claremontiella nodulosa", "Echinometra lucunter", "Morula",  "Onchidella indolens", "Turbelaria", "Pachygrapsus transversus",
#                                           "Amphipoda"),
#                                  to = c(rep("other motile", 7))) %>% 
#           factor(., levels = c("Echinolittorina lineolata", "Lottia subrugosa", "Fissurella rosea", 
#                                "Stramonita haemastoma", "other motile")),
#         tideHeight = factor(tideHeight, levels = c("high", "mid", "low"))) %>% 
#  ggplot(aes(x = year, y = mean_density, fill = motile)) + 
#  geom_bar(position = "stack", stat = "identity") +
#  ggtitle('Inverno') +
#  labs(y = 'Densidade por 0,25m²', x = '') +
#  scale_fill_viridis_d() + # mudar a escala para aumentar contraste
#  facet_grid(tideHeight ~ locality, scales = "free_y") +
#  theme_bw()

#prima_bar_density + v_bar_density + outo_bar_density + inv_bar_density


library (installr)
updateR() 

ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res <- detect_event(ts)
event_line(res, spread = 100, metric = duration,
           start_date = "2010-12-01", end_date = "2011-06-30")
