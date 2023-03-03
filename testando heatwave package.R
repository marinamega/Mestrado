#### HeatWave Package ####
#Testando com os dados disponibilizados pelo pacote
library(dplyr)
library(ggpubr)
library(heatwaveR)

Algiers<- Algiers #chamando dataframe
# observe que os dados tem apenas um valor de tmáx e tmin

# Nesse conceito de onda atmosférica de calor, a tmax tem que ser superior que 90º percentil e a tmin tem que exceder 19ºC
# Resumo: são dois thresholds, um com percentil e outro estático
# Dias em que a tmáx superou 90º percentil
tMax_clim <- ts2clm(data = Algiers, y = tMax, climatologyPeriod = c("1961-01-01", "1990-12-31"), pctile = 90)

# Dias em que a tmin foi maior que 19ºC 
# como é um threshold estático, usamos a função exceedance
tMin_exc <- exceedance(data = Algiers, y = tMin, threshold = 19, minDuration = 3, maxGap = 1)$threshold

# Calculando os eventos
# Observe que com dois limites é possível filtrar alguns resultados. Por ex, apenas no verão excede 19ºC, então estamos escolhendo trabalhar apenas com essa estação
events <- detect_event(data = tMax_clim, y = tMax, # The 90th percentile threshold
                       threshClim2 = tMin_exc$exceedance) # The flat exceedance threshold

# Criando os gráficos
bubble_plot <- ggplot(data = events$event, aes(x = date_peak, y = intensity_max)) +
  geom_point(aes(size = intensity_cumulative), shape = 21, fill = "salmon", alpha = 0.8) +
  labs(x = NULL, y = "Maximum Intensity [°C] ", size = "Cumulative Intensity [°C x days]") +
  scale_size_continuous(range = c(1, 10), 
                        guide = guide_legend(title.position = "top", direction = "horizontal")) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.12),
        legend.box.background = element_rect(colour = "black"))

ggarrange(event_line(events, y = tMax, metric = "intensity_max"),
          event_line(events, y = tMax, metric = "intensity_max", category = T),
          lolli_plot(events),
          bubble_plot,
          ncol = 2, nrow = 2, align = "hv")

# Dois thresholds em percentil. Neste caso não usamos exceendance.
# Observe que também podemeos ajustar os limites de duração (minDuration) 
# e também o intervalo máximo (maxGap) para nossos limites múltiplos

# Por exemplo, digamos que estamos interessados em eventos noturnos (tMin), 
# quando as temperaturas permanecem acima do limite do percentil 80 (pctile = 80),
# por 10 ou mais dias (minDuration = 10) sem cair abaixo desse limite por mais
# de 2 dias consecutivos dias (maxGap = 2).
# Mas, além disso, também estamos interessados apenas nas partes do evento 
# em que as temperaturas diurnas excedem o limite do percentil 90 (pctile = 90) 
# por 3 ou mais dias (minDuration = 3) sem parar (maxGap = 0).
# Deve-se observar aqui que o critério que for mais rigoroso, neste caso 
# minDuration = 3 e maxGap = 0, será o filtro predominante através do qual 
# as métricas do evento serão quantificadas.

# Primeiro threshold baseado em tMin (temperaturas noturnas acima do 80 percentil)
thresh_tMin <- ts2clm(data = Algiers, y = tMin, pctile = 80, 
                      climatologyPeriod = c("1961-01-01", "1990-12-31"))

# Segundo threshold baseado em tMax (temperaturas diurnas acima do 90 percentil por mais de 3 dias sem gap)
thresh_tMax <- detect_event(ts2clm(data = Algiers, y = tMax, pctile = 90, 
                                   climatologyPeriod = c("1961-01-01", "1990-12-31")),
                            # esse argumentos seguintes são para detect_event(), não ts2clm()
                            minDuration = 3, maxGap = 0, y = tMax, protoEvents = T)

# Detectando e calculando os eventos usando dois thresholds pre calculados
# Note que devemos especificar de novo os argumentos usados
events_two_thresh <- detect_event(data = thresh_tMin, y = tMin, minDuration = 10, maxGap = 2,
                                  threshClim2 = thresh_tMax$event, minDuration2 = 3, maxGap2 = 0)

# Detectando e calculando os eventos usando um thresholds apenas
events_one_thresh <- detect_event(data = thresh_tMin, y = tMin, minDuration = 10, maxGap = 2)

# Verificando os eventos para cada método:
head(events_one_thresh$event)  #apenas um threshold

head(events_two_thresh$event) #dois  thresholds

# Representando graficamente:
# Note que foram calculadas as mesmas quantidades de ondas de calor
# as ondas de calor na série temporal são tão pronunciadas que surgem independentemente. 
# não entendi muito bem o que é isso, de surgir independentemente... não tava com os dois?
ggarrange(lolli_plot(events_one_thresh), lolli_plot(events_two_thresh), labels = c("One threshold", "Two thresholds"))

############# não entendi muito bem ###############

## Filtrando com o segundo threshold
# A metodologia descrita abaixo para a detecção e filtragem de eventos com dois limites 
# é um tanto complicada. Um possível problema com essa técnica é que os vários 
# filtros não afetam o cálculo das métricas de evento (por exemplo, intensidade_cumulativa),
# pois apenas o limite primário fornecido para detect_event() é usado ao calcular 
# as métricas de evento. No entanto, esse pode ser o caso desejado se alguém ainda 
# estiver interessado em saber a intensidade cumulativa acima do limite de percentil
# fornecido, mas deseja apenas filtrar o evento completo com base em algum outro 
# critério de limite.

# Como já calculamos nossos eventos de limite único (events_one_thresh) e nosso 
# segundo limite (thresh_tMax), podemos começar a filtrar os resultados diretamente.
# Antes de fazer isso, vamos extrair os componentes da lista de nossos resultados 
# em dataframes de dados para facilitar o uso no futuro.

events_one_event <- events_one_thresh$event
events_one_climatology <- events_one_thresh$climatology

# É aqui que as coisas podem ficar complicadas e onde termina o uso padrão das funções 
# no pacote heatwaveR. Mas não se desespere! Podemos usar o conjunto de pacotes do aimverse
# Para facilitar a filtragem de eventos, combinaremos os dois dataframes de dados 
# diferentes que estamos usando como guias de limite/filtragem para escolher os 
# eventos que atendem a todos os nossos critérios de seleção.

# Juntando os dataframes dos dois thresholds
two_thresh <- left_join(events_one_climatology, thresh_tMax, by = c("t"))

# Removendo todos os dias que não se qualificam para ambos thresholds
two_thresh_filtered <- two_thresh %>%
  filter(event.x == TRUE,
         event.y == TRUE)

# Com nosso guia de filtragem criado, agora podemos aplicá-lo a events_one_thresh 
# para obter nossos resultados filtrados!!!!!

events_one_thresh_filtered <- events_one_thresh

# filtrando
events_one_thresh_filtered$event <- events_one_thresh_filtered$event %>% 
  filter(event_no %in% two_thresh_filtered$event_no.x)

# Comparando resultados
head(events_one_thresh_filtered$event)
head(events_two_thresh$event)

# Os números de eventos encontrados em events_one_thresh_filtered são iguais aos 
# números de eventos encontrados em events_two_thresh com a importante diferença de 
# que as métricas de eventos em events_two_thresh foram calculadas apenas nos dias 
# que excederam ambos os limites, enquanto os eventos em events_one_thresh_filtered
# tiveram suas métricas calculadas de todos os os dias que excederam apenas o 
# primeiro limite.

# Visualizando graficamente
# não ta funcionando. Que nem o event_line no script das exploratórias.
ggarrange(lolli_plot(events_two_thresh, metric = duration), 
          lolli_plot(events_one_thresh_filtered, metric = duration), 
          labels = c("Double threshold", "Filter threshold"))

ggarrange(lolli_plot(events_two_thresh, metric = intensity_cumulative), 
          lolli_plot(events_one_thresh_filtered, metric = intensity_cumulative), 
          labels = c("Double threshold", "Filter threshold"))


