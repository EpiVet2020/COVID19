# PACKAGES E LIBRARYS PARA ANALISAR DADOS

##install.packages("data.table")
library(data.table)
library(dplyr)
library(tibble)
library(ggplot2)
library(grid)


# PACKAGES E LIBRARYS PARA FAZER GRAFICOS

##install.packages("rgdal")
##install.packages("geojsonio")
##install.packages("geojsonR")
##install.packages("leaflet")
##install.packages("sf")
library(rgdal)
library(geojsonio)
library(geojsonR)
library(RColorBrewer)
library(leaflet)
library(sf)



# FONTE DOS DADOS
covid.pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")




#POR AS DATAS EM FORMATO DATA

covid.pt$data<-as.Date(as.character(covid.pt$data),format = "%d-%m-%Y")




# SINAIS CLINICOS
## Calcular as frequencias relativas dos sinais clinicos (tabela e grafico de barras)

sintomas <- as.data.frame(t(covid.pt[173, 41:46]))
                          
sintomas <- sintomas %>%
  rownames_to_column(var="Sintomas")
  names(sintomas)[2] <- "Frequ?ncia"

ggplot(sintomas, aes(x=Sintomas, y=Frequ?ncia*100)) + 
  geom_col(fill="darksalmon", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade\nrespirat?ria", "Dores\nmusculares", "Febre", "Fraqueza\ngeneralizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequ?ncia (%)", title = "Frequ?ncia de sintomas da COVID-19",x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequ?ncia, digits=4)), vjust=-0.5)
      

## Avaliar se as frequencias relativas/incidencia dos sinais clinicos se foram alterando ao longo do tempo e fazer um grafico de linhas

sintomas_tempo <- as.data.frame((covid.pt[8:173,41:46]*covid.pt$confirmados[8:173])-(covid.pt[7:172,41:46]*covid.pt$confirmados[7:172]))/covid.pt$confirmados_novos[8:173]

sintomas_tempo_2 <- rbind(covid.pt[7,41:46], sintomas_tempo)

sintomas_tempo_3 <- cbind(covid.pt$data[7:173], sintomas_tempo_2)
names(sintomas_tempo_3) <- c("Data", "Tosse", "Febre", "Dificuldade respirat?ria", "Cefaleia", "Dores musculares", "Fraqueza generalizada")

sintomas_tempo_3[3,4] <- 0.11

sintomas_tempo_melt <- melt(sintomas_tempo_3, id.vars="Data")
names(sintomas_tempo_melt)[-1] <- c("Sintomas", "Valores")
 
ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$Sintomas) + ## para separar em varios graficos
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("Frequ?ncia (%)")+
  labs(title = "Frequ?ncia de Sintomas da COVID-19 ao longo do tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))
###Os valores negativos significam que houve uma diminui??o na percentagem de pessoas com determinado sinal clinico
###Os valores positivos mas menores do que o do dia anterior significam que houve crescimento menor do que do dia anterior.




# NUMERO DE CASOS
## N? de casos por grupo etario por genero e total (tabela e grafico de barras)

femininos <- as.data.frame(covid.pt %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos <- as.data.frame(covid.pt %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

casos_femininos <- as.data.frame(lapply(femininos, last))
casos_masculinos <- as.data.frame(lapply(masculinos, last))
casos_total <- as.data.frame(casos_femininos + casos_masculinos)

casos_femininos_invertido <- as.data.frame(t(casos_femininos))
casos_femininos_invertido <- casos_femininos_invertido %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_invertido)[2] <- "Feminino"
casos_femininos_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_masculinos_invertido <- as.data.frame(t(casos_masculinos))
casos_masculinos_invertido <- casos_masculinos_invertido %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_invertido)[2] <- "Masculino"
casos_masculinos_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_total_invertido <- as.data.frame(t(casos_total))
casos_total_invertido <- casos_total_invertido %>% 
  rownames_to_column(var = "Idade")
names(casos_total_invertido)[2] <- "Total"
casos_total_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_fem_masc <- merge(casos_femininos_invertido, casos_masculinos_invertido, by = "Idade")
casos_fem_masc_tot <- merge(casos_fem_masc, casos_total_invertido, by = "Idade")

casos_grupo_etario_genero_melted <- reshape2::melt(casos_fem_masc_tot, id.vars = "Idade")
names(casos_grupo_etario_genero_melted)[2:3] <- c("G?nero","N?_casos")

ggplot(casos_grupo_etario_genero_melted, aes(x=Idade, y=N?_casos, fill=G?nero)) + 
  geom_col(width = 0.9, position = "dodge") +
  theme_classic() +
  labs(y="N? de casos", title = "N? de casos da COVID-19 por grupo et?rio e por g?nero", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 1, margin = margin(t=10, r=1, b= 2, l=1)), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N?_casos),position = position_dodge(width = 0.8), vjust=-0.5, size=3.2) +
  scale_fill_manual(values = c("pink2", "steelblue3", "grey71")) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(casos_grupo_etario_genero_melted$N?_casos)+500))


## N? de casos por grupo etario ao longo do tempo

femininos_casos_tempo <- femininos-lag(femininos)
masculinos_casos_tempo <- masculinos-lag(masculinos)
total_casos_tempo <- cbind(covid.pt$data, femininos_casos_tempo + masculinos_casos_tempo)
total_casos_tempo[7,2:10] <- femininos[7,] + masculinos[7,]
names(total_casos_tempo) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

total_casos_tempo_melted <- reshape2::melt(total_casos_tempo, id.vars="Data")
names(total_casos_tempo_melted)[2:3] <- c("Idade", "N?_casos")

### falta por os meses no eixo do x
ggplot(total_casos_tempo_melted, aes(x=Data, y=N?_casos, color=Idade)) +
  geom_line() +
  labs(title="N? de casos de COVID-19 por grupo et?rio ao longo do tempo")+
  facet_grid(total_casos_tempo_melted$Idade) +
  guides(color=FALSE) ##tirar a legenda


## N? de casos por regiao (tabela e grafico de barras)

regioes_casos <- as.data.frame(t(as.data.frame(lapply(covid.pt[,4:10], last))))

regioes_casos <- regioes_casos %>% 
  rownames_to_column(var="Regi?es")
names(regioes_casos)[2] <- "N?_casos"

ggplot(regioes_casos, aes(x=Regi?es, y=N?_casos)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("A?ores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="N? de casos", title = "N? de casos de COVID-19 por regi?es", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N?_casos), vjust=-0.5)


## N? de casos por regiao (mapa) 

mapa_portugal <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson", what = "sp")


regioes_casos_ordem <- regioes_casos[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
regioes_casos_ordem[,1] <- c("Alentejo", "Algarve", "A?ores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_casos <- colorBin("Blues",  bins = c(0,300, 2000, 6000, 20000, 30000, Inf)) ## tonalidade das cores consoante os casos

labels_casos <- paste( #tornar o mapa interativo
  "<strong>", regioes_casos_ordem[,1],"</strong><br/>", 
  regioes_casos_ordem[,2], " casos<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_casos(regioes_casos_ordem$N?_casos),
              label = labels_casos, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_casos, values = regioes_casos_ordem$N?_casos, opacity = 0.5, title = "N? de casos por regi?o") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Sat?lite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Sat?lite", "Dark"), options = layersControlOptions(collapsed = FALSE))




# INCIDENCIA
## Taxa de incidencia cumulativa por genero e por dia

pop_total <- 10295909
pop_homens <- 4859977
pop_mulheres <- 5435932

incidencia_total <- as.data.frame(covid.pt$confirmados_novos/(pop_total - covid.pt$confirmados - covid.pt$obitos))

incidencia_homens <- as.data.frame((covid.pt$confirmados_m - lag(covid.pt$confirmados_m))/(pop_homens - covid.pt$confirmados_m - covid.pt$obitos_m))
incidencia_homens[174:175,] <- NA

incidencia_mulheres <- as.data.frame((covid.pt$confirmados_f - lag(covid.pt$confirmados_f))/(pop_mulheres - covid.pt$confirmados_f - covid.pt$obitos_f))
incidencia_mulheres[174:175,] <- NA

incidencia_nt <- cbind(covid.pt$data, incidencia_total, incidencia_homens, incidencia_mulheres)
names(incidencia_nt) <- c("Data", "Total", "Homens", "Mulheres")

incidencia_melt <- reshape2::melt(incidencia_nt, id.vars="Data")
names(incidencia_melt)[-1] <- c("G?nero", "Valores")

###falta por os meses no eixo do x
ggplot(incidencia_melt, aes(x=Data, y=Valores*100, color=G?nero)) +
  geom_line() +
  labs(title="Incid?ncia Di?ria por G?nero", color="") +
  facet_grid(incidencia_melt$G?nero)+
  guides(color = FALSE) +
  xlab("Data") +
  ylab("Incid?ncia (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))


## Incidencia por regiao ao longo do tempo

pop_acores <- 242796
pop_alentejo <- 705018
pop_algarve <- 438635
pop_centro <- 2216927 
pop_lvt <- 2854802
pop_norte <- 3573961
pop_madeira <- 254254

pop_regioes <- as.data.frame(c(pop_norte, pop_centro, pop_lvt, pop_alentejo, pop_algarve, pop_acores,pop_madeira))
pop_regioes_rep <- as.data.frame(t(pop_regioes[rep(seq_len(ncol(pop_regioes)), each=nrow(covid.pt))]))
names(pop_regioes_rep) <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "A?ores", "Madeira")

incidencia_regioes_sd <- as.data.frame((covid.pt[,confirmados_arsnorte:confirmados_madeira]- lag(covid.pt[, confirmados_arsnorte:confirmados_madeira]))) / (pop_regioes_rep - as.data.frame(covid.pt[, confirmados_arsnorte:confirmados_madeira] - covid.pt[, obitos_arsnorte:obitos_madeira]))

incidencia_regioes <- cbind(covid.pt$data, incidencia_regioes_sd)
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "LVT", "Alentejo", "Algarve", "A?ores", "Madeira")

incidencia_regioes_melted <- reshape2::melt(incidencia_regioes, id.vars="Data")
names(incidencia_regioes_melted)[-1] <- c("ARS", "Incid?ncia")

### falta por os meses no eixo do x
ggplot(incidencia_regioes_melted, aes(x= Data, y = Incid?ncia*100, color = ARS)) +
  geom_line() +
  xlab("M?s") +
  ylab("Incid?ncia (%)") +
  labs(title="Incid?ncia da COVID-19 por regi?o ao longo do tempo") +
  facet_grid(incidencia_regioes_melted$ARS) +
  guides(color = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "1 month")
# scale_x_date(date_labels = "%b")




# CASOS DE MORTE
## N? de mortes por genero e total (Tabela e grafico de barras)

mortes_total <- last(covid.pt$obitos)
mortes_mulheres <- last(covid.pt$obitos_f)
mortes_homens <- last(covid.pt$obitos_m)

numero_mortes_nt <-cbind(mortes_total, mortes_homens, mortes_mulheres)

numero_mortes <- as.data.frame(t(numero_mortes_nt))
numero_mortes <- numero_mortes %>% 
  rownames_to_column(var="G?nero")
  names(numero_mortes)[2] <- "N?_mortes"

ggplot(numero_mortes, aes(x=G?nero, y=N?_mortes)) + 
  geom_col(fill="steelblue4", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="N? de mortes", title = "N?mero de mortes por COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = N?_mortes), vjust=-0.5)


## N? de mortes por grupo etario por genero e total (tabela e grafico de barras)

mortes_0_9_total <- last(covid.pt$obitos_0_9_f) + last(covid.pt$obitos_0_9_m)
mortes_0_9_f <- last(covid.pt$obitos_0_9_f)
mortes_0_9_m <- last(covid.pt$obitos_0_9_m)
mortes_10_19_total <- last(covid.pt$obitos_10_19_f) + last(covid.pt$obitos_10_19_m)
mortes_10_19_f <- last(covid.pt$obitos_10_19_f)
mortes_10_19_m <- last(covid.pt$obitos_10_19_m)
mortes_20_29_total <- last(covid.pt$obitos_20_29_f) + last(covid.pt$obitos_20_29_m)
mortes_20_29_f <- last(covid.pt$obitos_20_29_f)
mortes_20_29_m <- last(covid.pt$obitos_20_29_m)
mortes_30_39_total <- last(covid.pt$obitos_30_39_f) + last(covid.pt$obitos_30_39_m)
mortes_30_39_f <- last(covid.pt$obitos_30_39_f)
mortes_30_39_m <- last(covid.pt$obitos_30_39_m)
mortes_40_49_total <- last(covid.pt$obitos_40_49_f) + last(covid.pt$obitos_40_49_m)
mortes_40_49_f <- last(covid.pt$obitos_40_49_f)
mortes_40_49_m <- last(covid.pt$obitos_40_49_m)
mortes_50_59_total <- last(covid.pt$obitos_50_59_f) + last(covid.pt$obitos_50_59_m)
mortes_50_59_f <- last(covid.pt$obitos_50_59_f)
mortes_50_59_m <- last(covid.pt$obitos_50_59_m)
mortes_60_69_total <- last(covid.pt$obitos_60_69_f) + last(covid.pt$obitos_60_69_m)
mortes_60_69_f <- last(covid.pt$obitos_60_69_f)
mortes_60_69_m <- last(covid.pt$obitos_60_69_m)
mortes_70_79_total <- last(covid.pt$obitos_70_79_f) + last(covid.pt$obitos_70_79_m)
mortes_70_79_f <- last(covid.pt$obitos_70_79_f)
mortes_70_79_m <- last(covid.pt$obitos_70_79_m)
mortes_80_plus_total <- last(covid.pt$obitos_80_plus_f) + last(covid.pt$obitos_80_plus_m)
mortes_80_plus_f <- last(covid.pt$obitos_80_plus_f)
mortes_80_plus_m <- last(covid.pt$obitos_80_plus_m)

mortes_grupo_etario_f <- as.data.frame(t(cbind(mortes_0_9_f, mortes_10_19_f, mortes_20_29_f, mortes_30_39_f, mortes_40_49_f, mortes_50_59_f, mortes_60_69_f, mortes_70_79_f, mortes_80_plus_f)))
mortes_grupo_etario_m <- as.data.frame(t(cbind(mortes_0_9_m, mortes_10_19_m, mortes_20_29_m, mortes_30_39_m, mortes_40_49_m, mortes_50_59_m, mortes_60_69_m, mortes_70_79_m, mortes_80_plus_m)))
mortes_grupo_etario_total <- as.data.frame(t(cbind(mortes_0_9_total, mortes_10_19_total, mortes_20_29_total, mortes_30_39_total, mortes_40_49_total, mortes_50_59_total, mortes_60_69_total, mortes_70_79_total, mortes_80_plus_total)))

mortes_grupo_etario_f <- mortes_grupo_etario_f %>% 
  rownames_to_column(var="Idade")
names(mortes_grupo_etario_f)[2] <- "Feminino"
mortes_grupo_etario_f[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

mortes_grupo_etario_m <- mortes_grupo_etario_m %>% 
  rownames_to_column(var="Idade")
names(mortes_grupo_etario_m)[2] <- "Masculino"
mortes_grupo_etario_m[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

mortes_grupo_etario_total <- mortes_grupo_etario_total %>% 
  rownames_to_column(var="Idade")
names(mortes_grupo_etario_total)[2] <- "Total"
mortes_grupo_etario_total[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

mortes_grupo_etario_fm <- merge(mortes_grupo_etario_f, mortes_grupo_etario_m, by = "Idade")
mortes_grupo_etario_merged <- merge(mortes_grupo_etario_fm, mortes_grupo_etario_total, by = "Idade" )

mortes_grupo_etario_melted <- reshape2::melt(mortes_grupo_etario_merged, id.vars = "Idade")
names(mortes_grupo_etario_melted)[2:3] <- c("G?nero","Mortes")

ggplot(mortes_grupo_etario_melted, aes(x=Idade, y=Mortes, fill=G?nero)) + 
  geom_col(width = 0.9, position = "dodge") +
  theme_classic() +
  labs(y="N? de mortes", title = "N? de mortes da COVID-19 por grupo et?rio e por g?nero", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 1), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=Mortes),position = position_dodge(width = 0.9), vjust=-0.5, size=3.5) +
  scale_fill_manual(values = c("pink2", "steelblue3", "grey71"))


## N? de mortes por regiao (tabela e grafico de barras)

regioes_mortes <- as.data.frame(t(as.data.frame(lapply(covid.pt[,obitos_arsnorte:obitos_madeira], last))))

regioes_mortes <- regioes_mortes %>% 
  rownames_to_column(var="Regi?es")
names(regioes_mortes)[2] <- "N?_mortes"

ggplot(regioes_mortes, aes(x=Regi?es, y=N?_mortes)) + 
  geom_col(fill="olivedrab4", width = 0.5) +
  scale_x_discrete(labels= c("A?ores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="N? de mortes", title = "N? de mortes de COVID-19 por regi?es", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N?_mortes), vjust=-0.5)


## N? de mortes por regiao (mapa)

regioes_mortes_ordem <- regioes_mortes[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
regioes_mortes_ordem[,1] <- c("Alentejo", "Algarve", "A?ores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_mortes <- colorBin("YlOrBr", domain = regioes_mortes_ordem$N?_mortes, pretty = TRUE) 

labels_mortes <- paste( 
  "<strong>", regioes_mortes_ordem[,1],"</strong><br/>", 
  regioes_mortes_ordem[,2], " mortes<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mortes(regioes_mortes_ordem$N?_mortes),
              label = labels_mortes, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_mortes, values = regioes_mortes_ordem$N?_mortes, opacity = 0.5, title = "N? de mortes por regi?o") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Sat?lite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Sat?lite", "Dark"), options = layersControlOptions(collapsed = FALSE))




# TAXA DE MORTALIDADE
## Calcular a taxa de mortalidade por genero (tabela e grafico de barras)

tm_total <- last(covid.pt$obitos)/pop_total
tm_homens <- last(covid.pt$obitos_m)/ pop_homens
tm_mulheres <- last(covid.pt$obitos_f)/ pop_mulheres

tm_nt <- cbind(tm_total, tm_homens, tm_mulheres)

tm <- as.data.frame(t(tm_nt))
tm <- tm %>% 
    rownames_to_column(var="G?nero")
    names(tm)[2] <- "Taxa_Mortalidade"
    
ggplot(tm, aes(x=G?nero, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="snow3", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade da COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


## Taxa de mortalidade por regiao (tabela e grafico de barras)

tm_acores <- last(covid.pt$obitos_acores)/pop_acores
tm_alentejo <- last(covid.pt$obitos_arsalentejo)/pop_alentejo
tm_algarve <- last(covid.pt$obitos_arsalgarve)/pop_algarve
tm_centro <- last(covid.pt$obitos_arscentro)/pop_centro
tm_lvt <- last(covid.pt$obitos_arslvt)/pop_lvt
tm_norte <- last(covid.pt$obitos_arsnorte)/pop_norte
tm_madeira <- last(covid.pt$obitos_madeira)/pop_madeira

regioes_tm_nt <- cbind(tm_acores, tm_alentejo, tm_algarve, tm_centro, tm_lvt, tm_norte, tm_madeira)

regioes_tm <- as.data.frame(t(regioes_tm_nt))
regioes_tm <- regioes_tm %>% 
  rownames_to_column(var="Regi?o")
names(regioes_tm)[2] <- "Taxa_Mortalidade"

ggplot(regioes_tm, aes(x=Regi?o, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="grey73", width = 0.5) +
  scale_x_discrete(labels= c("A?ores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Madeira", "Norte")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade da COVID-19 por regi?es", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(round(Taxa_Mortalidade, digits=5))), vjust=-0.5)


## Taxa de mortalidade por regiao (mapa)

regioes_tm_ordem <- regioes_tm[c(2,3,1,4,7,6,5),] # colocar as regioes pela ordem do mapa
regioes_tm_ordem[,1] <- c("Alentejo", "Algarve", "A?ores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_tm <- colorBin("BuPu", domain = regioes_tm_ordem$Taxa_Mortalidade*100, bins = c(0, 0.003, 0.005, 0.007, 0.01, 0.02, Inf)) 

labels_tm <- paste( 
  "<strong>", regioes_tm_ordem[,1],"</strong><br/>", 
  round(regioes_tm_ordem[,2]*100, digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_tm(regioes_tm_ordem$Taxa_Mortalidade*100),
              label = labels_tm, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_tm, values = regioes_tm_ordem$Taxa_Mortalidade, opacity = 0.5, title = "Taxa de mortalidade por regi?o (%)") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Sat?lite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Sat?lite", "Dark"), options = layersControlOptions(collapsed = FALSE))




# TAXA DE LETALIDADE
## Calcular a taxa de letalidade por genero (tabela e grafico de barras)

letalidade_total <- last(covid.pt$obitos)/last(covid.pt$confirmados)
letalidade_homens <- last(covid.pt$obitos_m)/last(covid.pt$confirmados_m)
letalidade_mulheres <- last(covid.pt$obitos_f)/last(covid.pt$confirmados_f)

letalidade_nt <- cbind(letalidade_total, letalidade_homens, letalidade_mulheres)

letalidade <- as.data.frame(t(letalidade_nt))
letalidade <- letalidade %>% 
  rownames_to_column(var="G?nero")
  names(letalidade)[2] <- "Taxa_Letalidade"
  
ggplot(letalidade, aes(x=G?nero, y=Taxa_Letalidade*100)) + 
  geom_col(fill="lightblue3", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade da COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)


## Evolucao da letalidade por idade ao longo do tempo (total)

femininos_let_o <- as.data.frame(covid.pt %>% 
  dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))

femininos_let_conf <- as.data.frame(covid.pt %>% 
  dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos_let_o <- as.data.frame(covid.pt %>% 
  dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

masculinos_let_conf <- as.data.frame(covid.pt %>% 
  dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

total_let_o <- femininos_let_o + masculinos_let_o
total_let_conf <- femininos_let_conf + masculinos_let_conf

letalidade_idade_tempo_sd <- total_let_o / total_let_conf

letalidade_idade_tempo <- cbind(covid.pt$data, letalidade_idade_tempo_sd)
names(letalidade_idade_tempo) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

letalidade_idade_tempo_melted <- reshape2::melt(letalidade_idade_tempo, id.vars="Data")
  names(letalidade_idade_tempo_melted)[-1] <- c("Idade", "Taxa_Letalidade")

### falta por os meses no eixo do x
ggplot(letalidade_idade_tempo_melted, aes(x = Data, y = Taxa_Letalidade*100, color = Idade)) +
  geom_line() +
  xlab("Data") +
  ylab("Taxa de Letalidade (%)") +
  labs(title="Taxa de Letalidade por idade da COVID-19 ao longo do tempo") +
  facet_grid(letalidade_idade_tempo_melted$Idade)+ 
  guides(color = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))


## Evolucao da letalidade por idade e por sexo (x=idade)

letalidade_genero_grupo_etario_f_t <- femininos_let_o/femininos_let_conf
letalidade_genero_grupo_etario_f <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_f_t, last))))

letalidade_genero_grupo_etario_f <- letalidade_genero_grupo_etario_f %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_f)[2] <- "Feminino"
letalidade_genero_grupo_etario_f[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


letalidade_genero_grupo_etario_m_t <-masculinos_let_o/masculinos_let_conf
letalidade_genero_grupo_etario_m <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_m_t, last))))

letalidade_genero_grupo_etario_m <- letalidade_genero_grupo_etario_m %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_m)[2] <- "Masculino"
letalidade_genero_grupo_etario_m[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


letalidade_genero_grupo_etario_total_t <- (femininos_let_o + masculinos_let_o)/(femininos_let_conf+masculinos_let_conf)
letalidade_genero_grupo_etario_total <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_total_t, last))))

letalidade_genero_grupo_etario_total <- letalidade_genero_grupo_etario_total %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_total)[2] <- "Total"
letalidade_genero_grupo_etario_total[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


letalidade_genero_grupo_etario_merged_mf <- merge(letalidade_genero_grupo_etario_f, letalidade_genero_grupo_etario_m, by="Idade")
letalidade_genero_grupo_etario_merged <- merge(letalidade_genero_grupo_etario_merged_mf, letalidade_genero_grupo_etario_total, by="Idade")

letalidade_genero_grupo_etario_melted <- reshape2::melt(letalidade_genero_grupo_etario_merged, id.vars= "Idade")
  names(letalidade_genero_grupo_etario_melted)[-1] <- c("G?nero", "Taxa_Letalidade")

ggplot(letalidade_genero_grupo_etario_melted, aes(x = Idade, y = Taxa_Letalidade*100, color = G?nero)) +
  geom_point() +
  xlab("Idade") +
  ylab("Taxa de Letalidade (%)") +
  labs(title="Taxa de Letalidade da COVID-19 por g?nero por idade") +
  facet_grid(letalidade_genero_grupo_etario_melted$G?nero) + 
  guides(color = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))


## Taxa de letalidade ajustada ? idade (tabela) (eventualmente com a evolucao temporal - ainda nao tem)

percentagem_confirmados_0_9 <- (last(covid.pt$confirmados_0_9_f) + last(covid.pt$confirmados_0_9_m)) / last(covid.pt$confirmados)
percentagem_confirmados_10_19 <- (last(covid.pt$confirmados_10_19_f) + last(covid.pt$confirmados_10_19_m)) / last(covid.pt$confirmados)
percentagem_confirmados_20_29 <- (last(covid.pt$confirmados_20_29_f) + last(covid.pt$confirmados_20_29_m)) / last(covid.pt$confirmados)
percentagem_confirmados_30_39 <- (last(covid.pt$confirmados_30_39_f) + last(covid.pt$confirmados_30_39_m)) / last(covid.pt$confirmados)
percentagem_confirmados_40_49 <- (last(covid.pt$confirmados_40_49_f) + last(covid.pt$confirmados_40_49_m)) / last(covid.pt$confirmados)
percentagem_confirmados_50_59 <- (last(covid.pt$confirmados_50_59_f) + last(covid.pt$confirmados_50_59_m)) / last(covid.pt$confirmados)
percentagem_confirmados_60_69 <- (last(covid.pt$confirmados_60_69_f) + last(covid.pt$confirmados_60_69_m)) / last(covid.pt$confirmados)
percentagem_confirmados_70_79 <- (last(covid.pt$confirmados_70_79_f) + last(covid.pt$confirmados_70_79_m)) / last(covid.pt$confirmados)
percentagem_confirmados_80_plus <- (last(covid.pt$confirmados_80_plus_f) + last(covid.pt$confirmados_80_plus_m)) / last(covid.pt$confirmados)

percentagem_confirmados <- as.data.frame(t(cbind(round(percentagem_confirmados_0_9, digits = 5), round(percentagem_confirmados_10_19, digits = 5), round(percentagem_confirmados_20_29, digits = 5), round(percentagem_confirmados_30_39, digits = 5), round(percentagem_confirmados_40_49, digits = 5), round(percentagem_confirmados_50_59, digits = 5), round(percentagem_confirmados_60_69, digits = 5), round(percentagem_confirmados_70_79, digits = 5), round(percentagem_confirmados_80_plus, digits = 5))))

letalidade_aged_ajusted_tabela_1 <- cbind(round(letalidade_genero_grupo_etario_merged[,4], digits = 5), percentagem_confirmados) 
  row.names(letalidade_aged_ajusted_tabela_1) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

letalidade_aged_ajusted_tabela_1 <- letalidade_aged_ajusted_tabela_1 %>% 
  rownames_to_column(var="Idades")
colnames(letalidade_aged_ajusted_tabela_1)[2:3] <- c("Taxa_Letalidade", "Percentagem_confirmados")

cross_products_aged_ajusted <- round(letalidade_aged_ajusted_tabela_1[,2]* letalidade_aged_ajusted_tabela_1[,3], digits=8)

letalidade_aged_ajusted_tabela_2 <- cbind(letalidade_aged_ajusted_tabela_1, cross_products_aged_ajusted)
  colnames(letalidade_aged_ajusted_tabela_2)[4] <- "Cross_products"

total <- c("Total", round(letalidade[1,1], digits = 8) , round(sum(letalidade_aged_ajusted_tabela_2[,3]), digits = 1), sum(letalidade_aged_ajusted_tabela_2[,4]))

letalidade_aged_ajusted <- rbind(letalidade_aged_ajusted_tabela_2, total)


## Taxa de letalidade ajustada ao sexo (tabela)

percentagem_confirmados_fem <- last(covid.pt$confirmados_f) / last(covid.pt$confirmados)
percentagem_confirmados_masc <- last(covid.pt$confirmados_m) / last(covid.pt$confirmados)
percentagem_confirmados_genero <- as.data.frame(t(cbind(round(percentagem_confirmados_fem, digits = 5), round(percentagem_confirmados_masc, digits = 5))))
  rownames(percentagem_confirmados_genero)[1:2] <- c("Feminino", "Masculino")
  colnames(percentagem_confirmados_genero) [1] <- "Percentagem_confirmados"
  
letalidade_fem <- last(covid.pt$obitos_f) / last(covid.pt$confirmados_f)
letalidade_masc <- last(covid.pt$obitos_m) / last(covid.pt$confirmados_m)
letalidade_genero <- as.data.frame(t(cbind(round(letalidade_fem,digits = 5), round(letalidade_masc, digits = 5))))
  rownames(letalidade_genero)[1:2] <- c("Feminino", "Masculino")
  colnames(letalidade_genero) [1] <- "Taxa_Letalidade"

letalidade_sex_adjusted_tabela_1 <- cbind(letalidade_genero, percentagem_confirmados_genero)
  
cross_products_sex_ajusted <- as.data.frame(letalidade_sex_adjusted_tabela_1[,1] * letalidade_sex_adjusted_tabela_1[,2])
  colnames(cross_products_sex_ajusted) <- "Cross_products"

letalidade_sex_adjusted_tabela_2 <- cbind(letalidade_sex_adjusted_tabela_1, cross_products_sex_ajusted)

Total_sex <- c(round(letalidade[1,1], digits = 8), round(sum(letalidade_sex_adjusted_tabela_2$Percentagem_confirmados), digits = 1), sum(letalidade_sex_adjusted_tabela_2$Cross_products))
  
letalidade_sex_adjusted <- rbind(letalidade_sex_adjusted_tabela_2, Total_sex)
row.names(letalidade_sex_adjusted)[3] <- "Total"


## Taxa de letalidade por regioes (tabela e grafico de barras)

letalidade_acores <- last(covid.pt$obitos_acores)/last(covid.pt$confirmados_acores)
letalidade_alentejo <- last(covid.pt$obitos_arsalentejo)/last(covid.pt$confirmados_arsalentejo)
letalidade_algarve <- last(covid.pt$obitos_arsalgarve)/last(covid.pt$confirmados_arsalgarve)
letalidade_centro <- last(covid.pt$obitos_arscentro)/last(covid.pt$confirmados_arscentro)
letalidade_lvt <- last(covid.pt$obitos_arslvt)/last(covid.pt$confirmados_arslvt)
letalidade_madeira <- last(covid.pt$obitos_madeira)/last(covid.pt$confirmados_madeira)
letalidade_norte <- last(covid.pt$obitos_arsnorte)/last(covid.pt$confirmados_arsnorte)

regioes_letalidade_nt <- cbind(letalidade_acores, letalidade_alentejo, letalidade_algarve, letalidade_centro, letalidade_lvt, letalidade_madeira, letalidade_norte)

regioes_letalidade <- as.data.frame(t(regioes_letalidade_nt))
regioes_letalidade <- regioes_letalidade %>% 
  rownames_to_column(var="Regi?es")
names(regioes_letalidade)[2] <- "Taxa_Letalidade"

ggplot(regioes_letalidade, aes(x=Regi?es, y=Taxa_Letalidade*100)) + 
  geom_col(fill="powderblue", width = 0.5) +
  scale_x_discrete(labels= c("A?ores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Madeira", "Norte")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade da COVID-19 por regi?es", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)


## Taxa de letalidade por regioes (mapa)

regioes_letalidade_ordem <- regioes_letalidade[c(2,3,1,4,6,7,5),]
regioes_letalidade_ordem[,1] <- c("Alentejo", "Algarve", "A?ores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_letalidade <- colorBin("Oranges", domain = regioes_letalidade_ordem$Taxa_Letalidade*100, bins = c(0, 1, 2, 3, 4, 5, 6, Inf)) 

labels_letalidade <- paste( 
  "<strong>", regioes_letalidade_ordem[,1],"</strong><br/>", 
  round(regioes_letalidade_ordem[,2]*100, digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_letalidade(regioes_letalidade_ordem$Taxa_Letalidade*100),
              label = labels_letalidade, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_letalidade, values = regioes_letalidade_ordem$Taxa_Letalidade, opacity = 0.5, title = "Taxa de letalidade por regi?o (%)") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Sat?lite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Sat?lite", "Dark"), options = layersControlOptions(collapsed = FALSE))


## Evolucao temporal da taxa de Letalidade por regiao

letalidade_regioes_tempo <- cbind(covid.pt$data, as.data.frame(covid.pt[,49:55]/covid.pt[,4:10]))
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Alrgarve", "A?ores", "Madeira")

letalidade_regioes_tempo_melt <- reshape2::melt(letalidade_regioes_tempo, id.vars="Data")

### falta por os meses no eixo do x e alargar as colunas da color
ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line() +
  #facet_grid(letalidade_regioes_tempo_melt$variable) +
  #guides(color = FALSE) +
  xlab("M?s") +
  ylab("Taxa de Letalidade (%)")+
  labs(title = "Taxa de letalidade da COVID-19 por regi?o ao longo do tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))




# RECUPERADOS
## N? de recuperados por confirmados

recuperados_sd <-  as.data.frame(covid.pt$recuperados/covid.pt$confirmados)
recuperados <- cbind(covid.pt$data, recuperados_sd)
 names(recuperados)[1:2] <- c("Data", "Percentagem_recuperados")

### falta por os meses no eixo do x 
ggplot(recuperados, aes(x = Data, y = Percentagem_recuperados*100)) +
   geom_line() +
   xlab("M?s") +
   ylab("Percentagem de Recuperados") +
   labs(title="Percentagem de Recuperados da COVID-19") +
   theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))
 



# INTERNADOS
##Internados e internados UCI (n? absoluto)

numero_internados <- covid.pt[,c(1,15,16)]
  names(numero_internados)[1:3] <- c("Data", "Internados", "Internados UCI")

numero_internados_melted <-  melt(numero_internados, id.vars="Data")

### falta por os meses no eixo do x
ggplot(numero_internados_melted, aes(x = Data, y = value, color = variable)) +
  geom_line() +
  xlab("M?s") +
  ylab("N? de internados e internados UCI")+
  labs(title = "N? de internados e internados UCI com COVID-19 ao longo do tempo", color="") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))


##Internados  e internados UCI (por n? de casos)

percentagem_internados_sd <- as.data.frame(covid.pt[,15:16]/covid.pt$confirmados)
percentagem_internados <- cbind(covid.pt$data, percentagem_internados_sd)
  names(percentagem_internados)[1:3] <- c("Data", "Internados", "Internados UCI")

percentagem_internados_melted <- reshape2::melt(percentagem_internados, id.vars="Data")

### falta por os meses no eixo do x
ggplot(percentagem_internados_melted, aes(x = Data, y = value*100, color = variable)) +
  geom_line() +
  xlab("M?s") +
  #scale_y_continuous(limits= c(0,5)) +
  ylab("Percentagem de internados e internados UCI")+
  labs(title = "Percentagem de internados e internados UCI com COVID-19 ao longo do tempo", color="") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))




# GitHub

#install.packages("usethis")
# library(usethis)
# 
# usethis::use_git_config(user.name = "carolina Merca",
#                         user.email = "carolinamerka@gmail.com")
# 
# usethis::browse_github_token()
usethis::edit_r_environ()
# c48199e6d72c1dd51767f43d9fb6b87275c15226

f27193b53afafc823dcaa496b2f25bcc73d75179
