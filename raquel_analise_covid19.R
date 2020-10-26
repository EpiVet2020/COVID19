#Importar libraries
library(data.table)
library(ggplot2)
library(grid)
library(dplyr)
library(tibble)
library(reshape2)
library(ggpubr)
library(maps)
library(ggiraph)
library(leaflet)
library(geojsonio)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(zoo)

#Importar dados do covid
data <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")


# Tratar base de dados do covid

## Houve um lapso na base de dados e os obitos cumulativos por idade e por sexo para dia 5/10 (linha 223) estao a 0's. Subituimos esses 0's
## pelos mesmos valores do dia anterior

data[223, 65:84] = data[222, 65:84]


#Data de chr para Date
data$data <- as.Date(data$data,"%d-%m-%Y")

#Mapa de Portugal
mapa_pt <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson",
                        what = "sp")



#SINTOMAS

##FREQU?NCIA
###Criar uma tabela com uma coluna para os sintomas, dar-lhe o nome "sintoma", mudar o nome de cada sintoma na tabela 
###e criar outra coluna para os ?ltimos valores registados de cada sintoma e dar-lhe o nome "percentagem"
sintomas <- as.data.frame(t(data[173,41:46])) %>% 
  rownames_to_column(var = "sintoma")

names(sintomas)[2] = "frequencia"
sintomas[, 1] = c("Tosse", "Febre", "Dificuldade Respirat?ria", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

###Fazer um gr?fico de barras com os sintomas no eixo do x e a percentagem no eixo dos y
ggplot(sintomas, aes(x = sintoma, y = frequencia*100)) +
  geom_col(fill = "slategray3",  width = 0.7) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(sintomas$frequencia*100 + 10))) +
  theme_classic() +
  labs(title = "Frequ?ncia de Sintomas nos Pacientes com COVID19") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  xlab("Sintomas") +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("Frequ?ncia (%)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(frequencia, digits = 2)), vjust = -0.5, size = 4) +
  scale_x_discrete(labels = c("Cefaleia", "Dificuldade\nRespirat?ria", "Dores\nMusculares",
                              "Febre", "Fraqueza\nGeneralizada", "Tosse"))



##EVOLU??O TEMPORAL
###Criar uma tabela com colunas, uma para cada sintoma, sendo o valor para cada dia a frequ?ncia desse 
###sintoma nesse dia vezes o n?mero de confirmados at? esse dia menos a frequ?ncia desse sintoma no dia anterior 
###vezes o n?mero de confirmados at? ao dia anterior, isto para nos dar o n?mero de pessoas com esse sintoma nesse 
###dia apenas, e depois tudo a dividir pelo n?mero de novos confirmados nesse dia para termos o resultado em percentagem. 
sintomas_tempo <- as.data.frame((data[8:173,41:46]*data$confirmados[8:173])
                                -(data[7:172, 41:46]*data$confirmados[7:172]))/data$confirmados_novos[8:173] 

###Acresecentar ? tabela que cri?mos, uma coluna com as datas
sintomas_tempo <- rbind(data[7, 41:46], sintomas_tempo)

###? ainda necess?rio acrescentar a linha 7 pois como s?o as primerias frequ?ncias a aparecer 
###j? indicam a frequ?ncia espec?fica para esse dia
sintomas_tempo <- cbind(data$data[7:173], sintomas_tempo)

###No casos do sintoma Dificuldade Respirat?ria, este s? come?ou a ser registado mais tarde pelo que 
###o primerio valor que j? representa a frequ?ncia espec?fica para esse dia ? o da linha 3, coluna 4
sintomas_tempo[3, 4] <- 0.11

###Mudar os nomes das colunas
names(sintomas_tempo) <- c("Data", "Tosse", "Febre", "Dificuldade respirat?ria", "Cefaleia", 
                           "Dores Musculares",  "Fraqueza Generalizada")

###Fazer o melt da tablea para poder representar a ecolu??o temporal num gr?fico e mudar o nome da coluna 2 para Sintomas
sintomas_tempo_melt <-  melt(sintomas_tempo, id.vars="Data")
names(sintomas_tempo_melt)[2] <- "sintoma"

###Fazer um gr?fico de linhas com a data no eixo do x, a frequ?ncia di?ria dos sintomas no eixo dos y 
###e cada sintoma numa linha
ggplot(sintomas_tempo_melt, aes(x = Data, y = value*100, color = sintoma)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$sintoma) +
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("Frequ?ncia (%)")+
  labs(title = "Frequ?cia de Sintomas ao Longo do Tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))




#CASOS

##CASOS POR GRUPO ET?RIO
###Selecionar as colunas de confirmados feminino para todas as idades e junt?-las numa tabela e
###fazer o mesmo para o masculino
femininos <- as.data.frame(data %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos <- as.data.frame(data %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

###Selecionar o valor mais recente de cada coluna de modo a ficar com o n?mero de casos at? ao momento para cada
###faixa et?ria e para cada g?nero
casos_femininos_idade <- as.data.frame(lapply(femininos, last))
casos_masculinos_idade <- as.data.frame(lapply(masculinos, last))

###Somar a tabela dos femininos com a dos masculinos o que vai dar o n?mero de casos at? ao momento por idade apenas
casos_total_idade <- as.data.frame(casos_femininos_idade + casos_masculinos_idade)

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de casos femininos e mudar coluna
###da faixa et?ria para os nomes adequados
casos_femininos_idade_invertido <- as.data.frame(t(casos_femininos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_idade_invertido)[2] <- "Femininos"
casos_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de casos masculinos e mudar coluna
###da faixa et?ria para os nomes adequados
casos_masculinos_idade_invertido <- as.data.frame(t(casos_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_idade_invertido)[2] <- "Masculinos"
casos_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de casos total e mudar coluna
###da faixa et?ria para os nomes adequados
casos_total_idade_invertido <- as.data.frame(t(casos_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_total_idade_invertido)[2] <- "Total"
casos_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Juntar as 3 tabelas que cri?mos
casos_fem_masc <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_fem_masc_tot <- merge(casos_fem_masc, casos_total_idade_invertido, by = "Idade")

###Fazer melt para poder fazer gr?fico 
casos_fem_masc_tot_melt <- melt(casos_fem_masc_tot, id.vars = "Idade")

###Fazer gr?fico de barras com idade no eixo do x, o n?mero de casos no eixo do y e o g?nero em cada barra
ggplot(casos_fem_masc_tot_melt, aes(x = Idade, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(casos_fem_masc_tot_melt$value + 1000))) +
  theme_classic() +
  labs(title = "Casos Por Idade e G?nero") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("Mortes") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  guides(fill=guide_legend(title="G?nero")) +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("coral2", "lightblue", "saddlebrown"))



##CASOS POR GRUPO ET?RIO LONGO DO TEMPO
###Pegando nas tabelas que fiz?mos por g?nero or faixa et?ria, como os valores na base de dados s?o cumulativos, 
###fazemos o valor desse dia menos o valor do dia anterior para obtermos o n?mero de novos casos por dia, por faixa et?ria
###por g?nero
femininos_novos <- femininos - lag(femininos)
masculinos_novos <- masculinos - lag(masculinos)

###Criar uma tabela com uma coluna para a data e outras colunas com o n?mero de casos por dia por faixa et?ria apenas
###que resultam da soma dos novos casos femininos com os novos casos masculinos
casos_total_tempo <- cbind(data$data, as.data.frame(femininos_novos + masculinos_novos))

###Como  linha 7 que ? a primeira em que h? registo dos casos j? representa o n?mero de casos nesse dia apenas,
###adicion?mos essa linha ? tabela e mud?mos o nome das colunas
casos_total_tempo[7, 2:10] <- femininos[7,] + masculinos[7,]
names(casos_total_tempo)<- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Fazer melt para fazer o gr?fico
casos_total_tempo_melt <- melt(casos_total_tempo, id.vars = "Data")

###Fazer o gr?fico de linhas com a data no eixo do x, o n?mero de casos no eixo do y e a faixa et?ria em cada linha
ggplot(casos_total_tempo_melt, aes(x = Data, y = value, color = variable)) +
  geom_line() +
  facet_grid(casos_total_tempo_melt$variable) +
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("Casos") +
  labs(title = "Casos de COVID19 por Grupo Et?rio ao Longo do Tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##CASOS POR REGI?O
###Criar tabela com uma coluna para a regi?o e outra para o valor mais recente do n?mero de casos confirmados,
###mudar o nome das colunas e mudar o nome das regi?es
casos_regioes <- as.data.frame(t(as.data.frame(lapply(data[,confirmados_arsnorte:confirmados_madeira],
                                                      max, na.rm = TRUE))))%>% 
  rownames_to_column(var = "Regioes")
names(casos_regioes)[2] <- "Casos"
casos_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

###Fazer um gr?fico de barras com as regi?es no eixo do x e o n?mero de casos no eixo do y
ggplot(casos_regioes, aes(x = Regioes, y = Casos)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(casos_regioes$Casos + 10000))) +
  theme_classic() +
  labs(title = "n?mero de Casos por Regi?o") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = Casos), vjust = -0.5, size = 4)

###mapa
####Definir intervalos que queremos na legenda
bins =  c(0, 200, 600, 1200, 1400, 10000, Inf)

####Colocar as Regi?oes da tabela pela mesma ordem que a dos poligonos
casos_regioes_ordem <- casos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores para o mapa
pal <- colorBin("YlOrRd", domain = casos_regioes_ordem[,2], bins = bins)

####Definir legenda que aparece quando se passa o rato pelo mapa
labels <- sprintf(
  "<strong>%s</strong><br/>%g casos",
  casos_regioes_ordem[,1], casos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal(casos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = casos_regioes_ordem$Casos, opacity = 0.7, title = "N? Casos",
            position = "bottomright")



##INCID?NCIA (novos/popula??o de risco)
###Vari?veis com a popula??o total, feminina e masculina com base no INE
populacao_pt = 10295909
mulheres_pt = 5435932
homens_pt = 4859977

###C?lculo da incid?ncia criando uma tabbela para o total, outra para mulheres e outra para homens
incidencia_total <- as.data.frame(data$confirmados_novos/ (populacao_pt - data$confirmados - data$obitos))
incidencia_homens <- as.data.frame((data$confirmados_m - lag(data$confirmados_m)) 
                                   / (homens_pt - data$confirmados_m - data$obitos_m)) 
incidencia_mulheres <- as.data.frame((data$confirmados_f - lag(data$confirmados_f)) 
                                     / (mulheres_pt - data$confirmados_f - data$obitos_f))

##Remover valores negativos devido a erro na base de dados original em que valor cumulativo do n?mero casos homens 
###e mulheres era 0 e n?o devia
incidencia_homens[174:175,] <- NA
incidencia_mulheres[174:175,] <- NA

###Criar uma tabela com as 3 tabelas anteriores, adicionando uma coluna com a data e mudar os nomes das colunas
incidencia <- data.frame(data$data, incidencia_total, incidencia_mulheres, incidencia_homens)
names(incidencia) <- c("Data", "Total", "Mulheres", "Homens")

###Fazer melt para poder fazer gr?fico de linhas e dar nome ? coluna do g?nero
incidencia_melt <- melt(incidencia, id.vars = "Data")
names(incidencia_melt)[2] <- "Genero"

###Fazer gr?fico de linhas com data no eixo do x, a incidencia no eixo do y e o g?nero em cada linha
ggplot(incidencia_melt, aes(x = Data, y = value*100, color = Genero)) +
  geom_line() +
  facet_grid(incidencia_melt$Genero) +
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("incid?ncia (%)") + 
  labs(title = "incid?ncia Di?ria por g?nero") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##INCID?NCIA POR REGI?O AO LONGO DO TEMPO
###Valores da popula??o de cada Regi?o com base no INE
acores = 242796
alentejo = 705018
algarve = 438635
centro = 2216927
lisboa = 2854802
madeira = 254254
norte = 3573961

###Criar uma tabela com uma coluna para as Regi?oes e outra para o n?mero de pessoas nessa Regi?o
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "A?ores", "madeira"))

###Fazer com que cada coluna seja uma Regi?o e repetir cada n?mero as vezes necess?rias para ficar com o n?mero
###igual ao das colunas da base de dados
populacao_regioes_rep <- as.data.frame(t(populacao_regioes[rep(seq_len(ncol(populacao_regioes)), each = nrow(data))]))

###Calcular a incid?ncia em cada Regi?o fazendo os casos novos por Regi?o a dividir pela popula??o da Regi?o 
###menos os confirmados da Regi?o menos os ?bitos da Regi?o e dar nomes a cada coluna
incidencia_regioes <- cbind(data$data, (as.data.frame(data[, confirmados_arsnorte:confirmados_madeira] 
                                                      - lag(data[, confirmados_arsnorte:confirmados_madeira]))) 
                            / (populacao_regioes_rep - as.data.frame(data[,confirmados_arsnorte:confirmados_madeira] 
                                                                     - data[,obitos_arsnorte:obitos_madeira])))
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

###Fazer melt para fazer o gr?fico e dar nomes a cada coluna
incidencia_regioes_melt <- melt(incidencia_regioes, id.vars = "Data")
names(incidencia_regioes_melt) <- c("data", "regiao", "valor")

###Fazer o gr?fico de linhas com a data no eixo do x, a incid?ncia no eixo do y e a Regi?o em cada linha
ggplot(incidencia_regioes_melt, aes(x = data, y = valor*100, color = regiao)) +
  geom_line() + 
  labs(x = "M?s", y = "incid?ncia (%)", title = "incid?ncia de COVID19 por Regi?o ao Longo do Tempo") +
  facet_grid(incidencia_regioes_melt$regiao) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  guides(color = FALSE) +
  theme(strip.text.y = element_text(size = 8, colour = "burlywood4", angle = 0)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "Black", hjust = 0.5)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##INCID?NCIA POR REGI?O
###Definir intervalos para legenda
bins_2 =  c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, Inf)

###Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regi?oes
incidencia_regioes_ordem <- as.data.frame(t(as.data.frame(lapply(incidencia_regioes[,c(5, 6, 7, 3, 8, 2, 4)], last)))) %>% 
  rownames_to_column(var = "Regiao") 
incidencia_regioes_ordem[,1] <- c("Alentejo", "Algarve", "A?ores","Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo")

###Definir palete de cores para mapa
pal_2 <- colorBin("YlOrRd", domain = incidencia_regioes_ordem[,2]*100, bins = bins_2)

###Definir legenda quando se passa com o rato por cima
labels_2 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 incid?ncia",
  incidencia_regioes_ordem[,1], round(incidencia_regioes_ordem[,2]*100, digits = 4)
) %>% lapply(htmltools::HTML)

###Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_2(incidencia_regioes_ordem[,2]*100), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_2,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = incidencia_regioes_ordem[,2]*100, opacity = 0.7, title = "incid?ncia",
            position = "bottomright")




#MORTES

##N?MERO
###Criar uma tabela com o valor mais recente, que ? tamb?m o valor mais alto, do ?bitos femininos e masculinos, 
###fazendo uma coluna para o g?nero e outra para o n?mero de ?bitos
obitos_genero <- as.data.frame(t(as.data.frame(lapply(data[,obitos_f:obitos_m], max, na.rm = TRUE)))) %>% 
  rownames_to_column(var = "genero")
names(obitos_genero)[2] <- "mortes"

###Fazer um gr?fico de barras com o genero no eixo do x e o n?mero de mortes no eixo do y
ggplot(obitos_genero, aes(x = genero, y = mortes)) +
  geom_col(fill = "darksalmon") + 
  labs(title = "n?mero de Mortes Por g?nero", x = "") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(obitos_genero$mortes + 100))) +
  theme_classic() +
  scale_x_discrete(labels = c("Feminino", "Masculino")) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = mortes), vjust = -0.5, size = 4) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5))



##MORTES POR GRUPO ET?RIO E G?NERO
###Selecionar as colunas de obitos feminino para todas as idades e junt?-las numa tabela e 
###fazer o mesmo para o masculino
femininos_mortes <- as.data.frame(data %>% 
                                    dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos_mortes <- as.data.frame(data %>% 
                                     dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

###Selecionar o valor mais recente de cada coluna de modo a ficar com o n?mero de ?bitos at? ao momento 
###para cada faixa et?ria e para cada g?nero
mortes_femininos_idade <- as.data.frame(lapply(femininos_mortes, last))
mortes_masculinos_idade <- as.data.frame(lapply(masculinos_mortes, last))

###Somar a tabela dos femininos com a dos masculinos o que vai dar o n?mero de ?bitos at? ao momento por idade apenas
mortes_total_idade <- as.data.frame(mortes_femininos_idade + mortes_masculinos_idade)

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de ?bitos femininos e mudar coluna da 
###faixa et?ria para os nomes adequados
mortes_femininos_idade_invertido <- as.data.frame(t(mortes_femininos_idade))%>% 
  rownames_to_column(var = "Idade")
names(mortes_femininos_idade_invertido)[2] <- "Femininos"
mortes_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de ?bitos masculinos e mudar coluna da 
###faixa et?ria para os nomes adequados
mortes_masculinos_idade_invertido <- as.data.frame(t(mortes_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_masculinos_idade_invertido)[2] <- "Masculinos"
mortes_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Criar tabela com uma colunda para a faixa et?ria e outra para o n?mero de ?bitos total e mudar coluna da 
###faixa et?ria para os nomes adequados
mortes_total_idade_invertido <- as.data.frame(t(mortes_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_total_idade_invertido)[2] <- "Total"
mortes_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Juntar as 3 tabelas que cri?mos
mortes_fem_masc <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_fem_masc_tot <- merge(mortes_fem_masc, mortes_total_idade_invertido, by = "Idade")

###Fazer melt para poder fazer gr?fico 
mortes_fem_masc_tot_melt <- melt(mortes_fem_masc_tot, id.vars = "Idade")

###Fazer gr?fico de barras com idade no eixo do x, o n?mero de ?bitos no eixo do y e o g?nero em cada barra
ggplot(mortes_fem_masc_tot_melt, aes(x = Idade, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortes_fem_masc_tot_melt$value + 100))) +
  theme_classic() +
  labs(title = "Mortes Por Idade e g?nero") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("Mortes") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  guides(fill=guide_legend(title="g?nero")) +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("coral2", "lightblue", "saddlebrown"))



##MORTES POR REGI?O
###Criar tabela com uma coluna para as Regi?oes e outra para o n?mero mais recente total de ?bitos e dar nomes
mortos_regioes <- as.data.frame(t(as.data.frame(lapply(data[,obitos_arsnorte:obitos_madeira], last)))) %>% 
  rownames_to_column(var = "Regioes")
names(mortos_regioes)[2] <- "Mortes"
mortos_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

###Fazer gr?fico com Regi?oes no eixo do x e n?mero de mortes no eixo do y
ggplot(mortos_regioes, aes(x = Regioes, y = Mortes)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortos_regioes$Mortes + 125))) +
  theme_classic() +
  labs(title = "n?mero de Mortes por Regi?o") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = Mortes), vjust = -0.5, size = 4)

###mapa
####Definir intervalos da legenda
bins_3 =  c(0, 25, 50, 100, 500, 1000, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortos_regioes_ordem <- mortos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_3 <- colorBin("YlOrRd", domain = mortos_regioes_ordem[,2], bins = bins_3)

####Definir a legenda quando se passa com o rato por cima
labels_3 <- sprintf(
  "<strong>%s</strong><br/>%g Mortos",
  incidencia_regioes_ordem[,1], mortos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_3(mortos_regioes_ordem[,2]), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_3,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = incidencia_regioes_ordem[,2]*100, opacity = 0.7, title = "N? Mortos",
            position = "bottomright")



##MORTALIDADE (obitos/popula??o)
###C?lculo da mortalidade total, feminina e masculina com base no valor mais recente dos ?bitos, que ? tamb?m o mais alto
mortalidade_total <- max(data$obitos, na.rm = TRUE) / populacao_pt
mortalidade_mulheres <- max(data$obitos_f, na.rm = TRUE) / mulheres_pt
mortalidade_homens <- max(data$obitos_m, na.rm =TRUE) / homens_pt

###Criar uma tabela com uma coluna para o g?nero e outra para o valor da mortalidade e dar nome apropriado aos generos
mortalidade <- data.frame(t(data.frame(mortalidade_total, mortalidade_mulheres, mortalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(mortalidade)[2] <- "mortalidade"
mortalidade[, 1] <- c("Total", "Mulheres", "Homens")

###Criar um gr?fico de barras com o genero no eixo do x e a mortalidade no eixo do y
ggplot(mortalidade, aes(x = genero, y = mortalidade*100)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortalidade$mortalidade*106))) +
  theme_classic() +
  labs(title = "Mortalidade", x = "") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("%") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(mortalidade, digits = 4)), vjust = -0.5, size = 4)



##MORTALIDADE POR REGI?O
###Criar tabela com uma coluna para as Regi?oes e outra para a mortalidade mais recente e dar nomes apropriados
mortalidade_regioes <- data.frame(t(as.data.frame(lapply(data[, obitos_arsnorte:obitos_madeira], last))) 
                                  / populacao_regioes) %>% 
  rownames_to_column(var = "Regiao")
names(mortalidade_regioes)[2] <- "Mortalidade"
mortalidade_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

###Fazer gr?fico com Regi?oes no eixo do x e mortaldiade no eixo do y
ggplot(mortalidade_regioes, aes(x = Regiao, y = Mortalidade*100)) +
  geom_col(fill = "gray") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortalidade_regioes$Mortalidade*106))) +
  theme_classic() +
  labs(title = "Mortalidade Por Regi?o") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("Mortalidade (%)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(round(Mortalidade, digits = 5))), vjust = -0.5, size = 4)

###mapa
####Definir intervalos da legenda
bins_4 =  c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.02, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortalidade_regioes_ordem <- mortalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_4 <- colorBin("YlOrRd", domain = mortalidade_regioes_ordem[,2]*100, bins = bins_4)

####Definir a legenda quando se passa com o rato por cima
labels_4 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Mortalidade",
  mortalidade_regioes_ordem[,1], round(mortalidade_regioes_ordem[,2]*100, digits =  3)
) %>% lapply(htmltools::HTML)

####Fazer o map
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_4(mortalidade_regioes_ordem[,2]*100), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_4,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = mortalidade_regioes_ordem[,2]*100, opacity = 0.7, title = "Mortalidade",
            position = "bottomright")



##LETALIDADE (obitos/confirmados)
###C?lculo da letalidade total, feminina e masculina com base no valor mais recente dos ?bitos, 
###que ? tamb?m o mais alto, e no valor mais recente de confirmados
letalidade_total <- max(data$obitos, na.rm = TRUE) / last(data$confirmados)
letalidade_mulheres <- max(data$obitos_f, na.rm = TRUE) / last(data$confirmados_f)
letalidade_homens <- max(data$obitos_m, na.rm = TRUE) / last(data$confirmados_m)

###Criar uma tabela com uma coluna para o g?nero e outra para o valor da letalidade e dar nome apropriado aos generos
letalidade <- data.frame(t(data.frame(letalidade_total, letalidade_mulheres, letalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(letalidade)[2] <- "letalidade"
letalidade[, 1] <- c("Total", "Mulheres", "Homens")

###Criar um gr?fico de barras com o genero no eixo do x e a letalidade no eixo do y
ggplot(letalidade, aes(x = genero, y = letalidade*100)) +
  geom_col(fill = "salmon1") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(letalidade$letalidade*100 + 4))) +
  theme_classic() +
  labs(title = "Letalidade", x = "") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("%") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(letalidade, digits = 4)), vjust = -0.5, size = 4)


##LETALIDADE LONGO DO TEMPO
###Calcular letalidade toal, para mulheres e para homens, criando uma tabela para cada com uma coluna para a 
###data e outra para os valores da letalidade para cada dia
letalidade_tempo_total <- cbind(data$data, as.data.frame((data$obitos / data$confirmados)*100))
letalidade_tempo_mulheres <- cbind(data$data, as.data.frame((data$obitos_f / data$confirmados_f)*100))
letalidade_tempo_homens <- cbind(data$data, as.data.frame((data$obitos_m / data$confirmados_m)*100))

###Juntar as 3 tabelas numa s?o mudando os nomes de cada coluna
letalidade_tempo_total_mulheres <- merge(letalidade_tempo_total, letalidade_tempo_mulheres, by ="data$data")
letalidade_tempo <- merge(letalidade_tempo_total_mulheres, letalidade_tempo_homens, by="data$data")
names(letalidade_tempo) <- c("Data", "Total", "Mulheres", "Homens")

###Fazer o melt para poder faze um gr?fico de linhas
letalidade_tempo_melt <- melt(letalidade_tempo, id.vars = "Data")
names(letalidade_tempo_melt) <- c("Data", "Genero", "Letalidade")

###Fazer gr?fico de linhas com a data no eixo do x, a letalidade no eixo do y e o g?nero em cada linha
letalidade_tempo_grafico <- ggplot(letalidade_tempo_melt, aes(x = Data, y = Letalidade, color = Genero)) +
  geom_line() +
  xlab("M?s") +
  ylab("Letalidade (%)")+
  labs(title = "Letalidade ao Longo do Tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(letalidade_tempo_grafico, add_tracey = "Letalidade")



##LETALIDADE POR GRUPO ET?RIO E G?NERO
###Tabela com n?mero de casos confirmados por faixa et?ria por g?nero
casos_genero <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_genero_total <-  merge(casos_genero, casos_total_idade_invertido, by = "Idade")

###Tabela com n?mero de ?bitos por faixa et?ria por g?nero
mortes_genero <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_genero_total <-  merge(mortes_genero, mortes_total_idade_invertido, by = "Idade")

###Criar tabela com uma coluna com as faixas et?rioas e outra com a letalidade e dar nomes ?s colunas
letalidade_genero <- cbind(casos_femininos_idade_invertido[,1], (mortes_genero_total[,2:4]/casos_genero_total[,2:4]))
names(letalidade_genero) <- c("Idade", "Feminino", "Masculino", "Total")

###Fazer melt para poder fazer gr?fico
letalidade_genero_melt <- melt(letalidade_genero, id.vars = "Idade")

###Fazer gr?fico com idade no eixo do x, letalidade no eixo do y e faixa et?ria em cada linha
letalidade_genero_grafico <- ggplot(letalidade_genero_melt, aes(x = Idade, y = value*100, color = variable, 
                                                                tooltip = round(value*100, digits = 2), data_id = value)) +
  geom_point_interactive() +
  guides(color = FALSE) +
  facet_grid(letalidade_genero_melt$variable) +
  xlab("Faixa et?ria (anos)") +
  ylab("Letalidade (%)") +
  labs(title = "Letalidade por Faixa et?ria por g?nero") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15))

###Animar o gr?fico 
girafe(code = print(letalidade_genero_grafico),
       options = list(
         opts_zoom(max = 5),
         opts_hover(css = "fill:black;")
       ))



##LETALIDADE POR GRUPO ET?RIO AO LONGO DO TEMPO
###Tabela com o n?mero de mortes total di?rios por faixa et?ria
total_mortes_novos <- femininos_mortes + masculinos_mortes

###Tabela com o n?mero de casos totais di?rios por faixa et?ria
total_casos_novos <- femininos + masculinos

###Tabela com uma coluna com a data e outras com cada faixa et?ria onde tem o valor da letalidade total para cada dia e 
###dar nomes ?s colunas
letalidade_tempo_idade <- cbind(data$data, total_mortes_novos/total_casos_novos)
names(letalidade_tempo_idade) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###Fazer melt para fazer o gr?fico
letalidade_tempo_idade_melt <- melt(letalidade_tempo_idade, id.vars = "Data")

###Fazer o gr?fico
ggplot(letalidade_tempo_idade_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line() +
  facet_grid(letalidade_tempo_idade_melt$variable) +
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("Letalidade (%)") +
  labs(title = "Letalidade por Faixa et?ria ao Longo do Tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##LETALIDADE POR REGI?O
###Fazer uma tabela com uma coluna com a Regi?o e outra com a letalidade para cada Regi?o e dar nomes adequados
letalidade_regioes <- data.frame(t(as.data.frame(lapply(data[, obitos_arsnorte:obitos_madeira], last)) 
                                   / (as.data.frame(lapply(data[, confirmados_arsnorte:confirmados_madeira], last))))) %>% 
  rownames_to_column(var = "Regiao")
names(letalidade_regioes)[2] <- "Letalidade"
letalidade_regioes[, 1] <- c("Norte", "Centro", "Lisboa e Vale \ndo Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

###Fazer o gr?fico com a Regi?o no eixo do x e a letalidade no eixo do y
ggplot(letalidade_regioes, aes(x = Regiao, y = Letalidade*100)) +
  geom_col(fill = "gray") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(letalidade_regioes$Letalidade*106))) +
  theme_classic() +
  labs(title = "Letalidade Por Regi?o") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "Black", hjust = 0.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  theme(axis.text.x = element_text(size=12, color = "black")) +
  ylab("Letalidade (%)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(round(Letalidade, digits = 5))), vjust = -0.5, size = 4)

###mapa
####Definir intervalos da legenda
bins_5 =  c(0, 1, 2, 3, 4, 5, 6, 7, Inf)

####Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
letalidade_regioes_ordem <- letalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_5 <- colorBin("YlOrRd", domain = letalidade_regioes_ordem[,2]*100, bins = bins_5)

####Definir a legenda quando se passa com o rato por cima
labels_5 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Letalidade",
  letalidade_regioes_ordem[,1], round(letalidade_regioes_ordem[,2]*100, digits =  2)
) %>% lapply(htmltools::HTML)

####Fazer o mapa
leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_5(letalidade_regioes_ordem[,2]*100), 
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_5,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal, values = letalidade_regioes_ordem[,2]*100, opacity = 0.7, title = "Letalidade",
            position = "bottomright")



##LETALIDADE POR REGI?O AO LONGO DO TEMPO
###Criar tabela com uma coluna para data e outras colunas uma para cada regi?o tendo l? os valores da letalidade di?ria
###e dar nomes ?s colunas
letalidade_regioes_tempo <- cbind(data$data, as.data.frame(data[,49:55]/data[,4:10]))
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                     "Alrgarve", "A?ores", "Madeira")

###Fazer melt para poder fazer o gr?fico
letalidade_regioes_tempo_melt <- melt(letalidade_regioes_tempo, id.vars = "Data")

###Fazer o gr?fico de linhas com data no eixo do x, letalidade no eixo do y e regiao em cada linha
ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line() +
  guides(color = FALSE) +
  xlab("M?s") +
  ylab("Letalidade (%)") +
  labs(title = "Letalidade por Regi?oes ao Longo do Tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")




#RECUPERADOS 

##Criar tabela com coluna para data e outra coluna para a percentagem de recuperados em cada dia e dar nomes ?s colunas
recuperados <- cbind(data$data, as.data.frame(data$recuperados / data$confirmados))
names(recuperados) <- c("Data", "Recuperados")

##Fazer gr?fico de linhas com data no eixo do x e percentagem recuperados no eixo y
ggplot(recuperados, aes(x = Data, y = Recuperados*100)) +
  geom_line(color = "salmon1", size = 1) +
  labs(title = "Percentagem de Recuperados dentro dos Infetados", y = "Recuperados (%)", x = "M?s") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")




#INTERNADOS

##N?MERO DE INTERNADOS AO LONGO DO TEMPO
###Fazer melt das colunas data, internados e internados UCI para ter n?mero de internados em cada dia
internados <- melt(data[,c(1, 15, 88, 16)], id.vars = "data")

###Fazer gr?fico de linhas com data no eixo do x, n?mero de internados no eixo do y e tipo de internamento nas linhas
ggplot(internados, aes(x = data, y =value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "n?mero de Internados ao Longo do Tempo", y = "n?mero Pessoas", x = "M?s", color = "") +
  scale_color_discrete(labels = c("Internados", "Internados Enfermaria", "Internados UCI")) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")



##PERCENTAGEM DE INTERNADOS AO LONGO DO TEMPO
###Fazer melt para ter tabela com coluna da data, coluna do tipo de internamento e coluna com percentagem de internados
###que s?o os internados a dividir pelos confirmados e dar nomes ?s colunas
internados_confirmados <- melt((cbind(data$data, (as.data.frame(lapply(data[,c(15, 88, 16)], 
                                                                       function(x) {x/data[, 3]}))))), id.vars = "data$data")
names(internados_confirmados) <- c("data", "internados", "percentagem")

###Fazer gr?fico de linhas com data no eixo do x, percentagem internados no eixo do y e tipo de internamento em cada linha
ggplot(internados_confirmados, aes(x = data, y =percentagem*100, color = internados)) +
  geom_line(size = 1) +
  labs(title = "Percentagem Internados dentro dos Infetados ao Longo do Tempo", y = "Percentagem (%)",
       x = "M?s", color = "") +
  scale_color_discrete(labels = c("Internados","Internados Enfermaria", "Internados UCI")) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), 
                                  size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(breaks = "months", date_labels = "%B")

