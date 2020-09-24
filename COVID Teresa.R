#Libraries
library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(grid)
library(tibble)
library(leaflet)
library(ggmap)
library(rgdal)
library(geojsonio)

setwd("C:/Users/teres/Desktop")

#Data
covid19pt<-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

#Transformar para formato de data
covid19pt$data<-as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")

# SINTOMAS
##frequencia de sintomas por casos confirmados (ultimos dados recolhidos)
## fazer uma tabela e fazer um grafico de barras

sintomas_geral <- as.data.frame(t(covid19pt[173, 41:46]))

sintomas_geral <- sintomas_geral %>%
  rownames_to_column(var="Sintomas")
  names(sintomas_geral)[2] <- "Frequencia"

ggplot(data=sintomas_geral, aes(x = Sintomas, y = Frequencia*100)) +
  geom_col(colour= "#DD8888", fill="#DD8888", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade\nrespiratoria", "Dores musculares", "Febre", "Fraqueza generalizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequencia (%)", title = "Frequencia de Sintomas da COVID-19",x="Sintomas") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequencia, digits=4)), vjust=-0.5)
  
## Avaliar se as frequencias relativas se foram alterando ao longo do tempo

sintomas_tempo <- as.data.frame((covid19pt[8:173, 41:46]*covid19pt$confirmados[8:173]) - covid19pt[7:172, 41:46]*covid19pt$confirmados[7:172]) / covid19pt$confirmados_novos[8:173]
sintomas_tempo <- rbind(covid19pt[7,41:46], sintomas_tempo)
sintomas_tempo <- cbind(covid19pt$data[7:173], sintomas_tempo)

sintomas_tempo[3,4] <- 0.11

names(sintomas_tempo) <- c("Data", "Tosse", "Febre", "Dificuldades Respiratorias", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

sintomas_tempo <- melt(sintomas_tempo, id.vars="Data")
names(sintomas_tempo)[2:3] <- c("Sintomas", "Valores")

## Fazer um grafico de linhas
ggplot(sintomas_tempo, aes(x = Data, y = Valores*100, color = Sintomas, group=1)) +
  geom_line() +
  scale_color_discrete(labels = c("Tosse", "Febre", "Dificuldade Respiratoria", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")) +
  xlab("Sintomas") +
  ylab("Frequencia (%)") +
  labs(title="Evolucao de sintomas ao longo do tempo")

#GENERO
# Casos e Mortalidade entre homens e mulheres
mortes_total <- last(covid19pt$obitos)
mortes_mulheres <- last(covid19pt$obitos_f)
mortes_homens <- last(covid19pt$obitos_m)

table_mortes <-cbind(mortes_total, mortes_homens, mortes_mulheres)

numero_mortes <- as.data.frame(t(table_mortes))

numero_mortes <- numero_mortes %>% 
  rownames_to_column(var="Genero")
  names(numero_mortes)[2] <- "N_mortes"

ggplot(numero_mortes, aes(x=Genero, y=N_mortes)) + 
  geom_col(fill="salmon", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="N de mortes", title = "Numero de mortes por COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = N_mortes), vjust=-0.5)

##Adicionar duas novas colunas de obitos diarios fem e masc
covid19pt2 <- covid19pt %>%
  mutate(obitosdiariosf = covid19pt$obitos_f - lag(covid19pt$obitos_f))

covid19pt3 <- covid19pt2 %>%
  mutate(obitosdiariosm = covid19pt2$obitos_m - lag(covid19pt2$obitos_m))

##Media de obitos diarios fem e masc
mean(covid19pt3$obitosdiariosf, na.rm = TRUE)
mean(covid19pt3$obitosdiariosm, na.rm = TRUE)

##Adicionar duas novas colunas de casos novos diarios fem e masc
covid19pt4 <- covid19pt3 %>%
  mutate(casosdiariosf = covid19pt3$confirmados_f - lag(covid19pt3$confirmados_f))

covid19pt5 <- covid19pt4 %>%
  mutate(casosdiariosm = covid19pt4$confirmados_m - lag(covid19pt4$confirmados_m))

##Media de casos diarios fem e masc
mean(covid19pt5$casosdiariosf, na.rm = TRUE)
mean(covid19pt5$casosdiariosm, na.rm = TRUE)


# Dados INE - 10 295 909 total populacao ; 4 859 977 homens ; 5 435 932 mulheres
# Calcular a "taxa" de mortalidade
# Tabela e Grafico de barras
populacao_total <- 10295909
populacao_f <- 5435932
populacao_m <- 4859977

Mortalidade <- last(covid19pt$obitos) / populacao_total
Mortalidadef <- last(covid19pt$obitos_f) / populacao_f
Mortalidadem <- last(covid19pt$obitos_m) / populacao_m

tm_t <- cbind(Mortalidade, Mortalidadem, Mortalidadef)

tm <- as.data.frame(t(tm_t))
tm <- tm %>% 
  rownames_to_column(var="Genero")
  names(tm)[2] <- "Taxa_Mortalidade"

ggplot(tm, aes(x=Genero, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Total", "Mulheres", "Homens")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


# Calcular letalidade covid por genero
# Tabela e Grafico de Barras
Letalidadetotal <- last(covid19pt$obitos / covid19pt$confirmados)
Letalidadef <- last(covid19pt$obitos_f / covid19pt$confirmados_f)
Letalidadem <- last(covid19pt$obitos_m / covid19pt$confirmados_m)           

tl_t <- cbind(Letalidadetotal, Letalidadem, Letalidadef)

tl <- as.data.frame(t(tl_t))
tl <- tl %>%
  rownames_to_column(var= "Genero")
  names(tl)[2] <- "Taxa_Letalidade"

ggplot(tl, aes(x=Genero, y=Taxa_Letalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Mulheres", "Homens", "Total")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)
  
#Fica para uma proxima - Boxplot (Nao fica fixe)
boxplot(Mortalidadef, Mortalidadem, Letalidadef, Letalidadem, 
        main = "Mortalidade e Letalidade por genero",
        names = c("Mortalidade Feminina", "Mortalidade Masculina", "Letalidade Feminina", "Letalidade Masculina"),
        las = 2,
        col = c("pink", "blue"),
        border = "brown",
        vertical = TRUE,
        notch = TRUE
)

# Taxa de incidencia cumulativa por genero (diaria)
incidenciacumulativaf <- (covid19pt$confirmados_novos[204] / populacao_f)
incidenciacumulativam <- (covid19pt$confirmados_novos[204] / populacao_m)


#GRUPO ETARIO 
## Casos por grupo etario
casos0_9 <- sum(max(covid19pt$confirmados_0_9_f, na.rm = TRUE), max(covid19pt$confirmados_0_9_m, na.rm = TRUE))
casos10_19 <- sum(max(covid19pt$confirmados_10_19_f, na.rm = TRUE), max(covid19pt$confirmados_10_19_m, na.rm = TRUE))
casos20_29 <- sum(max(covid19pt$confirmados_20_29_f, na.rm = TRUE), max(covid19pt$confirmados_20_29_m, na.rm = TRUE))
casos30_39 <- sum(max(covid19pt$confirmados_30_39_f, na.rm = TRUE), max(covid19pt$confirmados_30_39_m, na.rm = TRUE))
casos40_49 <- sum(max(covid19pt$confirmados_40_49_f, na.rm = TRUE), max(covid19pt$confirmados_40_49_m, na.rm = TRUE))
casos50_59 <- sum(max(covid19pt$confirmados_50_59_f, na.rm = TRUE), max(covid19pt$confirmados_50_59_m, na.rm = TRUE))
casos60_69 <- sum(max(covid19pt$confirmados_60_69_f, na.rm = TRUE), max(covid19pt$confirmados_60_69_m, na.rm = TRUE))
casos70_79 <- sum(max(covid19pt$confirmados_70_79_f, na.rm = TRUE), max(covid19pt$confirmados_70_79_m, na.rm = TRUE))
casos80plus <- sum(max(covid19pt$confirmados_80_plus_f, na.rm = TRUE), max(covid19pt$confirmados_80_plus_m, na.rm = TRUE))

## Mortes por grupo etario
obitos0_9 <- sum(max(covid19pt$obitos_0_9_f, na.rm = TRUE), max(covid19pt$obitos_0_9_m, na.rm = TRUE))
obitos10_19 <- sum(max(covid19pt$obitos_10_19_f, na.rm = TRUE), max(covid19pt$obitos_10_19_m, na.rm = TRUE))
obitos20_29 <- sum(max(covid19pt$obitos_20_29_f, na.rm = TRUE), max(covid19pt$obitos_20_29_m, na.rm = TRUE))
obitos30_39 <- sum(max(covid19pt$obitos_30_39_f, na.rm = TRUE), max(covid19pt$obitos_30_39_m, na.rm = TRUE))
obitos40_49 <- sum(max(covid19pt$obitos_40_49_f, na.rm = TRUE), max(covid19pt$obitos_40_49_m, na.rm = TRUE))
obitos50_59 <- sum(max(covid19pt$obitos_50_59_f, na.rm = TRUE), max(covid19pt$obitos_50_59_m, na.rm = TRUE))
obitos60_69 <- sum(max(covid19pt$obitos_60_69_f, na.rm = TRUE), max(covid19pt$obitos_60_69_m, na.rm = TRUE))
obitos70_79 <- sum(max(covid19pt$obitos_70_79_f, na.rm = TRUE), max(covid19pt$obitos_70_79_m, na.rm = TRUE))
obitos80plus <- sum(max(covid19pt$obitos_80_plus_f, na.rm = TRUE), max(covid19pt$obitos_80_plus_m, na.rm = TRUE))

## Taxa de mortalidade por grupo etario
mort0_9 <- obitos0_9 / 894631
mort10_19 <- obitos10_19 / 1056679 
mort20_29 <- obitos20_29 / 1092080
mort30_39 <- obitos30_39 / 1250448 
mort40_49 <- obitos40_49 / 1575225 
mort50_59 <- obitos50_59 / 1482121 
mort60_69 <- obitos60_69 / 1293301 
mort70_79 <- obitos70_79 / 973123 
mort80plus <- obitos80plus / 668660

## Taxa de letalidade por grupo etario
let0_9 <- obitos0_9 / casos0_9 
let10_19 <- obitos10_19 / casos10_19 
let20_29 <- obitos20_29 / casos20_29
let30_39 <- obitos30_39 / casos30_39 
let40_49 <- obitos40_49 / casos40_49 
let50_59 <- obitos50_59 / casos50_59 
let60_69 <- obitos60_69 / casos60_69 
let70_79 <- obitos70_79 / casos70_79 
let80plus <- obitos80plus / casos80plus

## Tabela e Grafico

mort_etaria <- c(mort0_9, mort10_19, mort20_29, mort30_39, mort40_49, mort50_59, mort60_69, mort70_79, mort80plus)

let_etaria <- c(let0_9, let10_19, let20_29, let30_39, let40_49, let50_59, let60_69, let70_79, let80plus)


mort_etaria <- data.frame(Grupos = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "mais de 80"), Mortalidade = mort_etaria)
ggplot(data=mort_etaria, aes(x=Grupos, y=Mortalidade)) +
  geom_bar(stat="identity", color = "seagreen2", fill = "seagreen2") +
  geom_text(aes(label = scales::percent(Mortalidade, digits = 2)), vjust = -0.5)

let_etaria <- data.frame (Grupos = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "mais de 80"), Letalidade = let_etaria)
ggplot(data=let_etaria, aes(x=Grupos, y=Letalidade)) +
  geom_bar(stat = "identity", color = "turquoise4", fill = "turquoise4") + 
  geom_text(aes(label = scales::percent(Letalidade, digits = 2)), vjust = -0.5)

##Evolução temporal letalidade por grupo etário
let_dia_0_9 <- as.data.frame((covid19pt$obitos_0_9_f + covid19pt$obitos_0_9_m) / (covid19pt$confirmados_0_9_f + covid19pt$confirmados_0_9_m)) 
let_dia_10_19 <- as.data.frame((covid19pt$obitos_10_19_f + covid19pt$obitos_10_19_m) / (covid19pt$confirmados_10_19_f + covid19pt$confirmados_10_19_m)) 
let_dia_20_29 <- as.data.frame((covid19pt$obitos_20_29_f + covid19pt$obitos_20_29_m) / (covid19pt$confirmados_20_29_f + covid19pt$confirmados_20_29_m)) 
let_dia_30_39 <- as.data.frame((covid19pt$obitos_30_39_f + covid19pt$obitos_30_39_m) / (covid19pt$confirmados_30_39_f + covid19pt$confirmados_30_39_m)) 
let_dia_40_49 <- as.data.frame((covid19pt$obitos_40_49_f + covid19pt$obitos_40_49_m) / (covid19pt$confirmados_40_49_f + covid19pt$confirmados_40_49_m)) 
let_dia_50_59 <- as.data.frame((covid19pt$obitos_50_59_f + covid19pt$obitos_50_59_m) / (covid19pt$confirmados_50_59_f + covid19pt$confirmados_50_59_m)) 
let_dia_60_69 <- as.data.frame((covid19pt$obitos_60_69_f + covid19pt$obitos_60_69_m) / (covid19pt$confirmados_60_69_f + covid19pt$confirmados_60_69_m)) 
let_dia_70_79 <- as.data.frame((covid19pt$obitos_70_79_f + covid19pt$obitos_70_79_m) / (covid19pt$confirmados_70_79_f + covid19pt$confirmados_70_79_m)) 
let_dia_80plus <- as.data.frame((covid19pt$obitos_80_plus_f + covid19pt$obitos_80_plus_m) / (covid19pt$confirmados_80_plus_f + covid19pt$confirmados_80_plus_m)) 

evol_let <- cbind(covid19pt$data , let_dia_0_9, let_dia_10_19, let_dia_20_29, let_dia_30_39, let_dia_40_49, let_dia_50_59, let_dia_60_69, let_dia_70_79, let_dia_80plus)
colnames(evol_let)[1:10] <- c("Data", "0 a 9 anos", "10 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60 a 69 anos", "70 a 79 anos", "Mais de 80 anos")
evol_let_etaria <- melt(evol_let, id.vars = "Data")

ggplot(evol_let_etaria, aes(x = Data, y = value * 100, col = variable)) + 
  geom_line() + 
  xlab("Data") + 
  ylab("Taxa de letalidade (%)") + 
  labs(title = "Taxa de Letalidade por grupo et?rio ao longo do tempo") + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  theme(plot.title = element_text(size = 15, face = "bold", family = "serif")) + 
  labs(colour = "Grupo etário")

#Evolucao Temporal dos sintomas

sintomas_tempo <- as.data.frame((covid19pt[8:173, 41:46]*covid19pt$confirmados[8:173]) - covid19pt[7:172, 41:46]*covid19pt$confirmados[7:172]) / covid19pt$confirmados_novos[8:173]
sintomas_tempo <- rbind(covid19pt[7,41:46], sintomas_tempo)
sintomas_tempo <- cbind(covid19pt$data[7:173], sintomas_tempo)

sintomas_tempo_melt <- melt(sintomas_tempo, id.vars="Data")
names(sintomas_tempo_melt)[2:3] <- c("Sintomas", "Valores")

## Fazer um grafico de linhas

ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas, group=1)) +
  geom_line() +
  scale_color_discrete(labels = c("Tosse", "Febre", "Dificuldade Respiratoria", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")) +
  xlab("Sintomas") +
  ylab("Frequencia (%)") +
  labs(title="Evolucao de sintomas ao longo do tempo")

# Taxa de letalidade com a evolucao temporal

letalidade_evolucao <- as.data.frame((covid19pt$obitos / covid19pt$confirmados))
letalidade_dia <- cbind(covid19pt$data, letalidade_evolucao)
colnames(letalidade_dia) [1:2] <- c("Data", "Letalidade")
ggplot(letalidade_dia, aes(x = Data, y = Letalidade *100, group =1)) + 
  geom_line(color = "olivedrab", size =1) +
  xlab("Data") + 
  ylab("Taxa de Letalidade (%)") + 
  labs(title = "Taxa de Letalidade ao longo do tempo") + 
  scale_x_date(date_breaks = "months", date_labels = "%b")


# REGIOES comparar regioes relativamente a:
##Incidencia (nº de casos)
populacao_acores <- 242796
populacao_madeira <- 254254
populacao_alentejo <- 705018
populacao_algarve <- 438635
populacao_centro <- 2216927 
populacao_lvt <- 2854802
populacao_norte <- 3573961


arsnorte <- max(covid19pt$confirmados_arscentro) / populacao_norte
arscentro <- max(covid19pt$confirmados_arscentro) / populacao_centro
arslvt <- max(covid19pt$confirmados_arslvt) / populacao_lvt
arsalentejo <- max(covid19pt$confirmados_arsalentejo) / populacao_alentejo
arsalgarve <- max(covid19pt$confirmados_arsalgarve) / populacao_algarve
acores <- max(covid19pt$confirmados_acores) / populacao_acores
madeira <- max(covid19pt$confirmados_madeira) / populacao_madeira

##Evolução temporal da incidência por região

incid_arsnorte <- as.data.frame((covid19pt$confirmados_arsnorte - covid19pt$obitos_arsnorte) / (populacao_norte - covid19pt$obitos_arsnorte))
incid_arscentro <- as.data.frame((covid19pt$confirmados_arscentro - covid19pt$obitos_arscentro) / (populacao_centro - covid19pt$obitos_arscentro))
incid_arslvt <- as.data.frame((covid19pt$confirmados_arslvt - covid19pt$obitos_arslvt) / (populacao_lvt - covid19pt$obitos_arslvt))
incid_arsalentejo <- as.data.frame((covid19pt$confirmados_arsalentejo - covid19pt$obitos_arsalentejo) / (populacao_alentejo - covid19pt$obitos_arsalentejo))
incid_arsalgarve <- as.data.frame((covid19pt$confirmados_arsalgarve - covid19pt$obitos_arsalgarve) / (populacao_algarve - covid19pt$obitos_arsalgarve))
incid_acores <- as.data.frame((covid19pt$confirmados_acores - covid19pt$obitos_acores) / (populacao_acores - covid19pt$obitos_acores))
incid_madeira <- as.data.frame((covid19pt$confirmados_madeira - covid19pt$obitos_madeira) / (populacao_madeira - covid19pt$obitos_madeira))

incid_diaria <- cbind(covid19pt$data, incid_arsnorte, incid_arscentro, incid_arslvt, incid_arsalentejo, incid_arsalgarve, incid_acores, incid_madeira)
colnames(incid_diaria)[1:8] <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "A?ores", "Madeira")

incid_diaria_melt <- melt(incid_diaria, id.vars = "Data")

ggplot(incid_diaria_melt, aes(x = Data, y = value*100, col = variable)) + 
  geom_line() + 
  xlab("Data") + 
  ylab("Incidência (%)") + 
  labs(title = "Incidência regional ao longo do tempo", colour = "Região") + 
  scale_x_date(date_breaks = "months", date_labels = "%b")
  

##Mortalidade (nº de mortos ou taxa de mortalidade)
Mortalidade_arsnorte <- max(covid19pt$confirmados_arsnorte, na.rm = TRUE) / populacao_total
Mortalidade_arscentro <- max(covid19pt$confirmados_arscentro, na.rm = TRUE) / populacao_total
Mortalidade_arslvt <- max(covid19pt$confirmados_arslvt, na.rm = TRUE) / populacao_total
Mortalidade_arsalentejo <- max(covid19pt$confirmados_arsalentejo, na.rm = TRUE) / populacao_total
Mortalidade_arsalgarve <- max(covid19pt$confirmados_arsalgarve, na.rm = TRUE) / populacao_total
Mortalidade_acores <- max(covid19pt$confirmados_acores, na.rm = TRUE) / populacao_total
Mortalidade_madeira <- max(covid19pt$confirmados_madeira, na.rm = TRUE) / populacao_total

##Letalidade (mortos/casos) ... Eventualmente com a evolucao temporal
Letalidade_arsnorte <- (max(covid19pt$obitos_arsnorte, na.rm = TRUE) / max(covid19pt$confirmados_arsnorte, na.rm = TRUE))
Letalidade_arscentro <- (max(covid19pt$obitos_arscentro, na.rm = TRUE) / max(covid19pt$confirmados_arscentro, na.rm = TRUE))
Letalidade_arslvt <- (max(covid19pt$obitos_arslvt, na.rm = TRUE) / max(covid19pt$confirmados_arslvt, na.rm = TRUE))
Letalidade_arsalentejo <- (max(covid19pt$obitos_arsalentejo, na.rm = TRUE) / max(covid19pt$confirmados_arsalentejo, na.rm = TRUE))
Letalidade_arsalgarve <- (max(covid19pt$obitos_arsalgarve, na.rm = TRUE) / max(covid19pt$confirmados_arsalgarve, na.rm = TRUE))
Letalidade_acores <- (max(covid19pt$obitos_acores, na.rm = TRUE) / max(covid19pt$confirmados_acores, na.rm = TRUE))
Letalidade_madeira <- (max(covid19pt$obitos_madeira, na.rm = TRUE) / max(covid19pt$confirmados_madeira, na.rm = TRUE))


#Recuperados
recuperados <- last(covid19pt$recuperados) / last(covid19pt$confirmados)

##Evoluçao temporal da taxa de recuperação
recuperados_evolucao <- as.data.frame(covid19pt$recuperados/covid19pt$confirmados)
recuperados_evolucao <- cbind(covid19pt$data, recuperados_evolucao)
colnames(recuperados_evolucao)[1:2] <- c("Data", "Recuperados")

ggplot(recuperados_evolucao, aes(x=Data, y=Recuperados*100, grupo = 1)) +
  geom_line(color = "orchid4", size =1) + 
  xlab("Data") +
  ylab("Taxa de Recuperação (%)") +
  labs(title = "Taxa de Recuperação ao longo do tempo")+
  scale_x_date(date_breaks = "months", date_labels = "%b")


#Internados/Internados UCI (nº absoluto e por nº de casos)
## taxa internados
internados <- as.data.frame(covid19pt$internados[9:207] / ((covid19pt$confirmados[9:207]) - (covid19pt$recuperados[9:207]) - (covid19pt$obitos[9:207])))
internados <- cbind(internados, covid19pt$data[9:207]) 
colnames(internados)[1] <- "Internamento"
colnames(internados)[2] <- "Data"

## taxa internados UCI
uci <- as.data.frame(covid19pt$internados_uci[18:207]/ ((covid19pt$confirmados[18:207]) - (covid19pt$recuperados[18:207]) - (covid19pt$obitos[18:207])))
uci <- cbind(uci, covid19pt$data[18:207])
colnames(uci) [1] <- "Internamentos_UCI"
colnames(uci) [2] <- "Data"

##Evolucao temporal de internados

ggplot(internados, aes(x = Data, y = Internamento*100, group =1 )) + 
  geom_line(color = "cornflowerblue", size = 1) +
  xlab("Data") + 
  ylab("Taxa de Internamento (%)") + 
  labs(title = "Evolução da taxa de internamento ao longo do tempo") 

##Evolução temporal de internados na UCI

ggplot(uci, aes(x = Data, y = Internamentos_UCI *100, grupo =1 ))+ 
  geom_line(color = "tomato", size = 1) +
  xlab("Data") + 
  ylab("Taxa de Internamento na UCI (%)") +
  labs(title = "Evolução da taxa de internamento na UCI ao longo do tempo")

#MAPAS
mapa_portugal <- rgdal::readOGR("C:/Users/teres/Desktop/Estágio/covid19pt-data-master/extra/mapas/portugal_continental.geojson")
