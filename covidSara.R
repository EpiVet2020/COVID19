#Libraries
library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(grid)
library(tibble)

#for maps
library(dplyr)
require(devtools)
require(RCurl)
install_github("pinanunes/Rempres")
library(Rempres)
require(leaflet)


setwd("~/Desktop/Treino Estágio 2020-2021")

#Data
covid19pt<-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

#Transformar para formato de data
covid19pt$data<-as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")

# SINTOMAS
##frequência de sintomas por casos confirmados (últimos dados recolhidos)
## fazer uma tabela e fazer um gráfico de barras

sintomas_geral2 <- as.data.frame(t(covid19pt[173,41:46]))

sintomas_geral2 <- sintomas_geral2 %>%
  rownames_to_column(var="Sintomas")
  names(sintomas_geral2)[2] <- "Frequência"

ggplot(data=sintomas_geral2, aes(x = Sintomas, y = Frequência*100)) +
  geom_col(colour= "black", fill="#DD8888", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade\nrespiratória", "Dores musculares", "Febre", "Fraqueza generalizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequência (%)", title = "Frequência de Sintomas da COVID-19",x="Sintomas") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequência, digits=4)), vjust=-0.5)
  
## Avaliar se as frequências relativas se foram alterando ao longo do tempo
### Equação para saber a %sintomas diário ( = nº casos tosse do dia / nº casos confirmados diários/novos)


##((sintomas do dia x nº confirmados até ao dia) - (sintomas dia anterior*confirmados dia anterior)) / confirmados novos
sintomas_tempo <- as.data.frame((covid19pt[8:173, 41:46]*covid19pt$confirmados[8:173] - covid19pt[7:172, 41:46]*covid19pt$confirmados[7:172]) / covid19pt$confirmados_novos[8:173])

### Adicionar a linha 7 - 1ª linha (valor de 6 era NA e não dava para ser subtraído antes) 

sintomas_tempo2 <- rbind(covid19pt[7,41:46], sintomas_tempo)

### Combinar com as datas
sintomas_tempo3 <- cbind(covid19pt$data[7:173], sintomas_tempo2)

sintomas_tempo3[3,4] <- 0.11  #1ª linha da dificuldade respiratória à qual tinha sido subtraído NA.

names(sintomas_tempo3) <- c("Data", "Tosse", "Febre", "Dificuldades Respiratórias", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

sintomas_tempo_melt <- melt(sintomas_tempo3, id.vars="Data")
names(sintomas_tempo_melt)[2:3] <- c("Sintomas", "Valores")

## Fazer um gráfico de linhas
ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas, group=1)) +
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  facet_grid(Sintomas ~ ., scales = "free") +
  scale_color_discrete(labels = c("Tosse", "Febre", "Dificuldade Respiratória", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")) +
  xlab("Sintomas") +
  ylab("Frequência (%)") +
  labs(title="Evolução de sintomas ao longo do tempo")

#GÉNERO
## Casos e Mortalidade entre homens e mulheres
mortes_total <- last(covid19pt$obitos)
mortes_mulheres <- last(covid19pt$obitos_f)
mortes_homens <- last(covid19pt$obitos_m)

table_mortes <-cbind(mortes_total, mortes_homens, mortes_mulheres)

numero_mortes <- as.data.frame(t(table_mortes))

numero_mortes <- numero_mortes %>% 
  rownames_to_column(var="Género")
  names(numero_mortes)[2] <- "N_mortes"

ggplot(numero_mortes, aes(x=Género, y=N_mortes)) + 
  geom_col(fill="salmon", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Nº de mortes", title = "Número de mortes por COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = N_mortes), vjust=-0.5)

##Adicionar duas novas colunas de óbitos diários fem e masc (covid19pt3 fica com +2 colunas)
covid19pt2 <- covid19pt %>%
  mutate(obitosdiariosf = covid19pt$obitos_f - lag(covid19pt$obitos_f))

covid19pt3 <- covid19pt2 %>%
  mutate(obitosdiariosm = covid19pt2$obitos_m - lag(covid19pt2$obitos_m))

##Média de óbitos diários fem e masc
mean(covid19pt3$obitosdiariosf, na.rm = TRUE)
mean(covid19pt3$obitosdiariosm, na.rm = TRUE)

##Adicionar duas novas colunas de casos novos diários fem e masc (covid19pt5 com +4 colunas que o original)
covid19pt4 <- covid19pt3 %>%
  mutate(casosdiariosf = covid19pt3$confirmados_f - lag(covid19pt3$confirmados_f))

covid19pt5 <- covid19pt4 %>%
  mutate(casosdiariosm = covid19pt4$confirmados_m - lag(covid19pt4$confirmados_m))

##Média de casos diários fem e masc
mean(covid19pt5$casosdiariosf, na.rm = TRUE)
mean(covid19pt5$casosdiariosm, na.rm = TRUE)

## Calcular a TAXA DE MORTALIDADE covid por género
## Tabela e Gráfico de barras

    ## Dados INE - 10 295 909 total população ; 4 859 977 homens ; 5 435 932 mulheres
população_total <- 10295909
população_f <- 5435932
população_m <- 4859977

Mortalidade <- last(covid19pt$obitos) / população_total
Mortalidadef <- last(covid19pt$obitos_f) / população_f
Mortalidadem <- last(covid19pt$obitos_m) / população_m

tm_t <- cbind(Mortalidadem, Mortalidadef, Mortalidade)

tm <- as.data.frame(t(tm_t))
tm <- tm %>% 
  rownames_to_column(var="Género")
  names(tm)[2] <- "Taxa_Mortalidade"

ggplot(tm, aes(x=Género, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Total", "Mulheres", "Homens")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


## Calcular TAXA DE LETALIDADE covid por género
# Tabela e Gráfico de Barras
Letalidadetotal <- last(covid19pt$obitos / covid19pt$confirmados)
Letalidadef <- last(covid19pt$obitos_f / covid19pt$confirmados_f)
Letalidadem <- last(covid19pt$obitos_m / covid19pt$confirmados_m)           

tl_t <- cbind(Letalidadetotal, Letalidadem, Letalidadef)

tl <- as.data.frame(t(tl_t))
tl <- tl %>%
  rownames_to_column(var= "Género")
  names(tl)[2] <- "Taxa_Letalidade"

ggplot(tl, aes(x=Género, y=Taxa_Letalidade*100)) + 
  geom_col(fill="grey", width = 0.5) +
  scale_x_discrete(labels= c("Mulheres", "Homens", "Total")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade - COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)
          
   ## Letalidade feminina menor justificada por um menor nº de óbitos (941 vs 958) e maior número de confirmados/suscetíveis (37290 vs 30735) 

##Fica para uma próxima - Boxplot (Não fica fixe!)
boxplot(Mortalidadef, Mortalidadem, Letalidadef, Letalidadem, 
        main = "Mortalidade e Letalidade por género",
        names = c("Mortalidade Feminina", "Mortalidade Masculina", "Letalidade Feminina", "Letalidade Masculina"),
        las = 2,
        col = c("pink", "blue"),
        border = "brown",
        vertical = TRUE,
        notch = TRUE
)

## Taxa de incidência por género (diária)
incidenciacumulativatotal <- as.data.frame((covid19pt$confirmados_novos / (população_total - covid19pt$confirmados - covid19pt$obitos)), na.rm = TRUE)
incidenciacumulativaf <- as.data.frame(((covid19pt$confirmados_f - lag(covid19pt$confirmados_f)) / (população_f - covid19pt$confirmados_f - covid19pt$obitos_f)), na.rm = TRUE)
incidenciacumulativam <- as.data.frame(((covid19pt$confirmados_m - lag(covid19pt$confirmados_m)) / (população_m - covid19pt$confirmados_m - covid19pt$obitos_m)), na.rm = TRUE)

#remover valores negativos
incidenciacumulativaf[174:175, ] <- NA
incidenciacumulativam[174:175, ] <- NA

incidencia <- cbind(covid19pt$data, incidenciacumulativatotal, incidenciacumulativaf, incidenciacumulativam)
names(incidencia) <- c("Data", "Total", "Mulheres", "Homens")

incidencia_diaria_melt <- melt(incidencia, id.vars="Data")

names(incidencia_diaria_melt)[2:3] <- c("Incidência", "Valores")

ggplot(incidencia_diaria_melt, aes(x = Data, y = Valores*100, color = Incidência)) +
  geom_line() +
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  facet_grid(Incidência ~ ., scales = "free") +
  scale_color_discrete(labels = c("Total", "Homens", "Mulheres")) +
  xlab("Incidência") +
  ylab("Frequência (%)") +
  labs(title="Incidência cumulativa de COVID19")
  ### Está a aumentar novamente....

#GRUPO ETÁRIO 
## Casos por grupo etário
casos0_9 <- sum(last(covid19pt$confirmados_0_9_f), last(covid19pt$confirmados_0_9_m))
casos10_19 <- sum(last(covid19pt$confirmados_10_19_f), last(covid19pt$confirmados_10_19_m))
casos20_29 <- sum(last(covid19pt$confirmados_20_29_f), last(covid19pt$confirmados_20_29_m))
casos30_39 <- sum(last(covid19pt$confirmados_30_39_f), last(covid19pt$confirmados_30_39_m))
casos40_49 <- sum(last(covid19pt$confirmados_40_49_f), last(covid19pt$confirmados_40_49_m))
casos50_59 <- sum(last(covid19pt$confirmados_50_59_f), last(covid19pt$confirmados_50_59_m))
casos60_69 <- sum(last(covid19pt$confirmados_60_69_f), last(covid19pt$confirmados_60_69_m))
casos70_79 <- sum(last(covid19pt$confirmados_70_79_f), last(covid19pt$confirmados_70_79_m))
casos80plus <- sum(last(covid19pt$confirmados_80_plus_f), last(covid19pt$confirmados_80_plus_m))

casos_t <- cbind(casos0_9, casos10_19, casos20_29, casos30_39, casos40_49, casos50_59, casos60_69, casos70_79, casos80plus)
casos_grupo_etário <- as.data.frame(t(casos_t))

casos_grupo_etário <- casos_grupo_etário %>%
  rownames_to_column(var= "Grupo_Etário")
  names(casos_grupo_etário)[2] = "N_casos"
  
ggplot(casos_grupo_etário, aes(x=Grupo_Etário, y=N_casos)) +
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80plus")) +
  theme_classic() +
  labs(y="Nº de casos", title = "Nº de casos de COVID-19 por Grupo Etário", x="Grupo Etário") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

## Mortes por grupo etário
obitos0_9 <- sum(last(covid19pt$obitos_0_9_f), last(covid19pt$obitos_0_9_m))
obitos10_19 <- sum(last(covid19pt$obitos_10_19_f), last(covid19pt$obitos_10_19_m))
obitos20_29 <- sum(last(covid19pt$obitos_20_29_f), last(covid19pt$obitos_20_29_m))
obitos30_39 <- sum(last(covid19pt$obitos_30_39_f), last(covid19pt$obitos_30_39_m))
obitos40_49 <- sum(last(covid19pt$obitos_40_49_f), last(covid19pt$obitos_40_49_m))
obitos50_59 <- sum(last(covid19pt$obitos_50_59_f), last(covid19pt$obitos_50_59_m))
obitos60_69 <- sum(last(covid19pt$obitos_60_69_f), last(covid19pt$obitos_60_69_m))
obitos70_79 <- sum(last(covid19pt$obitos_70_79_f), last(covid19pt$obitos_70_79_m))
obitos80plus <- sum(last(covid19pt$obitos_80_plus_f), last(covid19pt$obitos_80_plus_m))

obitos_t <- cbind(obitos0_9, obitos10_19, obitos20_29, obitos30_39, obitos40_49, obitos50_59, obitos60_69, obitos70_79, obitos80plus)
obitos_grupo_etário <- as.data.frame(t(obitos_t))

obitos_grupo_etário <- obitos_grupo_etário %>%
  rownames_to_column(var= "Grupo_Etário")
  names(obitos_grupo_etário)[2] = "N_obitos"

ggplot(obitos_grupo_etário, aes(x=Grupo_Etário, y=N_obitos)) +
  geom_col(fill="steelblue", width = 0.5) +
  scale_x_discrete(labels= c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80plus")) +
  theme_classic() +
  labs(y="Nº de obitos", title = "Nº de óbitos de COVID-19 por Grupo Etário", x="Grupo Etário") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

## Taxa de mortalidade por grupo etário
população_0_9 <- 894631
população_10_19 <- 1056679
população_20_29 <- 1092080
população_30_39 <- 1250448
população_40_49 <- 1575225
população_50_59 <- 1482121
população_60_69 <- 1293301
população_70_79 <- 973123
população_80plus <- 668660

mort0_9 <- obitos0_9 / população_0_9
mort10_19 <- obitos10_19 / população_10_19 
mort20_29 <- obitos20_29 / população_20_29
mort30_39 <- obitos30_39 / população_30_39
mort40_49 <- obitos40_49 / população_40_49 
mort50_59 <- obitos50_59 / população_50_59 
mort60_69 <- obitos60_69 / população_60_69 
mort70_79 <- obitos70_79 / população_70_79
mort80plus <- obitos80plus / população_80plus

## Taxa de letalidade por grupo etário
let0_9 <- obitos0_9 / casos0_9 
let10_19 <- obitos10_19 / casos10_19 
let20_29 <- obitos20_29 / casos20_29
let30_39 <- obitos30_39 / casos30_39 
let40_49 <- obitos40_49 / casos40_49 
let50_59 <- obitos50_59 / casos50_59 
let60_69 <- obitos60_69 / casos60_69 
let70_79 <- obitos70_79 / casos70_79 
let80plus <- obitos80plus / casos80plus

## Tabela e Gráfico

mort_etaria <- c(mort0_9, mort10_19, mort20_29, mort30_39, mort40_49, mort50_59, mort60_69, mort70_79, mort80plus)

let_etaria <- c(let0_9, let10_19, let20_29, let30_39, let40_49, let50_59, let60_69, let70_79, let80plus)

mortalidade_etaria <- data.frame(Grupos = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "mais de 80"), Mortalidade = mort_etaria)
ggplot(data=mortalidade_etaria, aes(x=Grupos, y=Mortalidade)) +
  geom_bar(stat="identity", color = "seagreen2", fill = "seagreen2") +
  geom_text(aes(label = scales::percent(Mortalidade, digits = 2)), vjust = -0.5) +
  labs(title="Taxa de Mortalidade por Grupo Etário")

letalidade_etaria <- data.frame (Grupos = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "mais de 80"), Letalidade = let_etaria)
ggplot(data=letalidade_etaria, aes(x=Grupos, y=Letalidade)) +
  geom_bar(stat = "identity", color = "turquoise4", fill = "turquoise4") + 
  geom_text(aes(label = scales::percent(Letalidade, digits = 2)), vjust = -0.5) +
  labs(title = "Taxa de Letalidade por Grupo Etário")

##Gráfico de duas barras (Letalidade e Mortalidade Etária)

grupoetário <- as.data.frame(c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "mais de 80"))
grupoetário_mortlet <- as.data.frame(cbind(grupoetário, mort_etaria, let_etaria))
names(grupoetário_mortlet) <- c("Grupo Etário", "Mortalidade", "Letalidade")

grupoetário <- c(rep("0 a 9" , 2) , rep("10 a 19" , 2) , rep("20 a 29" , 2) , rep("30 a 39" , 2) , rep("40 a 49" , 2) , rep("50 a 59" , 2) , rep("60 a 69" , 2), rep("70 a 79" , 2) , rep("80 ou mais" , 2) )
condition <- rep(c("Mortalidade" , "Letalidade"), 3)
value <- c(mort_etaria, let_etaria)
graphicdata <- data.frame(grupoetário,condition,value)

ggplot(graphicdata, aes(fill=condition, y=value, x=grupoetário)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) + 
  labs(y="Frequência (%)", title = "Taxa de Mortalidade e Letalidade por grupo etário", x="Grupo Etário", fill= "Taxas") +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10)) +
  geom_text(aes(label = scales::percent(value, digits=5)), vjust=-0.5)
  
#Evolução Temporal da letalidade por grupo etário
letalidade_tempo <- as.data.frame(covid19pt[])

obitos_idade_f <- select(covid19pt, starts_with("obitos_", ends_with("_f")))
obitos_idade_m <- select(covid19pt, starts_with("obitos_"), ends_with("_m"))

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
names(evol_let_etaria)[2:3] <- c("Grupo_Etário", "Letalidade")

ggplot(evol_let_etaria, aes(x = Data, y = Letalidade*100, col = Grupo_Etário)) + 
  geom_line() + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  xlab("Data") + 
  ylab("Taxa de letalidade (%)") + 
  labs(title = "Taxa de Letalidade por Grupo Etário ao longo do tempo") + 
  theme(plot.title = element_text(size = 15, face = "bold", family = "serif")) + 
  labs(colour = "Grupo Etário")

# REGIÕES comparar regiões relativamente a:
##Nº de casos e taxa de incidência

casos_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[,4:10], last))))

casos_região <- casos_região %>% 
  rownames_to_column(var="Regiões")
  names(casos_região)[2] <- "N_casos"

ggplot(casos_região, aes(x=Regiões, y=N_casos)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de casos", title = "Nº de casos de COVID-19 por regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N_casos), vjust=-0.5)

##Taxa de incidência ao longo do tempo
população_acores <- 242796
população_madeira <- 254254
população_alentejo <- 705018
população_algarve <- 438635
população_centro <- 2216927 
população_lvt <- 2854802
população_norte <- 3573961

pop_juntas <- as.data.frame(c(população_norte, população_centro, população_lvt, população_alentejo, população_algarve, população_acores , população_madeira))
pop_rep <- as.data.frame(t(pop_juntas[rep(seq_len(ncol(pop_juntas)), each=nrow(covid19pt))]))

incidencia_região <- (covid19pt[, 4:10] - lag(covid19pt[, 4:10])) / (pop_rep - covid19pt[, 4:10] - covid19pt[, 49:55])

incidencia_região_tempo <- as.data.frame(cbind(covid19pt$data, incidencia_região))
names(incidencia_região_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

incidencia_região_melt <- melt(incidencia_região_tempo, id.vars="Data")
names(incidencia_região_melt)[2:3] <- c("Regiões", "Incidência")

ggplot(incidencia_região_melt, aes(x = Data, y =Incidência*100, color = Regiões)) +
  geom_line(color = "olivedrab", size =1) +
  facet_grid(Regiões ~ ., scales = "free") +
  scale_color_discrete(labels = c("Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira")) +
  xlab("Data") +
  ylab("Taxa de Incidência") +
  labs(title="Taxa de Incidência de COVID19 por Região") +
  scale_x_date(date_breaks = "months", date_labels = "%b")

##Mortalidade (nº de mortos) por região

mortes_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last))))

mortes_região <- mortes_região %>% 
  rownames_to_column(var="Regiões")
names(mortes_região)[2] <- "N_mortes"

ggplot(mortes_região, aes(x=Regiões, y=N_mortes)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de óbitos", title = "Óbitos de COVID-19 por regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=N_mortes), vjust=-0.5)

##Letalidade - evolução temporal
  
letalidade_t <- as.data.frame(covid19pt[,49:55] / covid19pt[,4:10])
letalidade_t2 <- cbind (covid19pt$data, letalidade_t)
names(letalidade_t2) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

letalidade_tempo_melt <- melt(letalidade_t2, id.vars="Data")
names(letalidade_tempo_melt)[2:3] <- c("Regiões", "Letalidade")

ggplot(letalidade_tempo_melt, aes(x = Data, y =Letalidade, color = Regiões)) +
  geom_line(color = "salmon", size =1) +
  facet_grid(Regiões ~ ., scales = "free") +
  scale_color_discrete(labels = c("Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira")) +
  xlab("Data") +
  ylab("Taxa de Letalidade") +
  labs(title="Taxa de Letalidade de COVID19 por Região") +
  scale_x_date(date_breaks = "months", date_labels = "%b")


#RECUPERADOS
recuperados <- last(covid19pt$recuperados) / last(covid19pt$confirmados)

##Evolução temporal da taxa de recuperação
recuperados_evolucao <- as.data.frame(covid19pt$recuperados/covid19pt$confirmados)
recuperacao_diaria <- cbind(covid19pt$data, recuperados_evolucao)
colnames(recuperacao_diaria)[1:2] <- c("Data", "Recuperados")

ggplot(recuperacao_diaria, aes(x=Data, y=Recuperados*100, grupo = 1)) +
  geom_line(color = "orchid4", size =1) + 
  xlab("Data") +
  ylab("Taxa de Recuperação (%)") +
  labs(title = "Taxa de Recuperação ao longo do tempo")+
  scale_x_date(date_breaks = "months", date_labels = "%b")


#INTERNADOS / INTERNADOS UCI (nº absoluto e por nº de casos) e média
## Taxa de internados
internados <- as.data.frame(covid19pt$internados[9:204] / ((covid19pt$confirmados[9:204]) - (covid19pt$recuperados[9:204]) - (covid19pt$obitos[9:204])))
internados_dia <- cbind(internados, covid19pt$data[9:204]) 
colnames(internados_dia)[1] <- "Internamento"
colnames(internados_dia)[2] <- "Data"

INTERNADOS <- mean(internados_dia[1:196, 1])

## Taxa de internados na UCI e média
uci <- as.data.frame(covid19pt$internados_uci[18:204]/ ((covid19pt$confirmados[18:204]) - (covid19pt$recuperados[18:204]) - (covid19pt$obitos[18:204])))
uci_dia <- cbind(covid19pt$data[18:204], uci)
colnames(uci_dia) [2] <- "Internamentos_UCI"
colnames(uci_dia) [1] <- "Data"

UCI <- mean(uci[1:187, 1])

##Gráfico da evolução temporal de internados
ggplot(internados_dia, aes(x = Data, y = Internamento*100, group =1 )) + 
  geom_line(color = "cornflowerblue", size = 1) +
  xlab("Data") +
  ylab("Taxa de Internamento (%)") + 
  labs(title = "Evolução da taxa de internamento ao longo do tempo")

##Gráfico da evolução temporal de internados na UCI
ggplot(uci_dia, aes(x = Data, y = Internamentos_UCI *100, grupo =1 ))+ 
  geom_line(color = "tomato", size = 1) +
  xlab("Data") + 
  ylab("Taxa de Internamento na UCI (%)") +
  labs(title = "Evolução da taxa de internamento na UCI ao longo do tempo")

#MAPA (confirmados$mês$região)

library(leaflet)
library(maptools)
library(jsonlite)
library(geojson)

jsondata <- rgdal::readOGR("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson")

class(jsondata)
names(jsondata)
mymap <- st_read
covid19pt<-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

leaflet(data = jsondata) %>% 
  setView(lng = -6, lat = 40, zoom = 6)%>%
  addProviderTiles("Esri")

incidencia_região_tempo <- as.data.frame(cbind(covid19pt$data, incidencia_região))
names(incidencia_região_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira")

focos <- melt(incidencia_região_tempo, id.vars="Data")
names(focos)[2:3] <- c("Regiões", "Incidência")



# Taxa de Letalidade ajustada (grupo etário)
