
library(data.table)
library(dplyr)
library(tibble)
library(ggplot2)
library(grid)
library(rgdal)
library(httpuv)
library(xtable)
library(sourcetools)
library(fastmap)
library(rappdirs)
library(shiny)
library(renv)
library(learnr)

covid.pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
covid.pt$data <- as.Date(as.character(covid.pt$data_dados), format="%d-%m-%Y")
# FREQUENCIA DE SINAIS CLINICOS
## Calcular a media das frequencias relativas dos sinais clinicos

sintomas <- as.data.frame(t(covid.pt[173, 41:46]))

sintomas <- sintomas %>%
  rownames_to_column(var="Sintomas")
names(sintomas)[2] <- "Frequencia"

##fazer a tabela e o grafico de barras dos sinais clinicos

ggplot(sintomas, aes(x=Sintomas, y=Frequencia*100)) +
  geom_col(fill="darksalmon", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade_respiratoria", "Dores\nmusculares", "Febre", "Fraqueza_generalizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequencia (%)", title = "Frequencia de sintomas da COVID-19",x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequencia, digits=4)), vjust=-0.5)

## Avaliar se as frequencias relativas dos sinais clinicos se foram alterando ao longo do tempo e fazer um grafico de linhas

sintomas_tempo <- as.data.frame((covid.pt[8:173,41:46]*covid.pt$confirmados[8:173])-(covid.pt[7:172,41:46]*covid.pt$confirmados[7:172]))/covid.pt$confirmados_novos[8:173]

sintomas_tempo_2 <- rbind(covid.pt[7,41:46], sintomas_tempo)

sintomas_tempo_3 <- cbind(covid.pt$data[7:173], sintomas_tempo_2)
names(sintomas_tempo_3) <- c("Data", "Tosse", "Febre", "Dificuldade respiratoria", "Cefaleia", "Dores musculares", "Fraqueza generalizada")

sintomas_tempo_3[3,4] <- 0.11

sintomas_tempo_melt <- melt(sintomas_tempo_3, id.vars="Data")
names(sintomas_tempo_melt)[-1] <- c("Sintomas", "Valores")

###VER!!!
ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas, group=1)) +
  geom_line() +
  scale_color_discrete(labels = c("Tosse", "Febre", "Dificuldade Respiratoria", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")) +
  xlab("M?s") +
  ylab("Frequ?ncia (%)") +
  labs(title="Incid?ncia Di?ria por G?nero")


# Casos e Mortalidade entre homens e mulheres
# N?mero de mortes

mortes_total <- last(covid.pt$obitos)
mortes_mulheres <- last(covid.pt$obitos_f)
mortes_homens <- last(covid.pt$obitos_m)

#Tabela e gr?fico de barras de n?mero de mortes
numero_mortes_nt <- cbind(mortes_total, mortes_mulheres, mortes_homens)
numero_mortes <- as.data.frame(t(numero_mortes_nt))
numero_mortes <- numero_mortes %>%
  rownames_to_column(var="Genero")
names(numero_mortes)[2] <- "N_mortes"

ggplot(numero_mortes, aes(x=Genero, y=N_mortes)) +
  geom_col(fill="steelblue4", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="N? de mortes", title = "N?mero de mortes por COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = N_mortes), vjust=-0.5)

# Calcular a "taxa" de mortalidade

pop_total <- 10295909
pop_homens <- 4859977
pop_mulheres <- 5435932

tm_total <- last(covid.pt$obitos)/pop_total
tm_mulheres <-last(covid.pt$obitos_f)/pop_mulheres
tm_homens <-last(covid.pt$obitos_m)/pop_homens

#Tabela e gr?fico da taxa de mortalidade

tm_nt<-cbind(tm_homens, tm_mulheres, tm_total)
tm <-as.data.frame(t(tm_nt))
tm <- tm %>%
  rownames_to_column(var="Genero")
names(tm)[2] <- "Taxa_Mortalidade"


ggplot(tm, aes(x=Genero, y=Taxa_Mortalidade*100)) +
  geom_col(fill="#5aa7b0", width = 0.4) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade da COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


# Calcular letalidade covid por g?nero

letalidade_total <- last(covid.pt$obitos)/last(covid.pt$confirmados)
letalidade_homens <-last(covid.pt$obitos_m)/last(covid.pt$confirmados_m)
letalidade_mulheres <- last(covid.pt$obitos_f)/last(covid.pt$confirmados_f)

# Uma tabela, e um gr?fico de barras

letalidade_nt <- cbind(letalidade_total, letalidade_homens, letalidade_mulheres)
letalidade<-as.data.frame(t(letalidade_nt))

letalidade <- letalidade %>%
  rownames_to_column(var="Genero")
names(letalidade)[2] <- "Taxa_Letalidade"

ggplot(letalidade, aes(x=Genero, y=Taxa_Letalidade*100))+
  geom_col(fill="green", width=0.5)+
  scale_x_discrete(labels=c("Homens", "Mulheres", "Total"))+
  theme_classic()+
  labs(y="Taxa de Letalidade (%)", title = "Taxa de Letalidade da COVID-19", x="")+
  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))+
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5, size=3)


# Taxa de incid?ncia di?ria por g?nero (Evolu??o temporal)
  # novos casos/popula??o em risco (pop total - confirmados - ?bitos)


  incidencia_total <- as.data.frame(covid.pt$confirmados_novos/(pop_total- covid.pt$confirmados-covid.pt$obitos))

  incidencia_homens <- as.data.frame((covid.pt$confirmados_m - lag(covid.pt$confirmados_m))/(pop_homens-covid.pt$confirmados_m-covid.pt$obitos_m))

  incidencia_mulheres <- as.data.frame((covid.pt$confirmados_f - lag(covid.pt$confirmados_f))/(pop_mulheres - covid.pt$confirmados_f-covid.pt$obitos_f))


  incidencia_nt <- cbind(covid.pt$data, incidencia_total, incidencia_homens, incidencia_mulheres)
  names(incidencia_nt) <- c("Data", "Total", "Homens", "Mulheres")
  incidencia_nt <-as.data.table(incidencia_nt)
 incidencia_melt <- melt(incidencia_nt, id.vars="Data")
 names(incidencia_melt)[-1] <- c("Genero", "Valores")


 ggplot(incidencia_melt, aes(x=Data, y=Valores*100, color=Genero)) +
   geom_line() +
   labs(title="Incidencia Diaria por Genero")+
   scale_x_date(date_labels = "%b", date_breaks = "1 month")

 #Taxa incid?ncia cumulativa por g?nero

 incidencia_total_cumulativa <- as.data.frame(covid.pt$confirmados/(pop_total- covid.pt$confirmados-covid.pt$obitos))

 incidencia_homens_cumulativa <- as.data.frame(covid.pt$confirmados_m/(pop_homens-covid.pt$confirmados_m-covid.pt$obitos_m))

 incidencia_mulheres_cumulativa <- as.data.frame(covid.pt$confirmados_f/(pop_mulheres - covid.pt$confirmados_f-covid.pt$obitos_f))


 incidencia_nt_cumulativa <- cbind(covid.pt$data, incidencia_total_cumulativa, incidencia_homens_cumulativa, incidencia_mulheres_cumulativa)
 names(incidencia_nt_cumulativa) <- c("Data", "Total", "Homens", "Mulheres")
 incidencia_nt_cumulativa <-as.data.table(incidencia_nt_cumulativa)
 incidencia_cumulativa<- melt(incidencia_nt_cumulativa, id.vars="Data")
 names(incidencia_cumulativa)[-1] <- c("Genero", "Valores")


 ggplot(incidencia_cumulativa, aes(x=Data, y=Valores*100, color=Genero)) +
   geom_line() +
   labs(title="Incidencia Cumulativa por Genero")+
   scale_x_date(date_labels = "%b", date_breaks = "1 month")


 # Fazer o mesmo que o anterior para o grupo etário
 #GRUPOS ET?RIOS:

 casos0_9 <- sum(max(covid.pt$confirmados_0_9_f, na.rm = TRUE), max(covid.pt$confirmados_0_9_m, na.rm = TRUE))
 casos10_19 <- sum(max(covid.pt$confirmados_10_19_f, na.rm = TRUE), max(covid.pt$confirmados_10_19_m, na.rm = TRUE))
 casos20_29 <- sum(max(covid.pt$confirmados_20_29_f, na.rm = TRUE), max(covid.pt$confirmados_20_29_m, na.rm = TRUE))
 casos30_39 <- sum(max(covid.pt$confirmados_30_39_f, na.rm = TRUE), max(covid.pt$confirmados_30_39_m, na.rm = TRUE))
 casos40_49 <- sum(max(covid.pt$confirmados_40_49_f, na.rm = TRUE), max(covid.pt$confirmados_40_49_m, na.rm = TRUE))
 casos50_59 <- sum(max(covid.pt$confirmados_50_59_f, na.rm = TRUE), max(covid.pt$confirmados_50_59_m, na.rm = TRUE))
 casos60_69 <- sum(max(covid.pt$confirmados_60_69_f, na.rm = TRUE), max(covid.pt$confirmados_60_69_m, na.rm = TRUE))
 casos70_79 <- sum(max(covid.pt$confirmados_70_79_f, na.rm = TRUE), max(covid.pt$confirmados_70_79_m, na.rm = TRUE))
 casos80plus <- sum(max(covid.pt$confirmados_80_plus_f, na.rm = TRUE), max(covid.pt$confirmados_80_plus_m, na.rm = TRUE))

#Mortes por grupo et?rio:

 obitos0_9 <- sum(max(covid.pt$obitos_0_9_f, na.rm = TRUE), max(covid.pt$obitos_0_9_m, na.rm = TRUE))
 obitos10_19 <- sum(max(covid.pt$obitos_10_19_f, na.rm = TRUE), max(covid.pt$obitos_10_19_m, na.rm = TRUE))
 obitos20_29 <- sum(max(covid.pt$obitos_20_29_f, na.rm = TRUE), max(covid.pt$obitos_20_29_m, na.rm = TRUE))
 obitos30_39 <- sum(max(covid.pt$obitos_30_39_f, na.rm = TRUE), max(covid.pt$obitos_30_39_m, na.rm = TRUE))
 obitos40_49 <- sum(max(covid.pt$obitos_40_49_f, na.rm = TRUE), max(covid.pt$obitos_40_49_m, na.rm = TRUE))
 obitos50_59 <- sum(max(covid.pt$obitos_50_59_f, na.rm = TRUE), max(covid.pt$obitos_50_59_m, na.rm = TRUE))
 obitos60_69 <- sum(max(covid.pt$obitos_60_69_f, na.rm = TRUE), max(covid.pt$obitos_60_69_m, na.rm = TRUE))
 obitos70_79 <- sum(max(covid.pt$obitos_70_79_f, na.rm = TRUE), max(covid.pt$obitos_70_79_m, na.rm = TRUE))
 obitos80plus <- sum(max(covid.pt$obitos_80_plus_f, na.rm = TRUE), max(covid.pt$obitos_80_plus_m, na.rm = TRUE))


 #Taxa de Mortalidade por grupo et?rio

 populacao_0_9 <- 894631
 populacao_10_19 <- 1056679
 populacao_20_29 <- 1092080
 populacao_30_39 <- 1250448
 populacao_40_49 <- 1575225
 populacao_50_59 <- 1482121
 populacao_60_69 <- 1293301
 populacao_70_79 <- 973123
 populacao_80plus <- 668660

 tm_0_9 <- obitos0_9/populacao_0_9
 tm_10_19 <- obitos10_19/populacao_10_19
 tm_20_29 <- obitos20_29/populacao_20_29
 tm_30_39 <- obitos30_39/populacao_30_39
 tm_40_49 <- obitos40_49/populacao_40_49
 tm_50_59 <- obitos50_59/populacao_50_59
 tm_60_69 <- obitos60_69/populacao_60_69
 tm_70_79 <- obitos70_79/populacao_70_79
 tm_80plus <- obitos80plus/populacao_80plus

 #Tabela taxa de mortalidade por grupo et?rio

 tm_etaria <- c(tm_0_9, tm_10_19, tm_20_29, tm_30_39, tm_40_49, tm_50_59, tm_60_69, tm_70_79, tm_80plus)
 names(tm_etaria) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80plus")
 tm_etaria <- as.data.frame((tm_etaria))

 tm_etaria <- tm_etaria %>%
   rownames_to_column(var="Grupo")
 names(tm_etaria)[2] <- "Taxa_Mortalidade"

#Gr?fico taxa de mortalidade por Grupo Et?rio

 ggplot(tm_etaria, aes(x=Grupo, y=Taxa_Mortalidade*100))+
   theme_classic()+
   geom_bar(stat = "identity", color="#048753", fill="#048753")+
   geom_text(aes(label=scales::percent(round(Taxa_Mortalidade, digits = 4))), vjust=-0.5, size=3)+
   labs(title = "Taxa de Mortalidade por Grupo Etario")


 #Taxa de Letalidade por Grupo Et?rio

letalidade0_9 <- obitos0_9/casos0_9
letalidade10_19 <- obitos10_19/casos10_19
letalidade20_29 <- obitos20_29/casos20_29
letalidade30_39 <- obitos30_39/casos30_39
letalidade40_49 <- obitos40_49/casos40_49
letalidade50_59 <- obitos50_59/casos50_59
letalidade60_69 <- obitos60_69/casos60_69
letalidade70_79 <- obitos70_79/casos70_79
letalidade80plus <- obitos80plus/casos80plus

#Tabela e gr?fico

letalidade_etaria <- c(letalidade0_9, letalidade10_19, letalidade20_29, letalidade30_39, letalidade40_49, letalidade50_59, letalidade60_69, letalidade70_79, letalidade80plus)
names(letalidade_etaria) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80plus")
letalidade_etaria <- as.data.frame((letalidade_etaria))

letalidade_etaria <- letalidade_etaria %>%
  rownames_to_column(var="Grupo")
names(letalidade_etaria)[2] <- "Taxa_Letalidade"


ggplot(letalidade_etaria, aes(x=Grupo, y=Taxa_Letalidade*100))+
  theme_classic()+
  geom_bar(stat = "identity", color="yellow", fill="orange")+
  geom_text(aes(label=scales::percent(round(Taxa_Letalidade, digits = 4))), vjust=-0.5, size=3)+
  labs(title = "Taxa de Letalidade por Grupo Etario")


#Evolu??o temporal da letalidade por grupo et?rio


tl0_9 <- as.data.frame((covid.pt$obitos_0_9_f+covid.pt$obitos_0_9_m)/(covid.pt$confirmados_0_9_f+covid.pt$confirmados_0_9_m))
tl10_19 <- as.data.frame((covid.pt$obitos_10_19_f+covid.pt$obitos_10_19_m)/(covid.pt$confirmados_10_19_f+covid.pt$confirmados_10_19_m))
tl20_29 <- as.data.frame((covid.pt$obitos_20_29_f+covid.pt$obitos_20_29_m)/(covid.pt$confirmados_20_29_f+covid.pt$confirmados_20_29_m))
tl30_39 <- as.data.frame((covid.pt$obitos_30_39_f+covid.pt$obitos_30_39_m)/(covid.pt$confirmados_30_39_f+covid.pt$confirmados_30_39_m))
tl40_49 <- as.data.frame((covid.pt$obitos_40_49_f+covid.pt$obitos_40_49_m)/(covid.pt$confirmados_40_49_f+covid.pt$confirmados_40_49_m))
tl50_59 <- as.data.frame((covid.pt$obitos_50_59_f+covid.pt$obitos_50_59_m)/(covid.pt$confirmados_50_59_f+covid.pt$confirmados_50_59_m))
tl60_69 <- as.data.frame((covid.pt$obitos_60_69_f+covid.pt$obitos_60_69_m)/(covid.pt$confirmados_60_69_f+covid.pt$confirmados_60_69_m))
tl70_79 <- as.data.frame((covid.pt$obitos_70_79_f+covid.pt$obitos_70_79_m)/(covid.pt$confirmados_70_79_f+covid.pt$confirmados_70_79_m))
tl80plus <- as.data.frame((covid.pt$obitos_80_plus_f+covid.pt$obitos_80_plus_m)/(covid.pt$confirmados_80_plus_f+covid.pt$confirmados_80_plus_m))




#letalidade_etaria<- as.data.frame(covid.pt[,65:82]/covid.pt[,23:40])
#letalidade_etaria <- cbind(covid.pt$data, letalidade_etaria)

let_etaria_temporal <- cbind(covid.pt$data, tl0_9, tl10_19, tl20_29, tl30_39, tl40_49, tl50_59, tl60_69, tl70_79, tl80plus)
names(let_etaria_temporal) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80plus")

let_etaria_temporal <- as.data.table(let_etaria_temporal)
let_temporal <- melt(let_etaria_temporal, id.vars="Data")
names(let_temporal)[2]<-"Grupo"
names(let_temporal)[3]<-"Valores"

ggplot(let_temporal, aes(x=Data, y=Valores*100, color=Grupo)) +
  geom_line(size=0.7) +
  labs(title="Evolucao da Letalidade nos grupos etarios")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")


# Comparar regi?es relativamente a:
#Incid?ncia

populacao_acores <- 242796
populacao_madeira <- 254254
populacao_alentejo<- 705018
populacao_algarve <- 438635
populacao_centro <- 2216927
populacao_lvt <- 2854802
populacao_norte <- 3573961


arsnorte <- max(covid.pt$confirmados_arscentro) / populacao_norte
arscentro <- max(covid.pt$confirmados_arscentro) / populacao_centro
arslvt <- max(covid.pt$confirmados_arslvt) / populacao_lvt
arsalentejo <- max(covid.pt$confirmados_arsalentejo) / populacao_alentejo
arsalgarve <- max(covid.pt$confirmados_arsalgarve) / populacao_algarve
acores <- max(covid.pt$confirmados_acores) / populacao_acores
madeira <- max(covid.pt$confirmados_madeira) / populacao_madeira

#Criar tabela de incidencia por regi?es

incidencia_regioes <- c(arsalentejo, arsalgarve, arscentro, arslvt, arsnorte, acores, madeira)
incidencia_regioes <- as.data.frame(incidencia_regioes)
row.names(incidencia_regioes) <- c("alentejo", "algarve", "centro", "lisboa e VT", "norte", "a?ores", "madeira")

incidencia_regioes <- incidencia_regioes %>%
  rownames_to_column(var="Regioes")
names(incidencia_regioes)[2] <- "Incidencia"

#Criar gr?fico de incid?ncia por regi?o

ggplot(incidencia_regioes, aes(x= Regioes, y= Incidencia*100))+
  theme_classic()+
  geom_bar(stat = "identity", color="#82b6bf", fill="#82b6bf")+
  geom_text(aes(label=scales::percent(round(Incidencia, digits = 4))), vjust=-0.5, size=3)+
  labs(title = "Incidencia da COVID-19 em cada regiao")



#Mortalidade (nº de mortos ou taxa de mortalidade)
  

tm_alentejo <- max(covid.pt$obitos_arsalentejo) / populacao_alentejo
tm_algarve <- max(covid.pt$obitos_arsalgarve) / populacao_algarve
tm_centro <- max(covid.pt$obitos_arscentro) / populacao_centro
tm_lvt <- max(covid.pt$obitos_arslvt) / populacao_lvt
tm_norte <- max(covid.pt$obitos_arsnorte) / populacao_norte
tm_a?ores <- max(covid.pt$obitos_acores) / populacao_acores
tm_madeira <- max(covid.pt$obitos_madeira) /populacao_madeira

#Criar tabela da mortalidade em cada regi?o

tm_regioes <- c(tm_alentejo, tm_algarve, tm_centro, tm_lvt, tm_norte, tm_a?ores, tm_madeira)
tm_regioes <- as.data.frame(tm_regioes)

rownames(tm_regioes) <- c("alentejo", "algarve", "centro", "Lisboa e VT", "norte", "a?ores", "madeira")

tm_regioes <- tm_regioes %>%
  rownames_to_column(var="Regioes")
names(tm_regioes)[names(tm_regioes) == "tm_regioes"] <- "Taxa_Mortalidade"


#Criar gr?fico da mortalidade em cada regi?o

ggplot(tm_regioes, aes(x=Regioes, y=Taxa_Mortalidade*100))+
  geom_col(fill="#82b6bf", width=0.5)+
  scale_x_discrete(labels=c("alentejo", "algarve", "centro", "Lisboa e VT", "norte", "a?ores", "madeira"))+
  theme_classic()+
  geom_text(aes(label=scales::percent(round(Taxa_Mortalidade, digits = 4))), vjust=-0.5, size=3)+
  labs(y="Taxa de Mortalidade", title = "Taxa de Mortalidade em cada regiao", x= "")


#Letalidade em cada regiao - mortos/confirmados em cada regiao


let_alentejo <- as.data.frame(covid.pt$obitos_arsalentejo/covid.pt$confirmados_arsalentejo)
let_algarve <- as.data.frame(covid.pt$obitos_arsalgarve/covid.pt$confirmados_arsalgarve)
let_centro <- as.data.frame(covid.pt$obitos_arscentro/covid.pt$confirmados_arscentro)
let_lvt <- as.data.frame(covid.pt$obitos_arslvt/covid.pt$confirmados_arslvt)
let_norte <- as.data.frame(covid.pt$obitos_arsnorte/covid.pt$confirmados_arsnorte)
let_acores <- as.data.frame(covid.pt$obitos_acores/covid.pt$confirmados_acores)
let_madeira<- as.data.frame(covid.pt$obitos_madeira/covid.pt$confirmados_madeira)

#Criar Tabela da Letalidade nas Regoes
let_regioes <- cbind(covid.pt$data, let_alentejo, let_algarve, let_centro, let_lvt, let_norte, let_acores, let_madeira)

names(let_regioes) <- c("Data", "Alentejo", "Algarve", "Centro", "Lisboa e VT", "Norte", "Acores", "Madeira")
let_regioes <- melt(let_regioes, id.vars="Data")
names(let_regioes)<- c("Data", "Regioes", "Letalidade")

#Criar Gr?fico da Letalidade nas regi?es

ggplot(let_regioes, aes(x=Data, y=Letalidade*100, color=Regioes)) +
  geom_line(size=0.85) +
  labs(title="Evolucao da Letalidade nas regioes")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

#Criar colunas de confirmados por faixa etaria, de mortos por faixa et?ria e de recuperados

covid.pt <- covid.pt %>%
  mutate(confirmados0_9=confirmados_0_9_f+confirmados_0_9_m,
         confirmados10_19=confirmados_10_19_f+confirmados_10_19_m,
         confirmados20_29=confirmados_20_29_f+confirmados_20_29_m,
         confirmados30_39=confirmados_30_39_f+confirmados_30_39_m,
         confirmados40_49=confirmados_40_49_f+confirmados_40_49_m,
         confirmados50_59=confirmados_50_59_f+confirmados_50_59_m,
         confirmados60_69=confirmados_60_69_f+confirmados_60_69_m,
         confirmados70_79=confirmados_70_79_f+confirmados_70_79_m,
         confirmados80plus=confirmados_80_plus_f+confirmados_80_plus_m)

covid.pt<-covid.pt%>%
  mutate(mortes0_9=obitos_0_9_f+obitos_0_9_m,
         mortes10_19=obitos_10_19_f+obitos_10_19_m,
         mortes20_29=obitos_20_29_f+obitos_20_29_m,
         mortes30_39=obitos_30_39_f+obitos_30_39_m,
         mortes40_49=obitos_40_49_f+obitos_40_49_m,
         mortes50_59=obitos_50_59_f+obitos_50_59_m,
         mortes60_69=obitos_60_69_f+obitos_60_69_m,
         mortes70_79=obitos_70_79_f+obitos_70_79_m,
         mortes80plus=obitos_80_plus_f+obitos_80_plus_m)

#Recuperados (Evolu??o)
## Criar tabela com os recuperados e os confirmados 


Data <- as.data.table(covid.pt$data)


Ev_recuperados <- as.data.table(cbind(Data, covid.pt$recuperados, covid.pt$confirmados))
names(Ev_recuperados)[1]<- "Data"

Ev_rec_con <- melt.data.table(Ev_recuperados, id.vars = "Data")
names(Ev_rec_con) <- c("Data", "Estado", "Casos")


## Criar grafico com os recuperados e confirmados


  ggplot(Ev_rec_con, aes(x=Data, y=Casos, color=Estado)) +
  geom_line(size=0.75) +
  labs(title="Evolucao do numero de recuperados e confirmados")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")



#Recuperados por regiao (Evolucao)
  
##Cria tabela com os recuperados por regiao

Recuperados_regiao <- cbind(Data,
                            covid.pt$recuperados_acores,
                            covid.pt$recuperados_arsalentejo,
                            covid.pt$recuperados_arsalgarve,
                            covid.pt$recuperados_arscentro,
                            covid.pt$recuperados_arslvt,
                            covid.pt$recuperados_arsnorte,
                            covid.pt$recuperados_madeira,
                            covid.pt$recuperados_estrangeiro)

Recuperados_regiao <- na.omit(Recuperados_regiao)
names(Recuperados_regiao) <- c("Data", "Acores", "Alentejo", "Algarve", "Centro", "Lisboa e VT", "Norte", "Madeira", "Estrangeiro")

Recuperados_regiao <- melt.data.table(Recuperados_regiao, id.vars="Data")
names(Recuperados_regiao) <- c("Data", "Regiao", "Casos")

## Criar grafico


 ggplot(Recuperados_regiao, aes(x=Data, y=Casos, color=Regiao)) +
  geom_line(size=0.75) +
  labs(title="Evolucao do numero de recuperados em cada regiao")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

#Internados/Internados UCI (n? absoluto e por n? de casos)
#N? absoluto
  internados <- as.data.frame(covid.pt$internados)
internados_uci <- as.data.frame(covid.pt$internados_uci)

Ev_internados <- cbind(covid.pt$data, internados, internados_uci)
names(Ev_internados)<- c("Data", "Internados", "Internados_UCI")
Ev_internados <- melt(Ev_internados, id.vars="Data")
names(Ev_internados)<-c("Data", "Estado", "Casos")

#Gr?fico da evolu??o dos internados ao longo do tempo

ggplot(Ev_internados, aes(x=Data, y=Casos, color=Estado)) +
  geom_line(size=0.45) +
  labs(title="Evolu??o do n?mero de internados")+
scale_x_date(date_labels = "%b", date_breaks = "1 month")

#N? de novos casos ao longo do tempo por faixa et?ria

novos_0 <- as.data.frame(covid.pt$confirmados0_9-lag(covid.pt$confirmados0_9))
novos_10 <- as.data.frame(covid.pt$confirmados10_19-lag(covid.pt$confirmados10_19))
novos_20 <- as.data.frame(covid.pt$confirmados20_29-lag(covid.pt$confirmados20_29))
novos_30 <- as.data.frame(covid.pt$confirmados30_39-lag(covid.pt$confirmados30_39))
novos_40 <- as.data.frame(covid.pt$confirmados40_49-lag(covid.pt$confirmados40_49))
novos_50 <- as.data.frame(covid.pt$confirmados50_59-lag(covid.pt$confirmados50_59))
novos_60 <- as.data.frame(covid.pt$confirmados60_69-lag(covid.pt$confirmados60_69))
novos_70 <- as.data.frame(covid.pt$confirmados70_79-lag(covid.pt$confirmados70_79))
novos_80 <- as.data.frame(covid.pt$confirmados80plus-lag(covid.pt$confirmados80plus))

#Tabela dos novos casos por faixa et?ria ao longo do tempo

Ev_novos <- cbind(covid.pt$data,
                  novos_0,
                  novos_10,
                  novos_20,
                  novos_30,
                  novos_40,
                  novos_50,
                  novos_60,
                  novos_70,
                  novos_80)
names(Ev_novos) <-  c("Data", "0_9", "10_19", "20_29", "30_39", "40_49", "50_59", "60_69", "70_79", "80plus")
Ev_novos <- melt(Ev_novos, id.vars="Data")
names(Ev_novos) <- c("Data", "Grupos", "Novos_casos")

#Gr?fico com a evolu??o

ggplot(Ev_novos, aes(x=Data, y=Novos_casos, color=Grupos)) +
  geom_line(size=0.3) +
  labs(title="Evolu??o dos novos casos por grupo etario", y="Novos casos")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")


## MAPAS

mapa_portugal <- rgdal::readOGR("C:/Users/teres/Desktop/Est?gio/covid19pt-data-master/extra/mapas/portugal_continental.geojson")



