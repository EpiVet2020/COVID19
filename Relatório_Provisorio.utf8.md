---
title: "Análise COVID-19"
author: "Estagiárias"
date: "30/09/2020"
output:
  html_document:
    code_folding: hide
    df_print: paged
    theme: lumen
    toc: yes
    toc_depth: 3
  pdf_document:
    toc: yes
    toc_depth: '3'
  html_notebook:
    code_folding: hide
    theme: lumen
    toc: yes
    toc_depth: 3
    toc_float: yes
editor_options: 
  chunk_output_type: inline
---


<style type="text/css">
.main-container {
  max-width: 1000px;
  margin-left: auto;
  margin-right: auto;
}
</style>



<style type="text/css">
/* Whole document: */
body{
  font-size: 12pt;
}
</style>


<br>

## **Introdução**
***

Com este relatório pretendemos fazer uma análise sobre a COVID-19 em Portugal, com base nos dados da DGS disponíveis em: <https://github.com/dssg-pt/covid19pt-data>.

Este relatório tem como objetivo realizar uma análise detalhada dos seguintes tópicos:
<ul>
  <li>Sintomas</li>
  <li>Testagem</li>
  <li>Casos</li>
  <li>Mortes</li>
  <li>Internamentos</li>
  <li>Recuperação</li>
</ul>

Nota: Houve um lapso na base de dados e os óbitos cumulativos por idade e por sexo para dia 5/10 estão todos a 0. Subituimos esses 0's pelos valores do dia anterior.


```r
#Libraries
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
library(sf)
library(grid)
library(gridExtra)
library(htmltools)
library(zoo)

#Importar dados
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
covid19pt_testes <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/amostras.csv")

# Tratar base de dados do covid

## Houve um lapso na base de dados e os obitos cumulativos por idade e por sexo para dia 5/10 (linha 223) estao a 0's. Subituimos esses 0's pelos mesmos valores do dia anterior

covid19pt[223, 65:84] = covid19pt[222, 65:84]

#Data de chr para Date
covid19pt$data <- as.Date(covid19pt$data,"%d-%m-%Y")
covid19pt_testes$data <- as.Date(covid19pt_testes$data,"%d-%m-%Y")

#Mapa de Portugal
mapa_pt <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson",what = "sp")
```


## **Sintomas** {.tabset .tabset-fade .tabset-pills}
***

Na base de dados utilizada, as colunas 41 à 46 apresentam a percentagem de casos infetados que reportaram o sintoma respetivo. O comportamento dos dados leva-nos a crer que estes valores sejam cumulativos.

Estes dados só foram registados de dia 03/03/2020 até dia 16/08/2020 e, para além disso, tal como é referido na base de dados online, estes dados são relativos apenas a uma percentagem, não-especificada e variável, dos casos infetados. Por este motivo, apesar de termos realizado duas análises, estes resultados podem não ser fiáveis nem representativos da realidade.

<br><br>

### Frequência Relativa
<br>

Os valores apresentadas neste gráfico de barras referem-se à percentagem de casos infetados que reportaram cada um dos sintomas. O sintoma mais frequente foi a tosse com 34% dos infetados a apresentarem esse sintoma, seguindo-se a febre com 27% e a cefaleia e dores musculares ambos com 20%.


```r
#Criar uma tabela com uma coluna para os sintomas, dar-lhe o nome "sintoma", mudar o nome de cada sintoma na tabela e criar outra coluna para os últimos valores registados de cada sintoma e dar-lhe o nome "frequencia"
sintomas <- as.data.frame(t(covid19pt[173,41:46])) %>% 
  rownames_to_column(var = "sintoma")
names(sintomas)[2] = "frequencia"
sintomas[, 1] = c("Tosse", "Febre", "Dificuldade Respiratória", "Cefaleia", "Dores Musculares", "Fraqueza Generalizada")

#Fazer um gráfico de barras com os sintomas no eixo do x e a percentagem no eixo dos y
ggplot(sintomas, aes(x = sintoma, y = frequencia*100)) +
  geom_col(fill = "slategray3",  width = 0.7) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(sintomas$frequencia*100 + 10))) +
  theme_classic() +
  labs(x = "",
       y = "Frequência (%)") +
  theme(axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.x = element_text(size=10, 
                                   color = "black"),
        axis.text.y = element_text(size=12, 
                                   color = "black")) +
  geom_text(aes(label = scales::percent(frequencia, digits = 2)), 
            vjust = -0.5, 
            size = 4) +
  scale_x_discrete(labels = c("Cefaleia", "Dificuldade\nRespiratória", "Dores\nMusculares",
                              "Febre", "Fraqueza\nGeneralizada", "Tosse"))
```

<img src="Relatório_Provisorio_files/figure-html/pressure-1.png" width="672" style="display: block; margin: auto;" />



### Evolução da Frequência Relativa
<br>
Para entender melhor este gráfico é necessário perceber que os valores negativos indicam que houve uma diminuição na percentagem de pessoas com determinado sintoma clínico. Os valores positivos indicam que houve um aumento dessa percentagem sendo que os valores positivos menores do que o do dia anterior, significam que houve crescimento, mas menor.

Assim, é possível verificar que inicialmente existiu uma grande variação na frequência de casos confirmados que reportaram cada um dos sintomas. É de notar que a partir de junho esta variação foi sendo cada vez menor tornando os valores mais constantes, à exceção da tosse. 


```r
#Criar uma tabela com colunas, uma para cada sintoma, sendo o valor para cada dia a frequência desse sintoma nesse dia vezes o número de confirmados até esse dia menos a frequência desse sintoma no dia anterior vezes o número de confirmados até ao dia anterior, isto para nos dar o número de pessoas com esse sintoma nesse dia apenas, e depois tudo a dividir pelo número de novos confirmados nesse dia para termos o resultado em percentagem. 
sintomas_tempo <- as.data.frame((covid19pt[8:173,41:46]*covid19pt$confirmados[8:173])
                                -(covid19pt[7:172, 41:46]*covid19pt$confirmados[7:172]))/covid19pt$confirmados_novos[8:173] 

#É ainda necessário acrescentar a linha 7 pois como são as primerias frequências a aparecer já indicam a frequência específica para esse dia
sintomas_tempo <- rbind(covid19pt[7, 41:46], sintomas_tempo)

#Acrescentar à tabela que criámos, uma coluna com as datas
sintomas_tempo <- cbind(covid19pt$data[7:173], sintomas_tempo)

#No casos do sintoma Dificuldade Respiratória, este só começou a ser registado mais tarde pelo que o primerio valor que já representa a frequência específica para esse dia é o da linha 3, coluna 4
sintomas_tempo[3, 4] <- 0.11

#Mudar os nomes das colunas
names(sintomas_tempo) <- c("Data", "Tosse", "Febre", "Dificuldade \nrespiratória", "Cefaleia", 
                           "Dores \nMusculares",  "Fraqueza \nGeneralizada")

#Fazer o melt da tablea para poder representar a ecolução temporal num gráfico e mudar o nome da coluna 2 para Sintomas
sintomas_tempo_melt <-  melt(sintomas_tempo, id.vars="Data")
names(sintomas_tempo_melt)[2] <- "sintoma"
#Fazer um gráfico de linhas com a data no eixo do x, a frequência diária dos sintomas no eixo dos y e cada sintoma numa linha
ggplot(sintomas_tempo_melt, aes(x = Data, y = value*100, color = sintoma)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$sintoma) +
  guides(color = FALSE) +
  labs(x = "",
       y = "Frequência (%)") +
  theme(axis.title.y = element_text(margin = margin(r = 15, l = 10),
                                    size = 12),
        strip.text.y = element_text(size = 8, 
                                    angle = 0))
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-3-1.png" width="672" style="display: block; margin: auto;" />


<br>

## **Testagem** {.tabset .tabset-fade .tabset-pills}
***

Um dos fatores importantes a ter em conta é o número de pessoas testadas para a COVID-19. Assim sendo, utilizámos outra base de dados da mesma fonte para obter informações relativas à testagem.

É importante notar que os valores da base de dados *"correspondem ao número de amostras processadas para diagnóstico de SARS-CoV-2 em laboratórios públicos e privados desde o dia 1 de março."* Para além disso, no dashboard da DGS dizem ainda que *"Os dados diários após 2 de abril de 2020 ainda estão a ser recolhidos, pelo que os valores no gráfico poderão sofrer alterações."* Existe a possibilidade de, a cada dia, dados referentes a dias anteriores serem alterados, provavelmente pelo facto da informação relativa ao processamento de amostras ser recebida pela DGS com alguns dias de desfasamento.

<br>

### Evolução do Número de Testes Realizados
<br>
Com a análise deste gráfico é possível verificar que o número de testes diários realizados tem vindo a demonstrar uma tendência crescente. 

É ainda possível tirar outras conclusões, nomeadamente que os dias com menor testagem são  maioritariamente o domingo, mas também o sábado, e que os dias com maior testagem são principalmente as terças e quartas.


```r
#Tabela com coluna para data e outra para número de testes feitos nesse dia
testes_diarios <- covid19pt_testes[,c(1, 3)]
names(testes_diarios) = c("Data", "Testes")

#Gráfico de pontos e linhas com data no eixo do x e número de testes no eixo do y
testes_diarios_grafico <- ggplot(testes_diarios, aes(x = Data, y = Testes)) + 
  geom_point(color = "cadetblue", aes(text = paste('Data: ', Data,
                              '<br>Número de testes: ', Testes))) +
  geom_line(size = 0.4, color = "cadetblue") +
  labs(x = "") +
  theme(axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Fazer com que gráfico seja interativo
testes_diarios_grafico_interativo <- ggplotly(testes_diarios_grafico,  tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Testes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

#Testagem com média rolante 7 dias
##Fazer a média rolante
testes_media_rolante <- cbind(covid19pt_testes[7:nrow(covid19pt_testes),1], rollmean(covid19pt_testes[,3], k = 7))
names(testes_media_rolante) = c("Data", "Testes")

##Fazer o gráfico
testes_media_rolante_grafico <- ggplot(testes_media_rolante, aes(x = Data, y = Testes)) +
  geom_point(size = 0.1, color = "cadetblue", aes(text = paste('Data: ', Data,
                              '<br>Número de testes: ', Testes))) +
  geom_line(size = 0.5, color = "cadetblue") +
  labs(title = "Média Rolante (7 dias)",
       x = "",
       y = "Número de Testes") +
  scale_x_date(breaks = "months", date_labels = "%b")

##Fazer com que gráfico seja interativo
testes_media_rolante_grafico_interativo <- ggplotly(testes_media_rolante_grafico,  tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Testes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      testes_diarios_grafico_interativo
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      testes_media_rolante_grafico_interativo
    )
  ))
)
```

preserve80180128cfcd612c



### Evolução da Taxa de Testes Positivos
<br>
Segundo o gráfico de linhas, em abril verificou-se a maior percentagem de testes positivos face ao número de testes totais, com valores perto dos 15%. A partir daí esta percentagem tem vindo a diminuir o que mostra que: há menos casos positivos e/ou há mais testes a serem realizados por dia. Nos últimos meses a percentagem foi quase sempre menor do que 5% o que, face aos meses anteriores, pode demonstrar que a testagem tem sido mais abrangente, sendo provável que se esteja a testar grande parte dos casos positivos.

Além disso, podemos ver que, principalmente ao domingo, a percentagem de testes positivos é constantemente maior face aos outros dias da semana. Esta informação é coerente com o gráfico anterior, uma vez que é nestes dias que se verifica um menor número de testes diários e assim, supomos que dão primazia aos casos mais urgentes, aumentando assim a percentagem de testes positivos ao domingo. O contrário verifica-se à terça, dia em que são testadas mais pessoas, existindo normalmente uma menor percentagem de testes positivos.


```r
#Tabela com coluna para data e coluna para número de casos confirmados nesse dia
casos_diarios <- covid19pt[, c(1, 12)]
names(casos_diarios) = c("Data", "Casos")

###Tabela com coluna para data e outra para casos desse dia a dividir por número de testes desse dia
testes_positivos <- cbind(testes_diarios[,1], as.data.frame((casos_diarios[1:nrow(testes_diarios),2]/testes_diarios[,2])*100))
names(testes_positivos) = c("Data", "Percentagem_Positivos")

###Fazer gráfico com data no eixo do x e proporção de testes positivos no eixo do y
testes_positivos_grafico <- ggplot(testes_positivos, aes(x = Data, y = Percentagem_Positivos)) +
  geom_line(color = "lightseagreen") +
  geom_point(color = "lightseagreen", aes(text = paste('Data: ', Data,
                              '<br>Taxa de Testes Positivos (%): ', Percentagem_Positivos))) +
  labs(x = "",
       y = "Frequência (%)") +
  scale_x_date(breaks = "months", date_labels = "%b")

###Fazer com que gráfico seja interativo
ggplotly(testes_positivos_grafico,  tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Testes Positivos (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
```

preservec55972a9dd09584c

<br>

## **Casos** {.tabset .tabset-fade .tabset-pills}
***

Neste tópico vamos abordar a incidência (número de novos casos diários), a taxa de incidência (número de novos casos diários/população em risco), número de casos cumulativos em valor absoluto e prevalência (número total de casos/população).

<br>

### Geral {.tabset}
#### Evolução da Incidência
<br>
Na linha da incidência é possível ver que o número de casos diários teve um pico no dia 10/04/2020. Uma vez que o número de testes não aumentou de forma significativa nos dias anteriores, uma das justificações possíveis é ter ocorrido um lapso na notificação dos casos nesse dia.

Podemos ainda constatar um ligeiro aumento no número de casos durante os meses de maio e junho. Este aumento pode ser justificado na sequência do período de férias, que apesar das restrições impostas, leva sempre a um aumento das deslocações e a um maior número de contactos entre pessoas. A diminuição da incidência verificada entre julho e agosto pode ser justificada pelo facto dos possíveis infetados com sintomas ligeiros não recorrerem à linha de saúde 24, por não quererem fazer um isolamento profilático durante as suas férias.

À data desta análise, desde o início de setembro, a subida do número de casos tem sido crescente, sendo possíveis justificações:
<ul>
  <li>Regresso dos trabalhadores ao emprego presencial</li>
  <li>Regresso dos jovens às escolas</li>
  <li>Aumento da testagem sobretudo a partir de dia 08/09/2020</li>
  <li>Regresso a uma vida mais normal com mais contactos entre pessoas e menos cuidados e preocupações do que no início da pandemia</li>
</ul>

No gráfico da Evolução da Taxa de Testes Positivos, é possível ver que, desde o início de setembro, tem havido um aumento na percentagem de casos positivos. No entanto, não é um aumento tão acentuado como aquele verificado neste gráfico da Evolução da Incidência. Isto indica-nos que o aumento do número de casos não é totalmente explicado pelo aumento do número de testes realizados.


```r
#Fazer gráfico de linhas com data no eixo do x e número de casos no eixo do y
casos_diarios_grafico <- ggplot(casos_diarios, aes(x = Data, y = Casos))+
  geom_point(color = "coral3", aes(text = paste('Data: ', Data,
                              '<br>Incidência : ', Casos))) +
  geom_line(size = 0.4, color = "coral3")+
  labs(x = "") +
  theme(axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Fazer com que gráfico seja interativo
ggplotly(casos_diarios_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
```

preservedf82c68f0f39db06


### Administração Regional de Saúde (ARS) {.tabset}
#### Número de Casos
<br>
Neste gráfico e mapa podemos ver que Lisboa e Vale do Tejo é a ARS com mais casos, seguindo-se a ARS Norte. Com menos casos, é de realçar os Açores e a Madeira que, com as estratégias implementadas para os turistas que obrigam a realização de um teste ao SARS-CoV-2 negativo até 72h antes da ida ou a realização do teste à chegada ao aeroporto, cumprindo o isolamento profilático até à receção do resultado do mesmo, conseguiram limitar o aumento do número de casos. No entanto, temos de ter presente que este gráfico e mapa não têm em consideração a população de cada ARS.


```r
#Criar tabela com uma coluna para a região e outra para o valor mais recente do número de casos confirmados, mudar o nome das colunas e mudar o nome das regiões
casos_regioes <- as.data.frame(t(as.data.frame(lapply(covid19pt[,confirmados_arsnorte:confirmados_madeira],
                                                      max, na.rm = TRUE))))%>% 
  rownames_to_column(var = "Regioes")
names(casos_regioes)[2] <- "Casos"
casos_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

#Fazer um gráfico de barras com as regiões no eixo do x e o número de casos no eixo do y
casos_regioes_grafico <- ggplot(casos_regioes, aes(x = Regioes, y = Casos)) +
  geom_col(fill = "salmon1", width = 0.5, aes(text = paste('ARS: ', Regioes,
                              '<br>Nº de Casos: ', Casos))) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "") +
  theme(axis.title.y = element_text(size = 12))

casos_regioes_grafico_interativo <- ggplotly(casos_regioes_grafico, tooltip = "text") %>% 
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

#mapa
##Definir intervalos que queremos na legenda
bins =  c(0, 400, 3000, 10000, 20000, 40000, 60000, Inf)
##Colocar as Regiãoes da tabela pela mesma ordem que a dos poligonos
casos_regioes_ordem <- casos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

##Definir a palete de cores para o mapa
pal <- colorBin("YlOrRd", domain = casos_regioes_ordem[,2], bins = bins)

##Definir legenda que aparece quando se passa o rato pelo mapa
labels <- sprintf(
  "<strong>%s</strong><br/>%g casos",
  casos_regioes_ordem[,1], casos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

##Fazer o mapa
casos_regioes_mapa <- leaflet(mapa_pt) %>% 
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
  addLegend(pal = pal, values = casos_regioes_ordem$Casos, opacity = 0.7, title = "Número de Casos por ARS",
            position = "bottomright")

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      casos_regioes_mapa
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      casos_regioes_grafico_interativo
    )
  ))
)
```

preservead6bcaff4cbbe09c


#### Evolução da Incidência
<br>
Com a interpretação deste gráfico de linhas, podemos ver que as ARSs do Norte e do Centro têm tido um comportamento semelhante. Ambas tiveram um grande aumento em abril, tendo-se mantido estáveis durante os meses de verão, começando a aumentar de novo em setembro. No entanto, é importante realçar que no iníco da pandemia a ARS do Norte apresentou um número de novos casos muito superior a todas as outras ARSs, pois foi onde surgiram os primeiros surtos deste vírus em Portugal.

A ARS de Lisboa e Vale do Tejo, por outro lado, não apresentou tantos casos diários em abril como a ARS do Norte tendo iniciado o confinamento com menor número de casos. Em maio, com o fim do confinamento, é notável um aumento considerável no número de novos casos que se prolongou até julho. No verão a incidência sofreu um decréscimo, que pode ser resultado de deslocações para outras regiões durante o período de férias. A partir setembro, com o fim das férias e o regresso às escolas e à atividade profissional, a incidência tem vindo a apresentar valores mais elevados.

Relativamente às ARSs do Alentejo e do Algarve, podemos verificar um aumento significativo do número de novos casos a partir de meados de junho, correspondente ao período de férias. Como em todas as outras ARSs de Portugal continental, a incidência tem vindo a aumentar desde o início de setembro pelas mesmas razões apresentadas anteriormente.

Quanto aos Açores e à Madeira, é notável a ausência quase total de novos casos durante os meses de maio e junho, resultado de uma testagem massiva com adequado controlo dos casos iniciais e da implementação de medidas contenção, como a quarentena obrigatória e permissão de entrada exclusiva a residentes. A partir de julho, voltaram a surgir alguns casos devido ao levantamento da restrição à entrada de turistas. É também de realçar que são as únicas regiões que não têm sofrido um aumento desde setembro. 


```r
#Fazer tabela com número de casos diários por região
incidencia_regioes_tempo <- cbind(covid19pt$data, (as.data.frame(covid19pt[, confirmados_arsnorte:confirmados_madeira] -lag(covid19pt[,confirmados_arsnorte:confirmados_madeira]))))
names(incidencia_regioes_tempo) =  c("Data", "Norte", "Centro", "LVT", "Alentejo", 
                                     "Algarve", "Açores", "Madeira")
incidencia_regioes_tempo_melt <- melt(incidencia_regioes_tempo, id.vars = "Data")
names(incidencia_regioes_tempo_melt) = c("Data", "Regiao", "Incidencia")

#Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
incidencia_regioes_tempo_grafico <- ggplot(incidencia_regioes_tempo_melt, aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_point(aes(text = paste('Data: ', Data,
                              '<br>ARS: ', Regiao,
                              '<br>Incidência: ', Incidencia)), size = 0.1) +
  geom_line(size = 0.5) +
  labs(x = "") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        legend.position = "none") +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar interativo
incidencia_regioes_tempo_grafico_interativo <- ggplotly(incidencia_regioes_tempo_grafico, tooltip = "text") %>% 
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

#Incidência com média rolante 7 dias
##Fazer a média rolante
incidencia_regioes_tempo_media_rolante <- cbind(incidencia_regioes_tempo[7:nrow(incidencia_regioes_tempo),1],
                                                as.data.frame(rollmean(incidencia_regioes_tempo[,2:8], k = 7)))
names(incidencia_regioes_tempo_media_rolante)[1] = "Data"

incidencia_regioes_tempo_media_rolante_melt <- melt(incidencia_regioes_tempo_media_rolante, id.vars = "Data")
names(incidencia_regioes_tempo_media_rolante_melt) = c("Data", "Regiao", "Incidencia")

##Fazer gráfico
incidencia_regioes_tempo_media_rolante_melt_grafico <- ggplot(incidencia_regioes_tempo_media_rolante_melt, 
                                                              aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_point(aes(text = paste('Data: ', Data,
                              '<br>ARS: ', Regiao,
                              '<br>Incidência: ', Incidencia)), size = 0.1) +
  geom_line(size=0.5) +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Média Rolante (7 dias)",
       x = "",
       y = "Incidência") +
  scale_x_date(breaks = "months", date_labels = "%b")

##Tornar gráfico interativo
incidencia_regioes_tempo_media_rolante_melt_grafico_interativo <- ggplotly(incidencia_regioes_tempo_media_rolante_melt_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


#mapa
##Fazer tabela com novos casos mais recentes
incidencia_regioes_recente <- as.data.frame(t(as.data.frame(
  lapply((covid19pt[, confirmados_arsnorte:confirmados_madeira])
         - lag(covid19pt[, confirmados_arsnorte:confirmados_madeira]), last)))) %>% 
  rownames_to_column(var = "Regioes")
incidencia_regioes_recente[,1] = c("Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                   "Algarve", "Açores", "Madeira")

##Definir intervalos para legenda
bins_7 =  c(0, 50, 300, 500, 800, 1000, 1500, 2000, Inf)

##Definir palete de cores para mapa
pal_7 <- colorBin("YlOrRd", domain = incidencia_regioes_recente[,2], bins = bins_7)

##Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regiãoes
incidencia_regioes_recente <- incidencia_regioes_recente[c(4, 5, 6, 2, 7, 1, 3),] 

##Definir legenda quando se passa com o rato por cima
labels_7 <- sprintf(
  "<strong>%s</strong><br/>%g casos novos",
  incidencia_regioes_recente[,1], round(incidencia_regioes_recente[,2], digits = 4)
) %>% lapply(htmltools::HTML)


##Fazer o mapa
incidencia_regioes_mapa <- leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_7(incidencia_regioes_recente[,2]), 
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
    label = labels_7,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>% 
  addLegend(pal = pal_7, values = incidencia_regioes_recente[,2], opacity = 0.7, title = "Incidência Atual por ARS",
            position = "bottomright")

#Incidência com média rolante 14 dias
##Fazer a média rolante
incidencia_regioes_tempo_media_rolante_14 <- cbind(incidencia_regioes_tempo[14:nrow(incidencia_regioes_tempo),1],
                                                as.data.frame(rollmean(incidencia_regioes_tempo[,2:8], k = 14)))
names(incidencia_regioes_tempo_media_rolante_14)[1] = "Data"

incidencia_regioes_tempo_media_rolante_14_melt <- melt(incidencia_regioes_tempo_media_rolante_14, id.vars = "Data")
names(incidencia_regioes_tempo_media_rolante_14_melt) = c("Data", "Regiao", "Incidencia")

##Fazer gráfico
incidencia_regioes_tempo_media_rolante_14_melt_grafico <- ggplot(incidencia_regioes_tempo_media_rolante_14_melt, 
                                                              aes(x = Data, y = Incidencia, color = Regiao)) +
  geom_point(aes(text = paste('Data: ', Data,
                              '<br>ARS: ', Regiao,
                              '<br>Incidência: ', Incidencia)), size = 0.1 ) +
  geom_line(size=0.5) +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Média Rolante (14 dias)",
       x = "",
       y = "Incidência") +
  scale_x_date(breaks = "months", date_labels = "%b")

##Tornar gráfico interativo
incidencia_regioes_tempo_media_rolante_14_melt_grafico_interativo <- ggplotly(incidencia_regioes_tempo_media_rolante_14_melt_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      incidencia_regioes_tempo_grafico_interativo
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      incidencia_regioes_tempo_media_rolante_melt_grafico_interativo
    )
  ))
)
```

preservefd5a86f15d606525

```r
browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      incidencia_regioes_tempo_media_rolante_14_melt_grafico_interativo
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      incidencia_regioes_mapa
    )
  ))
)
```

preserveb3b90dd418403c4b


#### Evolução da Taxa de Incidência
<br>
Com este gráfico de linhas conseguimos ver a evolução temporal das diferentes taxas de incidência para cada ARS. 

As regiões dos Açores e da Madeira são as que têm as taxas de incidência mais baixas e mais constantes. 

As ARSs do Algarve e do Alentejo têm um comportamento muito semelhante entre si. Tiveram um aumento em abril, como todas as outras ARSs, sofrendo uma diminuição posteriormente e mantendo-se constantes até ao iníco do verão. Como podemos ver, nestas duas ARSs muito procuradas como destino de férias, as taxas de incidência aumentaram logo a partir de junho. Contudo, a ARS do Alentejo mostra alguns picos no verão que podem ser referentes aos surtos em lares.

A ARS do Centro é, das ARSs de Portugal continental, a única que se tem mantido constante deste setembro, sem grandes variações.

A ARS do Norte foi a que teve uma maior taxa de incidência em abril, tendo estabilizado em meados de maio. Desde setembro voltou a subir de forma considerável. 

A ARS de Lisboa e Vale do Tejo é a que tem o comportamento mais díspar e preocupante. Sem nunca ter diminuído a taxa de incidência de forma tão acentuada como as outras ARSs, manteve-se com oscilações recorrentes ao longo do verão. Esta começou a subir a partir de setembro, apresentando o maior valor desde o início da pandemia.


```r
#Valores da população de cada Região com base nas CCDRs
acores = 242796
alentejo = 503507
algarve = 450484
centro = 2217285
lisboa = 3631738
madeira = 253945
norte = 3575338

#Criar uma tabela com uma coluna para as Regiãoes e outra para o número de pessoas nessa Região
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "açores", "madeira"))
colnames(populacao_regioes) <- "População"

###Fazer com que cada coluna seja uma Região e repetir cada número as vezes necessárias para ficar com o número igual ao das colunas da base de dados
populacao_regioes_rep <- as.data.frame(t(populacao_regioes[rep(seq_len(ncol(populacao_regioes)), each = nrow(covid19pt))]))

###Calcular a incidência em cada Região fazendo os casos novos por Região a dividir pela população da Região menos os confirmados da Região menos os óbitos da Região e dar nomes a cada coluna
incidencia_regioes <- cbind(covid19pt$data, (as.data.frame(covid19pt[, confirmados_arsnorte:confirmados_madeira] 
                                                      - lag(covid19pt[, confirmados_arsnorte:confirmados_madeira]))) 
                            / (populacao_regioes_rep - as.data.frame(covid19pt[,confirmados_arsnorte:confirmados_madeira])))
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve",
                               "Açores", "Madeira")

###Fazer melt para fazer o gráfico e dar nomes a cada coluna
incidencia_regioes_melt <- melt(incidencia_regioes, id.vars = "Data")
names(incidencia_regioes_melt) <- c("data", "regiao", "valor")

###Fazer o gráfico de linhas com a data no eixo do x, a incidência no eixo do y e a Região em cada linha
ggplot(incidencia_regioes_melt, aes(x = data, y = valor*100, color = regiao)) +
  geom_line() + 
  labs(x = "", 
       y = "Taxa de Incidência (%)") +
  facet_grid(incidencia_regioes_melt$regiao) +
  theme(axis.title.x = element_text(margin = margin(t = 15, b = 10), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 15, l = 10), 
                                    size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 8, angle = 0)) +
  guides(color = FALSE) +
  scale_x_date(breaks = "months", date_labels = "%b")
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-9-1.png" width="672" />

#### Taxa de Incidência Cumulativa
<br>
Como é possível verificar com a análise deste gráfico de barras e mapa, as ARSs de Lisboa e Vale do Tejo e do Norte apresentam valores de prevalência bastante superiores às outras ARSs. Desta forma, concluímos que, apesar de serem as ARSs com maior população, não é este fator que justifica diretamente o elevado número de casos.

Relativamente às ARSs do Algarve e do Alentejo, apesar de serem as ARSs com menor população de Portugal continental, têm as terceiras e quartas prevalências mais elevadas, respetivamente. Isto pode ser justificado pelo facto de ambas as ARSs serem destinos de eleição de férias, tendo sofrido um aumento da incidência durante o verão (gráfico da Evolução da Incidência por ARS). O facto de os turistas não serem contabilizados na população destas ARSs, contribui para o aumento da prevalência (isto se o turista for considerado confirmado na ARS onde testou positivo).

Como referido anteriormente, as baixas prevalências dos Açores e da Madeira confirmam o sucesso das medidas implementadas. 


```r
populacao_regioes_invertido <- t(populacao_regioes)

prevalencia_regiao <- (as.data.frame(t(as.data.frame((lapply(covid19pt[,confirmados_arsnorte:confirmados_madeira], last)))))*100 / populacao_regioes_invertido) %>% 
  rownames_to_column(var="Regiao")
colnames(prevalencia_regiao)[2] <- "Prevalencia"
prevalencia_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

prevalencia_regiao_grafico <- ggplot(prevalencia_regiao, aes(x=Regiao, y=Prevalencia)) + 
  geom_col(fill="palegreen", width = 0.5, aes(text = paste('ARS: ', Regiao,
                              '<br>Taxa de Incidência Cumulativa (%): ', Prevalencia))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(y="Prevalência (%)", x="") +
  theme(axis.title.y = element_text(size = 12))

prevalencia_regiao_grafico_interativo <- ggplotly(prevalencia_regiao_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Incidência Cumulativa (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
#Mapa
prevalencia_regiao_ordem <- prevalencia_regiao[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
prevalencia_regiao_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_prevalencia_regiao <- colorBin("viridis", domain = prevalencia_regiao_ordem$Prevalencia, bins = c(0.1, 0.3, 0.5, 0.8, 1, 1.3, 1.5, Inf))

labels_prevalencia_regiao <- paste( 
  "<strong>", prevalencia_regiao_ordem[,1],"</strong><br/>", 
  round(prevalencia_regiao_ordem[,2], digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

prevalencia_regiao_mapa <- leaflet(mapa_pt) %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              fillOpacity = 1, 
              color = "black", 
              weight = 1,
              fillColor = ~pal_prevalencia_regiao(prevalencia_regiao_ordem$Prevalencia),
              label = labels_prevalencia_regiao, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                          textsize = "13px", 
                                          direction = "auto")) %>% 
  addLegend("bottomleft", 
            pal = pal_prevalencia_regiao, 
            values = prevalencia_regiao_ordem$Prevalencia , 
            opacity = 0.5, 
            title = "Taxa de Incidência Cumulativa (%) por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite"), options = layersControlOptions(collapsed = FALSE))

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      prevalencia_regiao_mapa
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      prevalencia_regiao_grafico_interativo
    )
  ))
)
```

preservee4ccd1cf5d1b7991

### Faixa Etária e Género {.tabset .tabset-fade .tabset-dropdown}
#### Número de Casos por Faixa Etária e por Género

<br>

Neste gráfico podemos ver que a maior parte dos casos existentes são das faixas etárias entre os 20 e os 59 anos. Este valor parece fazer sentido uma vez que corresponde à população ativa, geralmente a população mais exposta. No entanto, é importante realçar que, por ser um valor absoluto, não têm em consideração a população de cada faixa etária, o que significa que pode haver mais casos nestas idades por serem simultaneamente as faixas etárias com maior número de pessoas em Portugal. 

Por outro lado, é visível que, em todas as faixas etárias, o género feminino é o que tem maior número de casos, com exceção do grupo etário dos 0 aos 9 anos. Esta assimetria pode ser explicada por, em muitas das profissões com grande exposição ao vírus, a maioria dos trabalhadares são mulheres. Novamente, estes dados podem não ser estatisticamente fiáveis por não se ter tido em conta a população existente de cada género.


```r
#Selecionar as colunas de confirmados feminino para todas as idades e juntá-las numa tabela e fazer o mesmo para o masculino
femininos <- as.data.frame(covid19pt %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))
masculinos <- as.data.frame(covid19pt %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

#Selecionar o valor mais recente de cada coluna de modo a ficar com o número de casos até ao momento para cada faixa etária e para cada género
casos_femininos_idade <- as.data.frame(lapply(femininos, last))
casos_masculinos_idade <- as.data.frame(lapply(masculinos, last))

#Criar tabela com uma colunda para a faixa etária e outra para o número de casos femininos e mudar coluna da faixa etária para os nomes adequados
casos_femininos_idade_invertido <- as.data.frame(t(casos_femininos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_idade_invertido)[2] <- "Feminino"
casos_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Criar tabela com uma colunda para a faixa etária e outra para o número de casos masculinos e mudar coluna da faixa etária para os nomes adequados
casos_masculinos_idade_invertido <- as.data.frame(t(casos_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_idade_invertido)[2] <- "Masculino"
casos_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Juntar as 3 tabelas que criámos
casos_fem_masc <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")

#Fazer melt para poder fazer gráfico 
casos_fem_masc_melt <- melt(casos_fem_masc, id.vars = "Idade")
names(casos_fem_masc_melt) = c("Idade", "Genero", "Casos")

###Fazer gráfico de barras com idade no eixo do x, o número de casos no eixo do y e o género em cada barra
casos_fem_masc_grafico <- ggplot(casos_fem_masc_melt, aes(x = Idade, y = Casos, fill = Genero)) +
  geom_col(position = "dodge", aes(text = paste('Faixa Etária: ', Idade,
                              '<br>Género: ', Genero,
                              '<br>Nº de Casos: ', Casos))) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(casos_fem_masc_melt$Casos + 1000))) +
  theme_classic() +
  labs(x = "",
       y = "Mortes") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text( size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=10,
                                   color = "black")) +
  guides(fill=guide_legend(title="Género")) +
  scale_fill_manual(values = c("deeppink3", "lightblue", "grey60"))

#Fazer gráfico interativo
ggplotly(casos_fem_masc_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preserve832bc60b92bfec2d


#### Evolução da Incidência por Faixa Etária

<br>

Neste gráfico de área podemos verificar que as faixas etárias entre os 0 e os 19 anos, apresentaram inicialmente um baixo número de novos casos diários. Nos meses de verão verificou-se um aumento do número de casos, que se tornou mais expressivo a partir de setembro, o que poderá ser explicado pelo regresso dos estudantes às aulas e pelo regresso dos pais aos empregos.

Relativamente às faixas etárias entre os 20 e os 59 anos, estas apresentaram um comportamento semelhante, uma vez que correspondem à população ativa, mantendo-se o número de novos casos diários em torno dos mesmos valores.

Nas faixas etárias a partir dos 60 anos, é possível verificar que no mês de abril houve um maior número de novos casos diários, o que coincidiu com os surtos iniciais da doença, nomeadamente nos lares. De seguida, houve uma diminuição da incidência provavelmente devido a uma crescente preocupação pela proteção dos grupos etários de risco. A partir de setembro, é de notar um novo crescimento no número de casos diários, podendo isto ser justificado por um maior contacto com faixas etárias mais ativas (filhos e netos). 


```r
#Pegando nas tabelas que fizémos por género or faixa etária, como os valores na base de dados são cumulativos, fazemos o valor desse dia menos o valor do dia anterior para obtermos o número de novos casos por dia, por faixa etária por género
femininos_novos <- femininos - lag(femininos)
masculinos_novos <- masculinos - lag(masculinos)

#Criar uma tabela com uma coluna para a data e outras colunas com o número de casos por dia por faixa etária apenas que resultam da soma dos novos casos femininos com os novos casos masculinos
casos_total_tempo <- cbind(covid19pt$data, as.data.frame(femininos_novos + masculinos_novos))

#Como  linha 7 que é a primeira em que há registo dos casos já representa o número de casos nesse dia apenas, adicionámos essa linha à tabela e dar nomes às colunas
casos_total_tempo[7, 2:10] <- femininos[7,] + masculinos[7,]
names(casos_total_tempo)<- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Fazer melt para fazer o gráfico
casos_total_tempo_melt <- melt(casos_total_tempo, id.vars = "Data")
names(casos_total_tempo_melt) = c("Data", "Faixa_Etaria", "Casos")

#Fazer o gráfico de área com a data no eixo do x, o número de casos no eixo do y e a faixa etária em cada linha
casos_total_tempo_grafico <- ggplot(casos_total_tempo_melt, aes(x = Data, y = Casos, fill = Faixa_Etaria)) +
  geom_area(alpha=0.6 , size=.5) +
  labs(x ="", 
       y = "Incidência") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 3, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
casos_total_tempo_grafico_interativo <- ggplotly(casos_total_tempo_grafico) %>% 
  layout(legend = list(x = 1, y = 0))

#Número de casos com média rolante
##Calcular a média rolante
casos_total_tempo_media_rolante <- cbind(casos_total_tempo[7:nrow(casos_total_tempo),1], 
                                         as.data.frame(rollmean(casos_total_tempo[,2:10],k = 7)))
names(casos_total_tempo_media_rolante)[1] = "Data"

##Fazer melt
casos_total_tempo_media_rolante_melt <- melt(casos_total_tempo_media_rolante, id.vars = "Data")
names(casos_total_tempo_media_rolante_melt) = c("Data", "Faixa_Etaria", "Casos")

##Fazer gráfico de linhas
casos_total_tempo_media_rolante_grafico_linhas <- ggplot(casos_total_tempo_media_rolante_melt, 
                                                         aes(x = Data, y = Casos, color = Faixa_Etaria)) +
  geom_point(size=0.1, aes(text = paste('Data: ', Data,
                              '<br>Faixa Etária: ', Faixa_Etaria,
                              '<br>Incidência: ', Casos))) +
  geom_line(size=0.5) +
  labs(title = "Média Rolante (7 dias)",
       x ="", 
       y = "Incidência") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(size = 3, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%b")

##Tornar gráfico interativo
casos_total_tempo_media_rolante_grafico_linhas_interativo <- ggplotly(casos_total_tempo_media_rolante_grafico_linhas, tooltip = "text")
 


browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      casos_total_tempo_grafico_interativo
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      casos_total_tempo_media_rolante_grafico_linhas_interativo
    )
  ))
)
```

preserve92793f444740c705


#### Evolução Taxa de Incidência por Faixa Etária
<br>

Novamente neste gráfico, é possível observar que as faixas etárias dos 0 aos 19 anos apresentaram uma baixa taxa de incidência, mesmo nos primeiros meses da pandemia, tendo-se mantido relativamente estável. Desde setembro, estes valores têm aumentado na sequência do início do período escolar.

No caso das faixas etárias entre os 20 e os 39 anos, inicialmente apresentaram valores de taxa de incidência mais elevados, diminuindo posteriormente mas sempre com alguma instabilidade, não só por pertencerem a uma fração ativa da população, mas também pela maior tendência para frequentar locais com maiores aglomerados de indivíduos. Dos 40 aos 59 anos o comportamento da curva é semelhante, no entanto manteve-se mais ou menos constante durante os meses de Verão, tendo voltado a aumentar a partir de setembro, o que mimetiza o comportamento das faixas etárias anteriores.

Relativamente às faixas etárias dos 60 aos 79 anos, podemos verificar que a taxa de incidência foi principalmente preocupante nos meses iniciais da pandemia, tendo depois estabilizado até meados de setembro, novamente resultante de uma crescente preocupação pelo grupo etário mais suscetível. Da mesma forma, a faixa etária dos 80+ apresentou uma taxa de incidência inicial alarmante, no entanto nunca conseguiu estabilizar da mesma forma, provavelmente decorrente de diversos surtos em lares ao longo do tempo.


```r
#Dados da população de portugal em <https://www.pordata.pt/Portugal/Popula%C3%A7%C3%A3o+residente++m%C3%A9dia+anual+total+e+por+grupo+et%C3%A1rio-10>
pt_0_9 = 433332	+ 461299
pt_10_19 = 507646 + 549033
pt_20_29 = 544575 + 547505
pt_30_39 = 571355 + 679093
pt_40_49 = 792670 + 782555
pt_50_59 = 747581 + 734540
pt_60_69 = 672758 + 620543
pt_70_79 = 544016 + 429107
pt_80_plus = 352218 + 316442

#Fazer tabela com população por faixa etária
populacao_idades_pt <- as.data.frame(c(pt_0_9, pt_10_19, pt_20_29, pt_30_39, pt_40_49, pt_50_59, pt_60_69, pt_70_79, pt_80_plus))

##Repetir valores da população por faixa etária para ficar com mesmo número de linhas da base de dados
populacao_idades_pt_rep <- as.data.frame(t(populacao_idades_pt[rep(seq_len(ncol(populacao_idades_pt)), each = nrow(covid19pt))]))

#Calcular a população de risco subtraindo à população por faixa etária o número de óbitos e o número de casos até ao momento                
populacao_risco_idade <- populacao_idades_pt_rep - femininos - masculinos

#Calcular a taxa de incidência, retirando a coluna das datas
taxa_incidencia_idade <- (casos_total_tempo[,-1]/populacao_risco_idade)*100

#Voltar a adicionar a coluna da data e fazer melt para fazer o gráfico
taxa_incidencia_idade <- cbind(covid19pt$data, taxa_incidencia_idade)
names(taxa_incidencia_idade)[1] = "Data"
taxa_incidencia_idade_melt <- melt(taxa_incidencia_idade, id.vars = "Data")
names(taxa_incidencia_idade_melt) = c("Data", "Faixa_Etaria", "Taxa_Incidencia")

#Fazer gráfico de linhas com data no eixo do x, taxa de incidência no eixo do y e faixa etária nas cores das linhas
ggplot(taxa_incidencia_idade_melt, aes(x = Data, y = Taxa_Incidencia, color = Faixa_Etaria)) +
  geom_line(size = 0.8)+
  facet_grid(taxa_incidencia_idade_melt$Faixa_Etaria)+
  guides(color = FALSE) +
  labs(x = "",
       y ="Taxa de Incidência (%)") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20),
                                    size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%b")
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-13-1.png" width="672" />

#### Taxa de Incidência Cumulativa por Faixa Etária

<br>


Neste gráfico de barras é possível ver que, à data desta análise, a faixa etária com maior prevalência é a dos 80+ anos. Isto pode ser explicado pelo facto de esta faixa etária necessitar de mais cuidados, estando presente em lares (onde têm ocorrido surtos) ou recorrendo a cuidadores, sendo por isso mais suscetíveis à infeção.

A faixa etária dos 20 aos 29 anos é a que apresenta a segunda maior prevalência, sendo notório que este valor vai diminuindo à medida que se progride na faixa etária, com exceção da faixa etária dos 80+, pelo explicado anteriormente. Isto pode dever-se ao aumento do receio da doença com o aumento da idade.

As faixas etárias entre os 9 e os 19 anos são as que têm as prevalências mais baixas, o que pode ser explicado pelo encerramento das escolas que promoveu uma maior proteção das crianças. É ainda prematuro tirar conclusões sobre o impacto que a reabertura das escolas terá na prevalência destas faixas etárias.

Relativamente à evolução da prevalência por faixa etária, em abril é visível que a faixa etária dos 80+ anos era a que tinha, de longe, a maior prevalência. É de notar tabém que desde setembro que todas as faixa etárias sofreram um maior aumento da prevalência comparado com os meses anteriores.


```r
###Somar a tabela dos femininos com a dos masculinos o que vai dar o número de casos até ao momento por idade apenas
casos_total_idade <- as.data.frame(casos_femininos_idade + casos_masculinos_idade)

#Calcular prevalência
prevalencia_idade <- ((as.data.frame(t(casos_total_idade)))*100 / populacao_idades_pt) %>%  
  rownames_to_column(var = "Faixa_Etaria")
prevalencia_idade[,1] = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
names(prevalencia_idade)[2] = "Prevalencia"

#Fazer gráfico com faixa etária no eixo do x e prevalência no eixo do y
prevalencia_idade_grafico <- ggplot(prevalencia_idade, aes(x = Faixa_Etaria, y = Prevalencia)) +
  geom_col(fill = "coral2", width = 0.5, aes(text = paste('Faixa etária: ', Faixa_Etaria,
                                           '<br>Taxa de Incidência Cumulativa (%):', Prevalencia))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))

#Tornar gráfico interativo
prevalencia_idade_grafico_interativo <- ggplotly(prevalencia_idade_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                      "Taxa de Incidência Cumulativa (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
#Prevalência por faixa etária diária

##Cálculo da prevalência
prevalencia_tempo_idade <- cbind(covid19pt$data, ((femininos + masculinos) / populacao_idades_pt_rep)*100)
names(prevalencia_tempo_idade) = c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

##Fazer melt para fazer o gráfico
prevalencia_tempo_idade_melt <- melt(prevalencia_tempo_idade, id.vars = "Data")
names(prevalencia_tempo_idade_melt) = c("Data", "Faixa_Etaria", "Prevalencia")

##Fazer gráfico de linhas com data no eixo do x, prevalência no eixo do y e faixa etária nas cores das linhas
prevalencia_tempo_idade_grafico <- ggplot(prevalencia_tempo_idade_melt, aes(x = Data, y = Prevalencia, color = Faixa_Etaria)) +
  geom_line(size=0.5)+
  geom_point(size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Faixa Etária: ', Faixa_Etaria,
                                         '<br>Taxa de Incidência Cumulativa (%):', Prevalencia))) +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 6)) +
  scale_x_date(breaks = "months", date_labels = "%b")

##Tornar o grafico interativo
prevalencia_tempo_idade_grafico_interativo <- ggplotly(prevalencia_tempo_idade_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Incidência Cumulativa (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 1)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      prevalencia_idade_grafico_interativo
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      prevalencia_tempo_idade_grafico_interativo
    )
  ))
)
```

preserve5213bfacfc3e90c9

#### Evolução da Incidência por Género
<br>

Como podemos observar através deste gráfico, a incidência em ambos os géneros evoluiu de forma relativamente semelhante, sendo que o género feminino apresentou valores ligeiramente superiores. É provável que, nos meses iniciais da pandemia, indivíduos do sexo feminino tenham tido maior contacto direto com esta condição clínica, eventualmente pela profissão que exercem.

Mais uma vez, é de referir que esta análise não tem em consideração o número de indivíduos, tanto femininos como masculinos, no total da população.


```r
#Duas tabelas com casos diários, uma para cada género
femininos_total_novos <- as.data.frame(covid19pt$confirmados_f - lag(covid19pt$confirmados_f))
masculinos_total_novos <- as.data.frame(covid19pt$confirmados_m - lag(covid19pt$confirmados_m))

#Juntar as tabelas e adicionar coluna com data
incidencia_genero <- cbind(covid19pt$data, femininos_total_novos, masculinos_total_novos)
names(incidencia_genero) = c("Data", "Feminino", "Masculino")

#Fazer melt para fazer o gráfico
incidencia_genero_melt <- melt(incidencia_genero, id.vars = "Data")
names(incidencia_genero_melt) = c("Data", "Genero", "Incidencia")

##Fazer gráfico de linhas com data no eixo do x, incidencia no eixo do y e género nas cores das linhas
incidencia_genero_grafico <- ggplot(incidencia_genero_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Género: ', Genero,
                                         '<br>Incidência :', Incidencia))) +
  labs(x = "") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  scale_x_date(breaks = "months", date_labels = "%b")
  
#Tornar gráfico interativo
ggplotly(incidencia_genero_grafico, tooltip = "text" ) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Incidência",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preservec25280a46ef1d404

#### Evolução da Taxa de Incidência por Género

<br>

Com a análise deste gráfico podemos verificar que não existem diferenças acentuadas entre os géneros, mesmo tendo em conta a proporção de indivíduos de cada género.


```r
#Variáveis com a população total, feminina e masculina com base no INE
populacao_pt = 10295909
mulheres_pt = 5435932
homens_pt = 4859977

#Cálculo da incidência criando uma tabbela para o total, outra para mulheres e outra para homens
incidencia_total <- as.data.frame(covid19pt$confirmados_novos/ (populacao_pt - covid19pt$confirmados - covid19pt$obitos))
incidencia_homens <- as.data.frame((covid19pt$confirmados_m - lag(covid19pt$confirmados_m)) 
                                   / (homens_pt - covid19pt$confirmados_m)) 
incidencia_mulheres <- as.data.frame((covid19pt$confirmados_f - lag(covid19pt$confirmados_f)) 
                                     / (mulheres_pt - covid19pt$confirmados_f))

#Remover valores negativos devido a erro na base de dados original em que valor cumulativo do número casos homens e mulheres era 0 e não devia
incidencia_homens[174:175,] <- NA
incidencia_mulheres[174:175,] <- NA

#Criar uma tabela com as 3 tabelas anteriores, adicionando uma coluna com a data e mudar os nomes das colunas
incidencia <- data.frame(covid19pt$data, incidencia_total*100, incidencia_mulheres*100, incidencia_homens*100)
names(incidencia) <- c("Data", "Total", "Mulheres", "Homens")

#Fazer melt para poder fazer gráfico de linhas e dar nome à coluna do género
incidencia_melt <- melt(incidencia, id.vars = "Data")
names(incidencia_melt) <- c("Data", "Genero", "Incidencia")

#Fazer gráfico de linhas com data no eixo do x, a incidencia no eixo do y e o género em cada linha
ggplot(incidencia_melt, aes(x = Data, y = Incidencia, color = Genero)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.4) +
  facet_grid(incidencia_melt$Genero) +
  theme(legend.position = "none") +
  labs(x = "", 
       y ="Taxa de Incidência (%)") +
  theme(axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        strip.text.y = element_text(size = 8, angle = 0)) +
  scale_x_date(breaks = "months", date_labels = "%b")
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-16-1.png" width="672" />

<br>

## **Mortes** {.tabset .tabset-fade .tabset-pills}
***

Neste tópico vamos abordar a mortalidade em valor absoluto (número de mortes), a taxa de mortalidade (número total de mortes/população total) e taxa de letalidade (número total de mortes/população infetada).

<br>

### Mortalidade {.tabset}

#### Relação com a Evolução da Incidência
<br>

A evolução do número de mortes diárias tende a acompanhar a evolução do número de casos diários. Inicialmente, como os casos diagnosticados eram mais graves, os pacientes morriam mais cedo e por essa razão o desfasamento entre os casos e as mortes não era tão acentuado. Com a evolução da pandemia, começou a ser feito um diagnóstico mais precoce, o que aumentou o intervalo entre o diagnóstico e a morte. 

De forma geral, o número de mortes diárias tem vindo a decrescer. À data da análise, podemos verificar que mesmo perante um grande aumento no número de casos diários, o número de mortes não tem acompanhado este aumento. Isto pode dever-se a um maior conhecimento sobre a doença, à maior incidência nas faixas etárias mais jovens e, talvez, uma menor virulência do vírus.


```r
#Fazer tabela com coluna para data e outra para óbitos ocorridos em cada dia
mortes_diarias <- as.data.frame(cbind(covid19pt$data, as.data.frame(covid19pt$obitos - lag(covid19pt$obitos))))
names(mortes_diarias) = c("Data", "Mortes")

#Tabela com coluna para data, outra para casos e outra para mortes
casos_mortes_diarios <- merge(casos_diarios, mortes_diarias, by="Data")

#Fazer melt para poder fazer gráfico de linhas
casos_mortes_diarios_melt <- melt(casos_mortes_diarios, id.vars = "Data")

#Fazer gráfico de linhas com data no eixo do x, número no eixo do y e mortes ou casos em cada linha
ggplot(casos_mortes_diarios_melt, aes(x = Data, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_grid(casos_mortes_diarios_melt$variable, scales = "free_y") +
  guides(color = FALSE) +
  labs(x = "", 
       y = "Número de Pessoas",
       color = "") +
  scale_color_discrete(labels = c("Incidência", "Mortes Diárias")) +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%b")
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-17-1.png" width="672" />


#### Por Faixa Etária e por Género

<br>

Neste gráfico de barras é facilmente visível que até aos 39 anos o número de mortes é quase inexistente. Começa a aumentar de forma mais significativa a partir dos 59 anos (o que confirma que se trata da população de risco), sendo a faixa etária dos 80+ anos aquela que tem, de longe, o maior número de mortes. 

É perceptível também que o género masculino tem maior número de mortes, ao longo de todas as faixas etárias execto na faixa etária dos 80+. Para estes dados serem estatisticamente fiáveis, era necessário ter-se tido em conta a proporção de pessoas confirmadas dos géneros masculino e feminino existentes em cada faixa etária da população. No entanto, sabemos que em Portugal a esperança média de vida é superior nas mulheres do que nos homens, o que pressupõe que existem mais mulheres na faixa etária dos 80+ justificando, assim, um maior número de mortes do género feminino neste grupo etário.


```r
#Selecionar as colunas de obitos feminino para todas as idades e juntá-las numa tabela e fazer o mesmo para o masculino
femininos_mortes <- as.data.frame(covid19pt %>% 
                                    dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))
masculinos_mortes <- as.data.frame(covid19pt %>% 
                                     dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

#Selecionar o valor mais recente de cada coluna de modo a ficar com o número de óbitos até ao momento  para cada faixa etária e para cada género
mortes_femininos_idade <- as.data.frame(lapply(femininos_mortes, last))
mortes_masculinos_idade <- as.data.frame(lapply(masculinos_mortes, last))

#Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos femininos e mudar coluna da  faixa etária para os nomes adequados
mortes_femininos_idade_invertido <- as.data.frame(t(mortes_femininos_idade))%>% 
  rownames_to_column(var = "Idade")
names(mortes_femininos_idade_invertido)[2] <- "Feminino"
mortes_femininos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos masculinos e mudar coluna da faixa etária para os nomes adequados
mortes_masculinos_idade_invertido <- as.data.frame(t(mortes_masculinos_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_masculinos_idade_invertido)[2] <- "Masculino"
mortes_masculinos_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Juntar as 3 tabelas que criámos
mortes_fem_masc <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")

#Fazer melt para poder fazer gráfico 
mortes_fem_masc_melt <- melt(mortes_fem_masc, id.vars = "Idade")
names(mortes_fem_masc_melt) = c("Idade", "Genero", "Mortes")

#Fazer gráfico de barras com idade no eixo do x, o número de óbitos no eixo do y e o género em cada barra
mortes_fem_masc_grafico <- ggplot(mortes_fem_masc_melt, aes(x = Idade, y = Mortes, fill = Genero)) +
  geom_col(position = "dodge", aes(text = paste( 'Faixa Etária: ', Idade,
                                         '<br>Género: ', Genero,
                                         '<br>Nº de mortes :', Mortes))) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(mortes_fem_masc_melt$Mortes + 100))) +
  theme_classic() +
  labs(x = "",
       y = "Mortes") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size=8, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  guides(fill=guide_legend(title="género")) +
  scale_fill_manual(values = c("deeppink3", "lightblue", "grey60"))

#Tornar gráfico interativo
ggplotly(mortes_fem_masc_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Mortes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preserve45c92b196c5f2438


#### Por ARS

<br>

Neste gráfico de barras e no mapa vemos que a ARS com maior número de mortes é a Norte, seguida de Lisboa e Vale do Tejo. É importante realçar que este número é absoluto, logo, não tem em consideração a proporção de infetados de cada ARS.


```r
#Criar tabela com uma coluna para as Regiãoes e outra para o número mais recente total de óbitos e dar nomes
mortos_regioes <- as.data.frame(t(as.data.frame(lapply(covid19pt[,obitos_arsnorte:obitos_madeira], last)))) %>% 
  rownames_to_column(var = "Regioes")
names(mortos_regioes)[2] <- "Mortes"
mortos_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

#Fazer gráfico com Regiãoes no eixo do x e número de mortes no eixo do y
mortos_regioes_grafico <- ggplot(mortos_regioes, aes(x = Regioes, y = Mortes)) +
  geom_col(fill = "salmon1", width = 0.5, aes(text = paste( 'ARS: ', Regioes,
                                         '<br>Nº de mortes :', Mortes))) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 12))
mortos_regioes_grafico_interativo <- ggplotly(mortos_regioes_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Mortes",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))

#mapa
#Da tabela anterior com todas as datas, selecionar apenas o valor mais recente e mudar nome coluna e Regiãoes
incidencia_regioes_ordem <- as.data.frame(t(as.data.frame(lapply(incidencia_regioes[,c(5, 6, 7, 3, 8, 2, 4)], last)))) %>% 
  rownames_to_column(var = "Regiao") 
incidencia_regioes_ordem[,1] <- c("Alentejo", "Algarve", "Açores","Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo")

##Definir intervalos da legenda
bins_3 =  c(0, 25, 50, 100, 500, 1000, Inf)

##Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortos_regioes_ordem <- mortos_regioes[c(4, 5, 6, 2, 7, 1, 3),]

##Definir a palete de cores
pal_3 <- colorBin("YlOrRd", domain = mortos_regioes_ordem[,2], bins = bins_3)

##Definir a legenda quando se passa com o rato por cima
labels_3 <- sprintf(
  "<strong>%s</strong><br/>%g Mortos",
  incidencia_regioes_ordem[,1], mortos_regioes_ordem[,2]
) %>% lapply(htmltools::HTML)

##Fazer o mapa
mortos_regioes_mapa <- leaflet(mapa_pt) %>% 
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
  addLegend(pal = pal_3, values = incidencia_regioes_ordem[,2], opacity = 0.7, title = "Número de Mortes por ARS",
            position = "bottomright")

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      mortos_regioes_mapa
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      mortos_regioes_grafico_interativo
    )
  ))
)
```

preserve80743507e5643cc5


### Taxa de Mortalidade {.tabset}

#### Por Género

<br>

Neste gráfico de barras podemos ver que os homens têm uma taxa de mortalidade superior à das mulheres. Contudo, este gráfico não tem em conta se existem mais pessoas do género masculino infetadas do que do género feminino.


```r
#Cálculo da mortalidade total, feminina e masculina com base no valor mais recente dos óbitos, que é também o mais alto
mortalidade_total <- max(covid19pt$obitos, na.rm = TRUE) / populacao_pt
mortalidade_mulheres <- max(covid19pt$obitos_f, na.rm = TRUE) / mulheres_pt
mortalidade_homens <- max(covid19pt$obitos_m, na.rm =TRUE) / homens_pt

#Criar uma tabela com uma coluna para o género e outra para o valor da mortalidade e dar nome apropriado aos generos
mortalidade <- data.frame(t(data.frame(mortalidade_total, mortalidade_mulheres, mortalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(mortalidade)[2] <- "mortalidade"
mortalidade[, 1] <- c("Total", "Mulheres", "Homens")

#Criar um gráfico de barras com o genero no eixo do x e a mortalidade no eixo do y
ggplot(mortalidade, aes(x = genero, y = mortalidade*100)) +
  geom_col(fill = "salmon1", width = 0.5) +
  coord_cartesian( ylim = c(0, max(mortalidade$mortalidade*106))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "",
       y = "Taxa de Mortalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(mortalidade, digits = 4)), 
            vjust = -0.5, 
            size = 4)
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-20-1.png" width="672" />


#### Por Faixa Etária

Neste gráfico de barras é possível verificar que a taxa de mortalidade aumenta com o aumento da idade. Isto demonstra, mais uma vez, que a idade é um importante fator de risco, especialmente para a faixa etária dos 80+ anos.


```r
#Calcular a taxa de mortalidade para cada faixa etária e adicionar coluna com faixas etárias
mortalidade_idade <- cbind(mortes_femininos_idade_invertido[,1], ((mortes_femininos_idade_invertido[,2] 
                                                                   + mortes_masculinos_idade_invertido[,2])*100/ populacao_idades_pt))
names(mortalidade_idade) = c("Faixa_Etaria", "Taxa_Mortalidade")

#Fazer o gráfico com faixa etária no eixo do x e taxa de mortalidade no eixo do y
mortalidade_idade_grafico <- ggplot(mortalidade_idade, aes(x = Faixa_Etaria, y = Taxa_Mortalidade)) +
  geom_col(fill = "salmon1", width = 0.5, aes(text = paste( 'Faixa Etária: ', Faixa_Etaria,
                                         '<br>Taxa de Mortalidade (%) :', Taxa_Mortalidade))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12),
        axis.text.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"))

#Tornar gráfico interativo
ggplotly(mortalidade_idade_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                     "Taxa de Mortalidade (%)",
                                     rep("&nbsp;", 20),
                                     rep("\n&nbsp;", 2)),
                                   collapse = "")))
```

preservecc5f52eeb2eafcbb

#### Por ARS

<br>

Neste gráfico de barras e mapa, é possível ver que a ARS Lisboa e Vale do Tejo é a que tem a maior taxa de mortalidade seguida da ARS Norte. Por outro lado, é notável o facto de a Madeira ainda não apresentar qualquer caso de morte pela COVID-19.


```r
#Criar tabela com uma coluna para as Regiãoes e outra para a mortalidade mais recente e dar nomes apropriados
mortalidade_regioes <- data.frame(t(as.data.frame(lapply(covid19pt[, obitos_arsnorte:obitos_madeira], last)) *100) 
                                  / populacao_regioes) %>% 
  rownames_to_column(var = "Regiao")
names(mortalidade_regioes)[2] <- "Mortalidade"
mortalidade_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

#Fazer gráfico com Regiãoes no eixo do x e mortaldiade no eixo do y
mortalidade_regioes_grafico <- ggplot(mortalidade_regioes, aes(x = Regiao, y = Mortalidade)) +
  geom_col(fill = "gray", width = 0.5, aes(text = paste( 'ARS: ', Regiao,
                                         '<br>Taxa de Mortalidade (%) :', Mortalidade))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x="") +
  theme(axis.text.y = element_text(size=12))
mortalidade_regioes_grafico_interativo <- ggplotly(mortalidade_regioes_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                     "Taxa de Mortalidade (%)",
                                     rep("&nbsp;", 20),
                                     rep("\n&nbsp;", 2)),
                                   collapse = "")))
#mapa
##Definir intervalos da legenda
bins_4 =  c(0, 0.006, 0.001, 0.015, 0.02, 0.025, 0.03, Inf)

##Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
mortalidade_regioes_ordem <- mortalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

####Definir a palete de cores
pal_4 <- colorBin("YlOrRd", domain = mortalidade_regioes_ordem[,2], bins = bins_4)

##Definir a legenda quando se passa com o rato por cima
labels_4 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Mortalidade",
  mortalidade_regioes_ordem[,1], round(mortalidade_regioes_ordem[,2], digits =  3)
) %>% lapply(htmltools::HTML)

##Fazer o map
mortalidade_regioes_mapa <- leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_4(mortalidade_regioes_ordem[,2]), 
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
  addLegend(pal = pal_4, values = mortalidade_regioes_ordem[,2], opacity = 0.7, title = "Taxa de Mortalidade (%) por ARS",
            position = "bottomright")

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      mortalidade_regioes_mapa
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      mortalidade_regioes_grafico_interativo
    )
  ))
)
```

preserve5516b196282abe22


### Taxa de Letalidade {.tabset .tabset-fade .tabset-dropdown}
#### Por Género

<br>

Com a análise deste gráfico de barras é possível concluir que os homens afetados com COVID-19 tem uma maior probabilidade de morrer, relativamente às mulheres infetadas. Isto pode ser justificado pelo estilo de vida menos saudável adotado por este género (maior percentagem de tabagismo e alcoolismo).


```r
#Cálculo da letalidade total, feminina e masculina com base no valor mais recente dos óbitos,  que é também o mais alto, e no valor mais recente de confirmados
letalidade_total <- max(covid19pt$obitos, na.rm = TRUE) / last(covid19pt$confirmados)
letalidade_mulheres <- max(covid19pt$obitos_f, na.rm = TRUE) / last(covid19pt$confirmados_f)
letalidade_homens <- max(covid19pt$obitos_m, na.rm = TRUE) / last(covid19pt$confirmados_m)

#Criar uma tabela com uma coluna para o género e outra para o valor da letalidade e dar nome apropriado aos generos
letalidade <- data.frame(t(data.frame(letalidade_total, letalidade_mulheres, letalidade_homens))) %>% 
  rownames_to_column(var = "genero")
names(letalidade)[2] <- "letalidade"
letalidade[, 1] <- c("Total", "Mulheres", "Homens")

#Criar um gráfico de barras com o genero no eixo do x e a letalidade no eixo do y
ggplot(letalidade, aes(x = genero, y = letalidade*100)) +
  geom_col(fill = "salmon1", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian( ylim = c(0, max(letalidade$letalidade*100 + 1))) +
  theme_classic() +
  labs(x = "",
       y = "Taxa de Letalidade(%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20), 
                                    size = 15),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 15),
        axis.text.x = element_text(size=12, 
                                   color = "black"),
        axis.text.y = element_text(size=12, color = "black")) +
  geom_text(aes(label = scales::percent(letalidade, digits = 4)), 
            vjust = -0.5, 
            size = 4)
```

<img src="Relatório_Provisorio_files/figure-html/unnamed-chunk-23-1.png" width="672" />


#### Evolução por Género

<br>

No gráfico de linhas podemos ver que, a partir de meados de março, a letalidade sofreu um aumento considerável uma vez que os mais velhos, população mais debilitada, eram dos grupos etários mais afetados, atingindo o seu pico no dia 05/05/2020 com 5.04%. A partir de junho, a letalidade tem vindo a diminuir. Uma das razões que pode explicar este facto é a faixa etária da população infetada, que se alterou, passando dos mais velhos para os mais jovens, que têm menor probabilidade de morrer da doença viral. Para além deste motivo, existem ainda outros fatores a considerar, nomeadamente um maior conhecimento e experiência por parte dos profissionais de saúde, um alargamento do número de testes realizados e ainda uma melhoria da capacidade de resposta do sistema de saúde.


```r
#Calcular letalidade toal, para mulheres e para homens, criando uma tabela para cada com uma coluna para a  data e outra para os valores da letalidade para cada dia
letalidade_tempo_total <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos / covid19pt$confirmados)*100))
letalidade_tempo_mulheres <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos_f / covid19pt$confirmados_f)*100))
letalidade_tempo_homens <- cbind(covid19pt$data, as.data.frame((covid19pt$obitos_m / covid19pt$confirmados_m)*100))

#Juntar as 3 tabelas numa são mudando os nomes de cada coluna
letalidade_tempo_total_mulheres <- merge(letalidade_tempo_total, letalidade_tempo_mulheres, by ="covid19pt$data")
letalidade_tempo <- merge(letalidade_tempo_total_mulheres, letalidade_tempo_homens, by="covid19pt$data")
names(letalidade_tempo) <- c("Data", "Total", "Feminino", "Masculino")

#Fazer o melt para poder faze um gráfico de linhas
letalidade_tempo_melt <- melt(letalidade_tempo, id.vars = "Data")
names(letalidade_tempo_melt) <- c("Data", "Genero", "Letalidade")

#Fazer gráfico de linhas com a data no eixo do x, a letalidade no eixo do y e o género em cada linha
letalidade_tempo_grafico <- ggplot(letalidade_tempo_melt, aes(x = Data, y = Letalidade, color = Genero)) +
  geom_line(size=0.5) +
  geom_point(size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Género :', Genero,
                                         '<br>Taxa de Letalidade (%) :', Letalidade ))) +
  labs(x = "",
       y = "Taxa de Letalidade (%)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
ggplotly(letalidade_tempo_grafico, add_tracey = "Taxa de Letalidade (%)", tooltip = "text") %>% 
  layout(legend = list(x = 1, y = 0))
```

preservedc7dd30019ed1774


#### Por Faixa Etária e Género

<br>

Pela análise do gráfico de pontos, é possível ver que as faixas etárias entre os 0 e os 49 anos não apresentam diferenças significativas nos valores da letalidade entre si, e o mesmo acontece para os géneros dentro de cada faixa etária.

Os homens tendem a apresentar uma taxa de letalidade superior a partir da faixa etária dos 50 aos 59 anos, que se vai acentuando com a idade, o que, de acordo com estudos recentes, pode estar relacionado com a pré-existência de maior número de co-morbilidades, com um maior consumo de bebidas alcoólicas e maior tendência para o tabagismo, diferenças na resposta imunitária, questões fisiológicas relacionadas com o recetor ACE2 viral (que pode ter uma expressão diferente entre géneros) e, ainda, diferenças comportamentais, nomeadamente uma menor tendência para recorrer a serviços médicos.

É ainda relevante mencionar que a taxa de letalidade aumenta com o aumento da idade. Pode dever-se ao facto de serem faixas etárias mais debilitadas e, normalmente, com maior número de doenças concomitantes. Isto é particularmente notório na faixa etária dos 80+.


```r
#Criar tabela com uma colunda para a faixa etária e outra para o número de casos total e mudar coluna da faixa etária para os nomes adequados
casos_total_idade_invertido <- as.data.frame(t(casos_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(casos_total_idade_invertido)[2] <- "Total"
casos_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Tabela com número de casos confirmados por faixa etária por género
casos_genero_idade <- merge(casos_femininos_idade_invertido, casos_masculinos_idade_invertido, by = "Idade")
casos_genero_idade_total <-  merge(casos_genero_idade, casos_total_idade_invertido, by = "Idade")

#Somar a tabela dos femininos com a dos masculinos o que vai dar o número de óbitos até ao momento por idade apenas
mortes_total_idade <- as.data.frame(mortes_femininos_idade + mortes_masculinos_idade)

#Criar tabela com uma colunda para a faixa etária e outra para o número de óbitos total e mudar coluna da faixa etária para os nomes adequados
mortes_total_idade_invertido <- as.data.frame(t(mortes_total_idade)) %>% 
  rownames_to_column(var = "Idade")
names(mortes_total_idade_invertido)[2] <- "Total"
mortes_total_idade_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Tabela com número de óbitos por faixa etária por género
mortes_genero_idade <- merge(mortes_femininos_idade_invertido, mortes_masculinos_idade_invertido, by = "Idade")
mortes_genero_idade_total <-  merge(mortes_genero_idade, mortes_total_idade_invertido, by = "Idade")

#Criar tabela com uma coluna com as faixas etárioas e outra com a letalidade e dar nomes às colunas
letalidade_genero_idade <- cbind(casos_femininos_idade_invertido[,1], (mortes_genero_idade_total[,2:4]/casos_genero_idade_total[,2:4]))
names(letalidade_genero_idade) <- c("Idade", "Feminino", "Masculino", "Total")

#Fazer melt para poder fazer gráfico
letalidade_genero_idade_melt <- melt(letalidade_genero_idade, id.vars = "Idade")

#Fazer gráfico com idade no eixo do x, letalidade no eixo do y e faixa etária em cada linha
letalidade_genero_idade_grafico <- ggplot(letalidade_genero_idade_melt, aes(x = Idade, y = value*100, color = variable, 
                                                                tooltip = round(value*100, digits = 2), data_id = value)) +
  geom_point_interactive() +
  labs(x ="",
       y = "Taxa de Letalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 20,), 
                                    size = 12),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 12)) +
  scale_color_discrete(name= "Genéro")
        
#Animar o gráfico 
girafe(code = print(letalidade_genero_idade_grafico),
       options = list(
         opts_zoom(max = 2),
         opts_hover(css = "fill:black;"),
         opts_sizing(rescale = TRUE, width = 0.8)
       ))
```

preserve69d14a4a12058f4b


#### Evolução por Faixa Etária

<br>

Neste gráfico de linhas podemos verificar que nas faixas etárias entre os 0 e os 59 anos, a taxa de letalidade tem-se mantido relativamente constante e baixa, uma vez que não constituem a fração da população de maior risco.

Relativamente às faixas etárias entre os 60 e os 80+ (faixas etárias de risco), a taxa de letalidade foi aumentando progressivamente até junho dada a falta de conhecimento inicial desta doença viral e a ocorrência de surtos em lares. Posteriormente, as taxas de letalidade têm vindo a diminuir pois, para além de um maior conhecimento, existe uma melhor capacidade de resposta médica. No entanto, prevemos um aumento destas taxas devido ao recente aumento no número de casos nestas faixas etárias.


```r
#Tabela com o número de mortes total diários por faixa etária
total_mortes_novos <- femininos_mortes + masculinos_mortes

#Tabela com o número de casos totais diários por faixa etária
total_casos_novos <- femininos + masculinos

#Tabela com uma coluna com a data e outras com cada faixa etária onde tem o valor da letalidade total para cada dia e  dar nomes às colunas
letalidade_tempo_idade <- cbind(covid19pt$data, total_mortes_novos/total_casos_novos)
names(letalidade_tempo_idade) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

#Fazer melt para fazer o gráfico
letalidade_tempo_idade_melt <- melt(letalidade_tempo_idade, id.vars = "Data")

#Fazer o gráfico
letalidade_tempo_idade_grafico <- ggplot(letalidade_tempo_idade_melt, aes(x = Data, y = value*100, color = variable)) +
  geom_line(size = 0.5) +
  geom_point(size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Faixa Etária :', variable,
                                         '<br>Taxa de Letalidade (%) :', value*100 ))) +
  labs(x ="",
       y = "Taxa de Letalidade (%)") +
  theme(axis.title.x = element_text(margin = margin(t = 20 ,b = 20), 
                                    size = 15),
        axis.title.y = element_text(margin = margin(r = 20, l = 20), 
                                    size = 15),
        strip.text.y = element_text(angle = 0),
        legend.title = element_blank()) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar o gráfico interativo
ggplotly(letalidade_tempo_idade_grafico, tooltip = "text")
```

preservede8ae8cd17801f5a


#### Por ARS

<br>

Ao interpretarmos este gráfico de barras e este mapa, verificamos que os Açores têm uma taxa de letalidade muito elevada o que pode dever-se a uma débil estrutura médica e/ou uma maior incidência na população mais envelhecida. O mesmo também se verifica na ARS do Centro, ainda que com uma taxa de letalidade ligeiramente menor, que, às razões anteriormente mencionadas, pode-se acrescentar o facto de existir uma elevada discrepância de acessibilidade aos serviços hospitalares entre os concelhos do litoral e do interior.

Relativamente à ARS do Norte, esta apresenta a terceira taxa de letalidade mais elevada, provavelmente resultante dos primeiros surtos em Portugal terem ocorrido nesta ARS. 

Nas ARSs do Algarve e do Alentejo, era de esperar que as taxas de letalidade fossem mais elevadas. No entanto isto não se verifica, possivelmente pelo facto de terem sido atingidas faixas etárias mais jovens que estabelecem mais contacto entre si, enquanto que as faixas etárias mais velhas se encontram em zonas rurais e, por isso, mais isoladas.

A taxa de letalidade na ARS de Lisboa e Vale do Tejo parece ser baixa dada a prevalência que existe nesta ARS. Isto demonstra que possui um serviço de saúde eficaz com uma boa resposta médica.

Na Madeira a taxa de letalidade é zero, uma vez que não se verificaram óbitos nesta região. Este valor pode dever-se, por um lado à qualidade do serviço médico prestado ou, por outro lado, a uma menor incidência nas faixas etárias com maior risco de morte.


```r
#Fazer uma tabela com uma coluna com a Região e outra com a letalidade para cada Região e dar nomes adequados
letalidade_regioes <- data.frame(t(as.data.frame(lapply(covid19pt[, obitos_arsnorte:obitos_madeira], last))*100 
                                   / (as.data.frame(lapply(covid19pt[, confirmados_arsnorte:confirmados_madeira], last))))) %>% 
  rownames_to_column(var = "Regiao")
names(letalidade_regioes)[2] <- "Letalidade"
letalidade_regioes[, 1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

#Fazer o gráfico com a Região no eixo do x e a letalidade no eixo do y
letalidade_regioes_grafico <- ggplot(letalidade_regioes, aes(x = Regiao, y = Letalidade)) +
  geom_col(fill = "gray", width = 0.5, aes(text = paste( 'ARS: ', Regiao,
                                         '<br>Taxa de Letalidade (%) :', Letalidade))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = "") +
  theme(axis.text.y = element_text(size=12))
letalidade_regioes_grafico_interativo <- ggplotly(letalidade_regioes_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                     "Taxa de Letalidade (%)",
                                     rep("&nbsp;", 20),
                                     rep("\n&nbsp;", 2)),
                                   collapse = "")))
#mapa
##Definir intervalos da legenda
bins_5 =  c(0, 1, 2, 3, 4, 5, Inf)

##Ordenar a tabela feita anteriormente para ficar com a mesma ordem que os poligonos do mapa
letalidade_regioes_ordem <- letalidade_regioes[c(4, 5, 6, 2, 7, 1, 3),]

##Definir a palete de cores
pal_5 <- colorBin("YlOrRd", domain = letalidade_regioes_ordem[,2], bins = bins_5)

##Definir a legenda quando se passa com o rato por cima
labels_5 <- sprintf(
  "<strong>%s</strong><br/>%g&#x25 Letalidade",
  letalidade_regioes_ordem[,1], round(letalidade_regioes_ordem[,2], digits =  2)
) %>% lapply(htmltools::HTML)

##Fazer o mapa
letalidade_regioes_mapa <- leaflet(mapa_pt) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  addPolygons(
    fillColor = ~pal_5(letalidade_regioes_ordem[,2]), 
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
  addLegend(pal = pal_5, values = letalidade_regioes_ordem[,2], opacity = 0.7, title = "Taxa de Letalidade (%) por ARS",
            position = "bottomright")

browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      letalidade_regioes_mapa
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      letalidade_regioes_grafico_interativo
    )
  ))
)
```

preservef51ca3ea1161c859


#### Evolução por ARS

<br>

Segundo este gráfico de linhas, podemos ver que as ARSs do Norte, do Centro, de Lisboa e Vale do Tejo e do Algarve apresentam um comportamento semelhante embora em diferentes proporções, com os valores máximos entre maio e junho. A partir de junho os valores têm vindo a decrescer pelas razões mencionadas anteriormente, especialmente a ARS do Algarve em que essa diminuição foi mais acentuada, eventualmente devido à maior incidência em indivíduos mais jovens.

No caso dos Açores ocorreu um aumento abrupto da taxa de letalidade, em que no seu pico cerca de 11% das pessoas infetadas morriam. Desde junho esta taxa tem vindo a diminuir, possivelmente devido à estratégia implementada da realização de uma testagem em grande escala, detetando assim os casos assintomáticos e menos graves.

A ARS do Alentejo apresenta um comportamento atípico. Ocorre um pico no dia 03/04/2020 que pode representar a morte de pessoas infetadas no início da pandemia, no entanto é invulgar que tenham todas ocorrido no mesmo dia. De seguida a taxa de letalidade regressa a 0%, o que demonstra que houve um lapso no registo dos óbitos. Entre maio e junho a taxa de letalidade manteve-se em *platô*, sendo isso também incomum. De seguida, apresentou um aumento abrupto que pode ser explicado pelo aumento da incidência, possivelmente em faixas etárias mais velhas. Desde julho, tem demonstrado um comportamento semelhante às outras ARSs, com uma descida da taxa de letalidade.

Na Madeira a taxa de letalidade é zero, uma vez que não se verificaram óbitos nesta região.


```r
#Criar tabela com uma coluna para data e outras colunas uma para cada região tendo lá os valores da letalidade diária e dar nomes às colunas
letalidade_regioes_tempo <- cbind(covid19pt$data, as.data.frame((covid19pt[,49:55]/covid19pt[,4:10]))*100)
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", 
                                     "Algarve", "Açores", "Madeira")

#Fazer melt para poder fazer o gráfico
letalidade_regioes_tempo_melt <- melt(letalidade_regioes_tempo, id.vars = "Data")
names(letalidade_regioes_tempo_melt) = c("Data", "Regiao", "Letalidade")

#Fazer o gráfico de linhas com data no eixo do x, letalidade no eixo do y e regiao em cada linha
letalidade_regioes_tempo_grafico <- ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = Letalidade, 
                                                                              color = Regiao)) +
  geom_line(size=0.5) +
  geom_point(size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>ARS :', Regiao,
                                         '<br>Taxa de Letalidade (%) :', Letalidade ))) +
  labs(x = "",
       y ="Letalidade (%)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
ggplotly(letalidade_regioes_tempo_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Letalidade (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preserveced69a70c8b64579


## **Internamentos** {.tabset .tabset-fade .tabset-pills}
***

Um fator muito importante a ter em conta no decorrer de uma pandemia é, não só o número de pessoas infetadas, mas também o número de pessoas que necessita de acompanhamento hospitalar para impedir a falência do sistema de saúde.

<br>

### Evolução do Número de Internados

<br>

Ao realizar a interpretação deste gráfico verificámos que no início da pandemia o número de internados aumentou exponencialmente, tendo atingido o valor máximo no dia 16/04/2020. É de salientar que nesta fase ainda não existia o conhecimento necessário para atuar medicamente sobre os casos mais graves da doença e, também por essa falta de conhecimento, recorria-se a um internamento mais precoce. Para além disso, uma das faixas etárias mais atingidas foi a dos 80+ anos, levando, naturalmente, a um maior número de complicações. Desde esse pico até meados de junho, o número de internados foi diminuindo, seguindo-se um ligeiro aumento até julho com nova descida até setembro. A partir de setembro o número de internados tem vindo a acompanhar o aumento no número de casos.

Relativamenta aos internados na unidade de cuidados intensivos (UCI), houve um aumento concordante com o aumento dos internados, embora em muito menor escala. A partir do dia 7/04/2020 foi diminuindo gradualmente tendo voltado a aumentar também no início de setembro.


```r
#Fazer melt das colunas data, internados e internados UCI para ter número de internados em cada dia
internados <- melt(covid19pt[,c(1, 15, 88, 16)], id.vars = "data")

#Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados$variable)[levels(internados$variable)=="internados"] <- "Internados Total"
levels(internados$variable)[levels(internados$variable)=="internados_uci"] <- "Internados UCI"
levels(internados$variable)[levels(internados$variable)=="internados_enfermaria"] <- "Internados Enfermaria"
names(internados) = c("data", "tipo_internamento", "internados")

#Fazer gráfico de linhas com data no eixo do x, número de internados no eixo do y e tipo de internamento nas linhas
internados_grafico <- ggplot(internados, aes(x = data, y =internados, color = tipo_internamento)) +
  geom_line(size = 0.5) +
  geom_point(size=0.1, aes(text = paste( 'Data: ', data,
                                         '<br>Tipo de Internamento :', tipo_internamento,
                                         '<br>Nº de Internados :', internados ))) +
  labs(x = "", color = "") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 7)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
ggplotly(internados_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Número de Internados",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preserveeb01411376b4a0cc


### Evolução da Taxa de Internamento

<br>

No início da pandemia apenas os casos graves procuravam ajuda médica e, por isso, eram os únicos a serem diagnosticados, o que justifica a taxa de internamento de 100%. De seguida houve uma descida abrupta, explicada pelo aumento do diagnóstico de casos menos graves. No final de março houve um ligeiro aumento na taxa de internados. Posteriormente houve uma diminuição desta taxa, possivelmente devido a um maior conhecimento desta condição médica assim como uma maior incidência nas faixas etárias mais jovens. A partir de setembro a taxa apresenta um comportamento ligeiramente crescente, que pode dever-se a um novo aumento do número de casos nas faixas etárias mais velhas (gráfico da Evolução da Incidência por Faixa Etária).


```r
#Fazer melt para ter tabela com coluna da data, coluna do tipo de internamento e coluna com percentagem de internados que são os internados a dividir pelos confirmados e dar nomes às colunas
internados_confirmados <- melt((cbind(covid19pt$data, (as.data.frame(lapply(covid19pt[,c(15, 88, 16)], 
                                                                       function(x) {(x/covid19pt[, 3])*100}))))), id.vars = "covid19pt$data")
names(internados_confirmados) <- c("data", "internados", "percentagem")

#Mudar o nome do conteúdo da coluna dos internados para internados ou internados UCI
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados"] <- " Internados Total"
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados.1"] <- "Internados Enfermaria"
levels(internados_confirmados$internados)[levels(internados_confirmados$internados)=="confirmados.2"] <- "Internados UCI"

#Fazer gráfico de linhas com data no eixo do x, percentagem internados no eixo do y e tipo de internamento em cada linha
internados_confirmados_grafico <- ggplot(internados_confirmados, aes(x = data, y = percentagem, color = internados)) +
  geom_line(size = 0.5) +
  geom_point(size=0.1, aes(text = paste( 'Data: ', data,
                                         '<br>Tipo de Internamento :', internados,
                                         '<br>Taxa de Internamento (%) :', percentagem ))) +
  labs(x = "", color = "") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 7)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
ggplotly(internados_confirmados_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Internamento (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
```

preserve4a1807abb9405d5a

### Rácio entre Internados UCI e Internados

<br>

Neste gráfico podemos ver que no início da pandemia, uma vez que apenas os casos mais graves eram diagnosticados, cerca de 30% dos internados encontravam-se na unidade de cuidados intensivos. 
Desde o início de abril este rácio tem vindo a diminuir. Teve um aumento ligeiro no dia 16 de maio, provavelmente resultante dos 52 surtos ativos em lares de idosos existentes de 1 a 7 de maio. No início do verão houve um novo aumento provavelmente também resultante dos surtos ativos em estruturas residenciais para idosos.

Posteriormente o rácio foi diminuindo de forma considerável até dia 10 de agosto, onde atingiu o seu mínimo com cerca de 7%. De seguida, o rácio tem vindo novamente a aumentar devido a um aumento da incidência na população mais velha (gráfico Evolução da Incidência por Faixa Etária).


```r
#Fazer o rácio
racio_internados <- cbind(covid19pt$data, as.data.frame((covid19pt$internados_uci/covid19pt$internados)*100))
names(racio_internados) = c("Data", "Racio")

#Fazer o gráfico
racio_internados_grafico <- ggplot(racio_internados, aes(x = Data, y = Racio)) + 
  geom_line(color = "tomato4", size=0.5) +
  geom_point(color = "tomato4", size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Rácio (%) :', Racio))) +
  labs(x = "",
       y = "Rácio (%)") +
  scale_x_date(breaks = "months", date_labels = "%b")


#Tornar interativo
ggplotly(racio_internados_grafico, tooltip = "text")
```

preserve6e3bd05eddca7d22

<br>

## **Recuperados** {.tabset .tabset-fade .tabset-pills}
***

É importante analisar-se o número de recuperados para se poder avaliar de que forma o tipo de tratamento e/ou cuidados aplicados estão a ser eficazes.

<br>

### Evolução da Taxa de Recuperados

<br>

Nos meses de março e abril a taxa de recuperados era bastante baixa. Isto pode ser justificado pelo facto de apenas os casos graves terem sido diagnosticados nesta fase. A partir de abril, a taxa de recuperados sofreu um crescimento devido ao aumento do número de diagnósticos de casos menos graves. No dia 24/05/2020, a taxa de recuperados teve uma subida abrupta, que pode dever-se a um lapso no registo dos recuperados. No dia 25/08/2020 ocorreu o pico. Desde então, a taxa tem vindo a diminuir de forma gradual, o que pode ser devido ao grande aumento do número de casos verificados em setembro, os quais ainda não tiveram tempo para recuperar.


```r
#Criar tabela com coluna para data e outra coluna para a percentagem de recuperados em cada dia e dar nomes às colunas
recuperados <- cbind(covid19pt$data, as.data.frame((covid19pt$recuperados / covid19pt$confirmados)*100))
names(recuperados) <- c("Data", "Recuperados")

#Fazer gráfico de linhas com data no eixo do x e percentagem recuperados no eixo y
recuperados_grafico <- ggplot(recuperados, aes(x = Data, y = Recuperados)) +
  geom_line(color = "salmon1", size = 0.5) +
  geom_point(color = "salmon1", size=0.1, aes(text = paste( 'Data: ', Data,
                                         '<br>Taxa de Recuperados (%) :', Recuperados))) +
  labs(x = "",
       y = "Taxa de Recuperados (%)") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  scale_x_date(breaks = "months", date_labels = "%b")

#Tornar gráfico interativo
ggplotly(recuperados_grafico, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Taxa de Recuperados (%)",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")))
```

preserve8572c87541ed37b0
