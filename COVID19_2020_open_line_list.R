library(tidyverse)
library(Amelia)
library(printr)

# https://github.com/midas-network/COVID-19/wiki/Data-catalog#interventions
# repo com origem dos dados: https://github.com/beoutbreakprepared/nCoV2019

## ler dados
dados<- read_csv2("dados/2.preparada/COVID19_2020_open_line_list.csv",
                  na = c("", "NA"))

## dimensão da tabela
dim(dados)
## variaveis na tabela
names(dados)

## selecionadas as variaveis que podem ter mais interesse
dados.sel<-dados %>% select("ID","age","sex","city","province","country",
                            "date_onset_symptoms","date_admission_hospital",
                            "date_confirmation","symptoms","reported_market_exposure",
                            "chronic_disease_binary","outcome","country_new"
                            )

## percentagem de missings
missmap(dados.sel)

## contar linhas com sintomas
dados.sel %>% filter(!is.na(symptoms)) %>% count()

## filtrar apenas paises UE
dados.sel.ue<-dados.sel %>% filter(country %in% c("Austria","Belgium","Bulgaria",
                                                  "Croatia","Czech Republic","Estonia",
                                                  "Finland","France","Germany","Greece",
                                                  "Ireland","Italy","Latvia","Lithuania",
                                                  "Luxembourg","Malta","Netherlands",
                                                  "Poland","Portugal","Slovakia",
                                                  "Slovenia","Spain","Sweden",
                                                  "United Kingdom"))

## numero de casos por pais europeu com sintomas
dados.sel.ue %>% filter(!is.na(symptoms)) %>% count(country)

                                                  
                                                  
                                                  
                                                