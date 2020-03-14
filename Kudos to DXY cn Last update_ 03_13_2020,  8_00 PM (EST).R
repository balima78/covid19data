library(tidyverse)
library(Amelia)
library(printr)

#https://github.com/midas-network/COVID-19/wiki/Data-catalog#interventions
# repo com origem dos dados: https://github.com/midas-network/COVID-19/wiki/Data-catalog#interventions
# Global > nCoV line listings from DXY.cn maintained by NIH Fogarty > Original source here

## ler dados
dados<- read_csv2("dados/2.preparada/Kudos to DXY.cn Last update_ 03_13_2020,  8_00 PM (EST).csv",
                  na = c("", "NA"))

## dimensão da tabela
dim(dados)
## variaveis na tabela
names(dados)

## selecionadas as variaveis que podem ter mais interesse
dados.sel<-dados %>% select("id","country","gender","age","international_traveler",
                            "domestic_traveler",
                            "death","recovered","symptom")

## percentagem de missings
missmap(dados.sel)

## contar linhas com sintomas
dados.sel %>% filter(!is.na(symptom)) %>% count(symptom)

dados.sel<-dados.sel %>%
  mutate(fever = ifelse(grepl("feve", symptom), 1, 
                        ifelse(is.na(symptom),NA,0)),
         cough = ifelse(grepl("cough", symptom), 1, 
                        ifelse(is.na(symptom),NA,0)),
         chills = ifelse(grepl("chills", symptom), 1, 
                         ifelse(is.na(symptom),NA,0)),
         malaise = ifelse(grepl("malaise", symptom), 1, 
                          ifelse(is.na(symptom),NA,0)),
         headache = ifelse(grepl("headache", symptom), 1, 
                           ifelse(is.na(symptom),NA,0)),
         pneumonia = ifelse(grepl("pneumonia", symptom), 1, 
                            ifelse(is.na(symptom),NA,0)),
         sputum = ifelse(grepl("sputum", symptom), 1, 
                         ifelse(is.na(symptom),NA,0)),
         chest.pain = ifelse(grepl("chest pain", symptom), 1, 
                             ifelse(is.na(symptom),NA,0)),
         muscle.pain = ifelse(grepl("muscle pain", symptom), 1, 
                              ifelse(is.na(symptom),NA,0)),
         joint.pain = ifelse(grepl("joint pain", symptom), 1, 
                             ifelse(is.na(symptom),NA,0)),
         diarrhea = ifelse(grepl("diarrhea", symptom), 1, 
                           ifelse(is.na(symptom),NA,0)),
         vomiting = ifelse(grepl("vomiting", symptom), 1, 
                           ifelse(is.na(symptom),NA,0)),
         nausea = ifelse(grepl("nausea", symptom), 1, 
                         ifelse(is.na(symptom),NA,0)),
         itchy.throat = ifelse(grepl("itchy throat", symptom), 1, 
                               ifelse(is.na(symptom),NA,0)),
         fatigue = ifelse(grepl("fatigue", symptom), 1, 
                          ifelse(is.na(symptom),NA,0)),
         breath = ifelse(grepl("breath", symptom), 1, 
                         ifelse(is.na(symptom),NA,0)),
         sneeze = ifelse(grepl("sneeze", symptom), 1, 
                         ifelse(is.na(symptom),NA,0)),
         dyspnea = ifelse(grepl("dyspnea", symptom), 1, 
                          ifelse(is.na(symptom),NA,0)),
         difficulty.breathing = ifelse(grepl("difficulty breathing", symptom), 1, 
                                       ifelse(is.na(symptom),NA,0)),
         cold = ifelse(grepl("cold", symptom), 1, 
                       ifelse(is.na(symptom),NA,0)),
         nose = ifelse(grepl("nose", symptom), 1, 
                       ifelse(is.na(symptom),NA,0)),
         appetite = ifelse(grepl("appetite", symptom), 1, 
                           ifelse(is.na(symptom),NA,0))
  )

## filtrar apenas paises UE
dados.sel.ue<-dados.sel %>% filter(country %in% c("Austria","Belgium","Bulgaria",
                                                  "Croatia","Czech Republic","Estonia",
                                                  "Finland","France","Germany","Greece",
                                                  "Ireland","Italy","Latvia","Lithuania",
                                                  "Luxembourg","Malta","Netherlands",
                                                  "Poland","Portugal","Slovakia",
                                                  "Slovenia","Spain","Sweden",
                                                  "United Kingdom")) 

## listar paises
dados.sel.ue %>% count(country) 

## listar sintomas
dados.sel.ue %>% count(symptom)

