library(tidyverse)

# ler dados ecdc: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
ecdc20200329 <- read_csv2("dados/2.preparada/COVID-19-geographic-disbtribution-worldwide-2020-03-29.csv") %>% 
  filter(GeoId %in% c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                      "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK")) %>% 
  rename("country" = `Countries and territories`)

# informação demografica de: https://www.worldometers.info/population/countries-in-europe-by-population/
popEU <- read_csv2("dados/2.preparada/popEU.csv")

str(popEU)

# dados ecdc de: 
#ecdc20200317 <- read_csv2("ecdc20200317.csv")

str(ecdc20200318)

# calcular soma comulativa e filtrar resultados a partir de 2020-02-20
dados <- ecdc20200329 %>% arrange(country, DateRep) %>% 
  group_by(GeoId) %>%
  mutate(cum_cases = cumsum(Cases)) %>% 
  filter(DateRep > "2020-02-20") 


# verificar paises sem data de 21-02-2020
c21<-dados %>% filter(date == "2020-02-21") %>% .$geoID
popEU %>% filter(!geoID %in% c21)

# juntar data 21-02-2020 aos paises a que falta
dados<-bind_rows(dados,
             data.frame(
               DateRep = rep(as.Date("2020-02-21"),9),
               Day = rep(21,9),
               Month = rep(2,9),
               Year = rep(2020,9),
               Cases = rep(0,9),
               Deaths = rep(0,9),
               country = c("Portugal","Bulgaria","Cyprus","Hungary","Latvia","Malta","Poland","Slovakia","Slovenia"),
               GeoId = c("PT","BG","CY","HU","LV","MT","PL","SK","SI"),
               cum_cases = rep(0,9)
               )
)


dados<-dados %>% left_join(popEU %>% select(GeoId,pop1m, landArea, density)) %>% 
  mutate(cum_cases_m = round(cum_cases/pop1m,3),
         cum_cases_a = round(cum_cases/landArea,3),
         cum_cases_d = round(cum_cases/density,3))

# coluna (dia) com numeração dos dias a partir de 10 casos
dados10<-dados %>% select(DateRep,GeoId, cum_cases) %>% 
  filter(cum_cases >=10) %>% arrange(GeoId,DateRep) %>% 
  group_by(GeoId) %>% mutate(dia = row_number()) %>% ungroup()
# join a dados
dados<-dados %>% left_join(dados10 %>% select(DateRep,GeoId, dia), by=c("DateRep","GeoId"))

# coluna (ordem) com rank diario de casos por milhao de habitantes
dadosOrd<-dados %>% arrange(DateRep,desc(cum_cases_m)) %>% 
  group_by(DateRep) %>% mutate(ordem2 = rank(desc(cum_cases_m), ties.method = "min"),
                            ordem = row_number()) %>% ungroup()


library(ggbump)
# selecionar as colunas a utilizar
df<-dadosOrd %>% select(DateRep,ordem, GeoId)
ggplot(df, aes(DateRep, ordem, color = GeoId)) +
  geom_point(size = 2) +
  geom_text(data = df %>% filter(DateRep == min(DateRep)),
            aes(x = DateRep - .5, label = GeoId), size = 3, hjust = 1) +
  geom_text(data = df %>% filter(DateRep == max(DateRep)),
            aes(x = DateRep + .5, label = GeoId), size = 3, hjust = 0) +
  geom_bump(size = 2, smooth = 8) + theme_bw() +
  # scale_x_continuous(limits = c(2010.6, 2013.4),
  #                    breaks = seq(2011, 2013, 1)) +
  #theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Rank for cases / million inhabitants",
       x = NULL) +
  scale_y_reverse(breaks = seq(1,28, 1)) +
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6"))
  
  

library(plotly)

# casos por milhao de habitantes
g1<-ggplot(dados, aes(DateRep,cum_cases_m, color = GeoId)) + geom_point() + geom_line() +
  theme_bw() + 
  #scale_color_grey() + 
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                                     "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                                          values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                                     "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "cum Cases / million inhabitants", color = "Pais")

  ggplotly(g1)

g2<-ggplot(dados, aes(DateRep,cum_cases_m, color = GeoId)) + geom_point() + geom_line() +  scale_y_continuous(trans='log10') +
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "log10 transform - cum Cases / million inhabitants", color = "Pais") 

ggplotly(g2)


# casos por Area
g3<-ggplot(dados, aes(DateRep,cum_cases_a, color = GeoId)) + geom_point() + geom_line() +
  theme_bw() + 
  #scale_color_grey() + 
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "cum Cases / Land Area", color = "Pais")

ggplotly(g3)

g4<-ggplot(dados, aes(DateRep,cum_cases_a, color = GeoId)) + geom_point() + geom_line() +  scale_y_continuous(trans='log10') +
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "log10 transform - cum Cases / Land Area", color = "Pais")

ggplotly(g4)


# casos por densidade populacional
g5<-ggplot(dados, aes(DateRep,cum_cases_d, color = GeoId)) + geom_point() + geom_line() +
  theme_bw() + 
  #scale_color_grey() + 
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "lcum Cases / density", color = "Pais")

ggplotly(g5)

g6<-ggplot(dados, aes(DateRep,cum_cases_d, color = GeoId)) + geom_point() + geom_line() +  scale_y_continuous(trans='log10') +
  scale_color_manual(breaks = c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
                                "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK"), 
                     values = c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6",
                                "#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "log10 transform - cum Cases / density", color = "Pais")

ggplotly(g6)

a<-c("#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","red","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6","#A9A6A6")
 
a == ifelse(unique(dados$GeoId) == "PT","red","#A9A6A6") 


length(a)
length(ifelse(unique(dados$GeoId) == "PT","red","#A9A6A6"))


# casos incidentes por casos acumulados
dados.new<-dados %>% group_by(GeoId) %>% 
  mutate(Cases.1 = lag(Cases, order_by = DateRep),
         Cases.2 = lag(Cases, n=2, order_by = DateRep),
         Cases.3 = lag(Cases, n=3, order_by = DateRep),
         Cases.4 = lag(Cases, n=4, order_by = DateRep),
         Cases.5 = lag(Cases, n=5, order_by = DateRep)) %>% 
  ungroup() %>% mutate(newCases = Cases + Cases.1 + Cases.2 + Cases.3 + Cases.4 + Cases.5,
                       avgCases = rowMeans(.[,c("Cases.1","Cases.2","Cases.3","Cases.4","Cases.5")]))

glag<-ggplot(dados.new, aes(cum_cases, newCases, color = country)) + 
  geom_line() +
  scale_color_manual(values = ifelse(unique(dados$country) == "Portugal","red","#A9A6A6")) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()

library(plotly)
ggplotly(glag)

