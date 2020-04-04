
library(rsconnect)
removeAccount("bruno-lima") # ("bioestatisticas")

rsconnect::setAccountInfo(name='balima',
                          token='BF7C6E5582E520E2317B396ACA21E1ED',
                          secret='<SECRET>') # https://www.shinyapps.io/admin/#/dashboard

rsconnect::deployApp("D:/PhD/HEADS/covid19data/sir_shiny")


#getwd()

y

