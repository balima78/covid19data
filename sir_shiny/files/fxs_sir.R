
library(tidyverse)
library(deSolve)
library(FME)

df_eu<-read_csv("data/df_eu.csv")

## função com parametros iniciais para SIR a partir de tabela de dados

parSIR<-function(data = df_eu, pais = "Portugal", 
                 check = FALSE,
                 dias = 0,
                 rec = 0){
  
  require(rlang)
  
  if(check == FALSE){
  data<-data %>% filter(countries == {{pais}}) %>% 
    select(dia,I,R,S, population) %>% 
    filter(R>rec, dia > dias) 
  } else if (check == TRUE) {
    data<-data %>% filter(countries == {{pais}}) %>% 
    mutate(I = I/population,
           R = R/population,
           S = S/population) %>% 
    select(dia,I,R,S, population) %>% 
    filter(R>rec, dia > dias)
    }
  
  data<-data %>% mutate(time = dia - data$dia[1])
  
  # listar parametros iniciais
  N = max(data$population)
  S0 = data$S[1]
  I0 = data$I[1]
  R0 = data$R[1]
  S1 = data$S[2]
  I1 = data$I[2]
  R1 = data$R[2]
  S0_ = S1-S0
  I0_ = I1-I0
  R0_ = R1-R0
  beta0 = -(S0_*N)/(I0*S0)
  gama0 = R0_/I0
  
  list(data = data,
       N = N,
       state = c(S = S0, I = I0,R = R0),
       pars = list(beta = beta0, gama = gama0))
}



# # função SIR com equações diferenciais
# SIR <- function(time = seq(0,maxtime,by=1), 
#                 state = parSIR(pais = pp, check = checkID,  rec = r)$state, 
#                 pars = parSIR(pais = pp, check = checkID,  rec = r)$pars, 
#                 N = parSIR(pais = pp)$N) { # returns rate of change
#   with(as.list(c(state, pars)), {
#     dS <- -beta*S*I/N
#     dI <- beta *S*I/N - gama*I
#     dR <- gama*I
#     return(list(c(dS, dI,dR)))
#   })
# }


### plot SIR
ggsir<-function(data = as.data.frame(out), pais = pp, rec = r, tipo = 1){
  
  ggplot(data, aes(time, S, color = "Susceptible")) + geom_line() +
    geom_line(aes(y = I, color = "Infected")) +
    geom_line(aes(y = R, color = "Recovered")) + 
    scale_color_manual(values = c("Susceptible" = "Black", "Infected" = "Red", "Recovered" = "Green")) +
    ggtitle(paste("SIR model for", pais)) +
    xlab(if(tipo == 1){
      paste("days after first recover")} else {
        paste("modelled days, after skipping",rec, "days")}
    ) + 
    ylab("population") + 
    theme_bw() + theme(legend.title = element_blank())
}

# maxtime<-200
# pp<-"Spain"
# r<-0
# ###### differential equations ########
# 
# out <- ode(y = parSIR(pais = pp, rec = r)$state,
#            times = seq(0,maxtime,by=1),
#            func = SIR,
#            parms = parSIR(pais = pp, rec = r)$pars)
# 
# ggsir() + geom_point(data = parSIR(pais = pp, rec = r)$data, aes(x=time, y=S)) +
#   geom_point(data = parSIR(pais = pp, rec = r)$data, aes(x=time, y=R), color = "green") +
#   geom_point(data = parSIR(pais = pp, rec = r)$data, aes(x=time, y=I), color = "red")
# # 
# # ################
# # par(las=1,tcl=0.5,mgp=c(3,0.8,0),bty="l")
# # matplot(out[,1], out[,-1], type = "l", lty = 1:3, lwd = 2,
# #         col = 1:3, xlab = "time, days", ylab = "")
# # legend("topright", c("S", "I", "R"),
# #        lty = 1:3, lwd = 2,col=1:4)
# # minor.tick(2,2,0.7)
# # minor.tick(10,10,0.5)
# # 
# # 
# #data
# df_eu %>% filter(countries == pp, R > 20) %>%
#   select(dia,I,R,S)
# 
# parSIR(rec=r)$data$S[2]
# 
# df_eu %>% count(countries)
