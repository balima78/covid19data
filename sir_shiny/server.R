


function(input, output) {
  
  # inputs da app
  pp <- reactive({ input$pais })
  ck <- reactive({ input$checkID })
  maxtime <- reactive({ input$maxt })
  
  
  # SIR function with diferencial equations 
  SIR <- function(time = seq(0,maxtime(),by=1), 
                  state = parSIR(pais = pp(), check = ck(),  rec = 0)$state, 
                  pars = parSIR(pais = pp(), check = ck(),  rec = 0)$pars, 
                  N = parSIR(pais = pp())$N) { # returns rate of change
    with(as.list(c(state, pars)), {
      dS <- -beta*S*I/N
      dI <- beta *S*I/N - gama*I
      dR <- gama*I
      return(list(c(dS, dI,dR)))
    })
  }

  # plot with initial values of beta and gama
  output$distPlot <- renderPlot({
    r<-0
    
    state<-parSIR(pais = pp(), check = ck(), rec = r)$state
    pars<-parSIR(pais = pp(), check = ck(), rec = r)$pars
    
    out <- ode(y = state,
               times = seq(0,maxtime(),by=1),
               func = SIR,
               parms = pars)

    # draw plot with SIR curves
    ggsir(data = as.data.frame(out), pais = pp(), rec = r)
    
  })
  
  # print parameters beta and gama
  output$pars1 <- renderPrint({
    r<-0
    parSIR(pais = pp(), check = ck(),  rec = r)$pars
    
  })
 
  ################ plot for model 1 fited with data 
  dd <- reactive({ input$dias })
  
  # SIR function with diferencial equations 
  SIR1 <- function(time = seq(0,maxtime(),by=1), 
                  state = parSIR(pais = pp(), check = ck(), dias = dd(),  rec = 0)$state, 
                  pars = parSIR(pais = pp(), check = ck(), dias = dd(),  rec = 0)$pars, 
                  N = parSIR(pais = pp())$N) { # returns rate of change
    with(as.list(c(state, pars)), {
      dS <- -beta*S*I/N
      dI <- beta *S*I/N - gama*I
      dR <- gama*I
      return(list(c(dS, dI,dR)))
    })
  }

  newdata<-reactive({
    r<-0
    
    as.data.frame(parSIR(pais = pp(), check = ck(), dias = dd(), rec = r)$data %>% 
                    select(time,S,I,R))
  })
  
  # funcao para modelar o custo
  Objective <- reactive({
    r<-0
    
    pars<-parSIR(pais = pp(), check = ck(), dias = dd(),  rec = r)$pars
    tout <- seq(0,maxtime(),by=1) ## output times
    state<-parSIR(pais = pp(), check = ck(), dias = dd(),  rec = r)$state
    
    function(x, parset = names(x)) {
      pars[parset] <- x
      
      out <- ode(y = state, 
                 times = tout, 
                 func = SIR1, 
                 parms = pars)
      ## Model cost
      return(modCost(obs = newdata(), 
                     model = out))
    }
  })
  
  # new parameters
  Fit <- reactive({
    r<-0
    
    pars<-parSIR(pais = pp(), check = ck(), dias = dd(),  rec = r)$pars
    
    modFit(p = c(beta = pars$beta, 
                 gama = pars$gama),
           f = Objective())
  })
  
  # fited data
  out2 <- reactive({
    r<-0
    
    tout <- seq(0,maxtime(),by=1) ## output times
    state<-parSIR(pais = pp(), check = ck(), dias = dd(),  rec = r)$state
    
    ode(y = state,
        times = tout, 
        func = SIR1, 
        parms = Fit()$par)
  })
  
  # plot results
  output$fitPlot <- renderPlotly({
    r<-0
    
    gp1<-ggsir(data = as.data.frame(out2()), pais = pp(), rec = dd(), tipo = 2) + 
      geom_point(data = newdata(), 
                 aes(x=time, y=S), color = "black") +
      geom_point(data = newdata(), 
                 aes(x=time, y=R), color = "green") +
      geom_point(data = newdata(), 
                 aes(x=time, y=I), color = "red")
    
    ggplotly(gp1)
  })
  
  output$pars2 <- renderPrint({
    Fit()$par
  })
  
  ## model 2nd country ##########
  pp2 <- reactive({ input$pais2 })
  
  # função SIR com equações diferenciais para o segundo pais
  SIR2 <- function(time = seq(0,maxtime(),by=1), 
                  state = parSIR(pais = pp2(), check = ck(),  rec = 0)$state, 
                  pars = parSIR(pais = pp2(), check = ck(),  rec = 0)$pars, 
                  N = parSIR(pais = pp2())$N) { # returns rate of change
    with(as.list(c(state, pars)), {
      dS <- -beta*S*I/N
      dI <- beta *S*I/N - gama*I
      dR <- gama*I
      return(list(c(dS, dI,dR)))
    })
  }
  
  newdata2<-reactive({
    r<-0

    as.data.frame(parSIR(pais = pp2(), check = ck(), dias = dd(), rec = r)$data %>% 
                    select(time,S,I,R))
  }) 
  
  # funcao para modelar o custo
  Objective2 <- reactive({
    r<-0
    
    pars<-parSIR(pais = pp2(), check = ck(), dias = dd(), rec = r)$pars
    tout <- seq(0,maxtime(),by=1) ## output times
    state<-parSIR(pais = pp2(), check = ck(), dias = dd(), rec = r)$state
    
    function(x, parset = names(x)) {
      pars[parset] <- x
      
      out <- ode(y = state, 
                 times = tout, 
                 func = SIR2, 
                 parms = pars)
      ## Model cost
      return(modCost(obs = newdata2(), 
                     model = out))
    }
  })
  
  # novos parametros
  Fit2 <- reactive({
    r<-0
    
    pars<-parSIR(pais = pp2(), check = ck(), dias = dd(), rec = r)$pars
    
    modFit(p = c(beta = pars$beta, 
                 gama = pars$gama),
           f = Objective2())
  })
  
  # dados fited
  out22 <- reactive({
    r<-0
    
    tout <- seq(0,maxtime(),by=1) ## output times
    state<-parSIR(pais = pp2(), check = ck(), dias = dd(), rec = r)$state
    
    ode(y = state,
        times = tout, 
        func = SIR2, 
        parms = Fit2()$par)
  })
  
  output$fitPlot2 <- renderPlotly({
    r<-0
    
    gp2<-ggsir(data = as.data.frame(out22()), pais = pp2(), rec = dd(), tipo = 2) + 
      geom_point(data = newdata2(), 
                 aes(x=time, y=S), color = "black") +
      geom_point(data = newdata2(), 
                 aes(x=time, y=R), color = "green") +
      geom_point(data = newdata2(), 
                 aes(x=time, y=I), color = "red")
    ggplotly(gp2)
  })
  
  output$pars22 <- renderPrint({
    Fit2()$par
  })
}