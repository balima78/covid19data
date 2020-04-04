#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

paises<-read.csv2("data/popEU.csv")
#df_eu<-read_csv("data/df_eu.csv")
source("files/fxs_sir.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("S I R model - graphical representation (COMPSTATS exercise)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("pais", "select country", paises$countries, 
                    selected = "Portugal"),
        h6("European Union countries + United Kingdom"),
        selectInput("pais2", "select 2nd country", paises$countries, 
                    selected = "Spain"),
        br(),
        sliderInput("maxt",
                    "maximal number of days:",
                    min = 100,
                    max = 1000,
                    value = 200),
        h6("maximum number of days to consider"),
        br(),
        
        h3("modelling parameters"),
        sliderInput("dias",
                    "skip initial days for modeling:",
                    min = 0,
                    max = 15,
                    value = 0),
        h6("initial days on data skiped to fit new parameters"),
        br(),
        submitButton("Apply changes", icon("refresh")),
        h6("All the code and data is available from:"),
        a(href="https://github.com/balima78/covid19data/tree/master/sir_shiny", "repo")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("All the data used here are available from:"),
        a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports",
          "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE"),
        h5("Only data from March are used, updated to 2020-03-31. For each country, data is filtered for days after first recovered"),
        br(),
         plotOutput("distPlot"),
         h4("model parameters:"),
         verbatimTextOutput("pars1"), 
         
         h3("Model fitted with data"),
         plotOutput("fitPlot"),
        p("In ", strong("bold "), "are represented observed data of", 
          strong("S"), ", ", strong("I"), "and ", strong("R")),
         h4("model parameters:"),
         verbatimTextOutput("pars2"),
        
        h3("Model fitted with data for 2nd country"),
        plotOutput("fitPlot2"),
        h4("model parameters for 2nd country:"),
        verbatimTextOutput("pars22")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     # generate out table with inputs from ui.R
     maxtime<-input$maxt
     pp<-input$pais
     r<-1
     
     out <- ode(y = parSIR(pais = pp, rec = r)$state,
                times = seq(0,maxtime,by=1),
                func = SIR,
                parms = parSIR(pais = pp, rec = r)$pars)
      
      # draw plot with SIR curves
      ggsir(data = as.data.frame(out), pais = pp, rec = r)
   })
   
   output$pars1 <- renderPrint({
     pp<-input$pais
     r<-1
     
     parSIR(pais = pp, rec = r)$pars
   })

   ################ plot for model 1 fited with data
   #####################################
   newdata<-reactive({
     pp<-input$pais
     r<-1
     dd<-input$dias
     
     as.data.frame(parSIR(pais = pp, rec = r)$data %>% 
                     select(time,S,I,R) %>% 
                     filter(time>dd))
   }) 
   
   # funcao para modelar o custo
   Objective <- reactive({
     maxtime<-input$maxt
     pp<-input$pais
     r<-1
     
     pars<-parSIR(pais = pp, rec = r)$pars
     tout <- seq(0,maxtime,by=1) ## output times
     
     function(x, parset = names(x)) {
     pars[parset] <- x
     
     out <- ode(y = parSIR(pais = pp, rec = r)$state, 
                times = tout, 
                func = SIR, 
                parms = pars)
     ## Model cost
     return(modCost(obs = newdata(), 
                    model = out))
     }
   })

   # novos parametros
   Fit <- reactive({
     pp<-input$pais
     r<-1
     
     pars<-parSIR(pais = pp, rec = r)$pars
     
     modFit(p = c(beta = pars$beta, 
                       gama = pars$gama),
                 f = Objective())
   })
   
   # dados fited
   out2 <- reactive({
     maxtime<-input$maxt
     pp<-input$pais
     r<-1
     
     tout <- seq(0,maxtime,by=1) ## output times
     
     ode(y = parSIR(pais = pp, rec = r)$state,
               times = tout, 
               func = SIR, 
               parms = Fit()$par)
   })
   
   #############################
   
   output$fitPlot <- renderPlot({
     pp<-input$pais
     dd<-input$dias
     r<-1
     
     ggsir(data = as.data.frame(out2()), pais = pp, rec = dd, tipo = 2) + 
       geom_point(data = parSIR(pais = pp, rec = r)$data, 
                  aes(x=time, y=S), color = "black") +
       geom_point(data = parSIR(pais = pp, rec = r)$data, 
                  aes(x=time, y=R), color = "green") +
       geom_point(data = parSIR(pais = pp, rec = r)$data, 
                  aes(x=time, y=I), color = "red")
     })
   
   output$pars2 <- renderPrint({
     Fit()$par
     })
   

   ################ plot for model 2 fited with data
   #####################################
   newdata2<-reactive({
     pp2<-input$pais2
     r<-1
     dd<-input$dias
     
     as.data.frame(parSIR(pais = pp2, rec = r)$data %>% 
                     select(time,S,I,R) %>% 
                     filter(time>dd))
   }) 
   
   # funcao para modelar o custo
   Objective2 <- reactive({
     maxtime<-input$maxt
     pp2<-input$pais2
     r<-1
     
     pars<-parSIR(pais = pp2, rec = r)$pars
     tout <- seq(0,maxtime,by=1) ## output times
     
     function(x, parset = names(x)) {
       pars[parset] <- x
       
       out <- ode(y = parSIR(pais = pp2, rec = r)$state, 
                  times = tout, 
                  func = SIR, 
                  parms = pars)
       ## Model cost
       return(modCost(obs = newdata2(), 
                      model = out))
     }
   })
   
   # novos parametros
   Fit2 <- reactive({
     pp2<-input$pais2
     r<-1
     
     pars<-parSIR(pais = pp2, rec = r)$pars
     
     modFit(p = c(beta = pars$beta, 
                  gama = pars$gama),
            f = Objective2())
   })
   
   # dados fited
   out22 <- reactive({
     maxtime<-input$maxt
     pp2<-input$pais2
     r<-1
     
     tout <- seq(0,maxtime,by=1) ## output times
     
     ode(y = parSIR(pais = pp2, rec = r)$state,
         times = tout, 
         func = SIR, 
         parms = Fit2()$par)
   })
   #############################
   
   output$fitPlot2 <- renderPlot({
     pp2<-input$pais2
     dd<-input$dias
     r<-1
     
     ggsir(data = as.data.frame(out22()), pais = pp2, rec = dd, tipo = 2) + 
       geom_point(data = parSIR(pais = pp2, rec = dd)$data, 
                  aes(x=time, y=S), color = "black") +
       geom_point(data = parSIR(pais = pp2, rec = dd)$data, 
                  aes(x=time, y=R), color = "green") +
       geom_point(data = parSIR(pais = pp2, rec = dd)$data, 
                  aes(x=time, y=I), color = "red")
   })
   
   output$pars22 <- renderPrint({
     Fit2()$par
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

