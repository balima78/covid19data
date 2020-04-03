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
   titlePanel("S I R model - graphical representation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("pais", "select country", paises$countries, selected = "Portugal"),
        h6("European Union countries + United Kingdom"),
        br(),
        sliderInput("rec",
                    "Number of recovers:",
                    min = 0,
                    max = 100,
                    value = 10),
        h6("number of recovers to define day 0"),
        br(),
        sliderInput("maxt",
                    "maximal number of days:",
                    min = 100,
                    max = 1000,
                    value = 200),
        h6("maximum number of days to consider"),
        br(),
        sliderInput("dias",
                    "skip days for modeling:",
                    min = 0,
                    max = 15,
                    value = 0),
        h6("initial days on data skiped to fit new parameters"),
        br(),
        submitButton("Apply changes", icon("refresh")),
        h6("All the code and data is available from:"),
        a(href="https://github.com/balima78/covid19data", "repo")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         h4("model parameters:"),
         verbatimTextOutput("pars1"), 
         
         h3("Model fitted with data"),
         plotOutput("fitPlot"),
         h4("model parameters:"),
         verbatimTextOutput("pars2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     # generate out table with inputs from ui.R
     maxtime<-input$maxt
     pp<-input$pais
     r<-input$rec
     
     out <- ode(y = parSIR(pais = pp, rec = r)$state,
                times = seq(0,maxtime,by=1),
                func = SIR,
                parms = parSIR(pais = pp, rec = r)$pars)
      
      # draw plot with SIR curves
      ggsir(data = as.data.frame(out), pais = pp, rec = r)
   })
   
   output$pars1 <- renderPrint({
     pp<-input$pais
     r<-input$rec
     
     parSIR(pais = pp, rec = r)$pars
   })

   ################ plot for model fited with data
   #####################################
   newdata<-reactive({
     pp<-input$pais
     r<-input$rec
     dd<-input$dias
     
     as.data.frame(parSIR(pais = pp, rec = r)$data %>% 
                     select(time,S,I,R) %>% 
                     filter(time>dd))
   }) 
   
   # funcao para modelar o custo
   Objective <- reactive({
     maxtime<-input$maxt
     pp<-input$pais
     r<-input$rec
     
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
     r<-input$rec
     
     pars<-parSIR(pais = pp, rec = r)$pars
     
     modFit(p = c(beta = pars$beta, 
                       gama = pars$gama),
                 f = Objective())
   })
   
   # dados fited
   out2 <- reactive({
     maxtime<-input$maxt
     pp<-input$pais
     r<-input$rec
     
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
     
     ggsir(data = as.data.frame(out2()), pais = pp, rec = dd, tipo = 2)
     })
   
   output$pars2 <- renderPrint({
     Fit()$par
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

