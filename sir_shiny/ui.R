library(shiny)
library(ggplot2)
library(plotly)

paises<-read.csv2("data/popEU.csv")
#df_eu<-read_csv("data/df_eu.csv")
source("files/fxs_sir.R")

fluidPage(
  
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
                  max = 365,
                  value = 200),
      h6("maximum number of days to consider"),
      br(),
      ####################################################
      h3("modelling parameters"),
      sliderInput("dias",
                  "skip initial days for modeling:",
                  min = 0,
                  max = 15,
                  value = 0),
      h6("initial days on data skiped to fit new parameters"),
      br(),
      checkboxInput("checkID", "per population", FALSE),
      h6("Select to use data as proportion of country population"),
      br(),
      submitButton("Apply changes", icon("refresh")),
      h6("All the code and data is available from:"),
      a(href="https://github.com/balima78/covid19data/tree/master/sir_shiny", "my repo"),
      h5("powered by Bruno A Lima")
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
      plotlyOutput("fitPlot"),
      p("In ", strong("bold "), "are represented observed data of", 
        strong("S"), ", ", strong("I"), "and ", strong("R")),
      p("You can zoom in the plot to see how the real data fit the predictive curve."),
      h4("model parameters:"),
      verbatimTextOutput("pars2"),
      
      h3("Model fitted with data for 2nd country"),
      plotlyOutput("fitPlot2"),
      h4("model parameters for 2nd country:"),
      verbatimTextOutput("pars22")
    )
  )
)