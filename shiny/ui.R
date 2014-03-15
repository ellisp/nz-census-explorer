library(shiny)
library(election2011)

load("IncomeTA_median.rda")
ethnicities <- names(IncomeTA_median)[-(1:2)]

# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("New Zealand median houshold income by ethnicity and district"),
  
  sidebarPanel(
    
    h3('Data options:'),
    
    radioButtons("Year", "Census year:",
                 c(2013, 2006, 2001)),
    
    br(),
    
    conditionalPanel(
      condition = "input.theTabs === 'Scatter plot'",
      selectInput("variabley", "Vertical axis and size (scatter plot only):",
                choices=as.list(ethnicities),
                selected="European"), 
    
    sliderInput("yRange", "Vertical axis range:",
                min = 0, max = 200000, value = c(20000,135000))
    ),
    
    selectInput("variablex", "Horizontal axis and colour:",
                choices=as.list(ethnicities),
                selected="Maori"), 
    
    sliderInput("xRange", "Horizontal axis range:",
                min = 0, max = 200000, value = c(20000,135000)),
      
     br(),
            
    checkboxInput("equality", "Show equality line", TRUE),
    
    checkboxInput("regression", "Show regression line", FALSE)    
    
    ),
    
  mainPanel(
    tabsetPanel(id="theTabs",
      tabPanel("Scatter plot", plotOutput("motion", height="700px")), 
      tabPanel("Barchart", plotOutput("bar", height="700px"))
    )
  ) 
  
  )
)