library(shiny)
library(election2011)


load("ethnicities.rda")
load("variables.rda")

# Define UI 
shinyUI(pageWithSidebar(
  
    # Application title
  headerPanel("New Zealand median houshold income by ethnicity and district"),
  
  #===============================Draw side panel================
  sidebarPanel(
    
    h3('Data options:'),
    
    #----------------------options that apply to all-------------------------
    radioButtons("Year", "Census year:",
                 c(2013, 2006, 2001)),
    
    selectInput("GeoType", "Geographical area type:",
                choices=list("Territorial Authority", "Regional Council"),
                selected="Territorial Authority"),
    
    selectInput("Variable", "Census variable:",
                choices=as.list(variables),
                selected=variables[1]),
    
    HTML("<hr>"),
    
    #-----------------------conditional options------------------------
    conditionalPanel(
      condition = "input.theTabs != 'Barchart'",
      selectInput("variabley", "Vertical axis and size:",
                choices=as.list(ethnicities),
                selected="European")
      ), 
    
        
    selectInput("variablex", "Horizontal axis and colour:",
                  choices=as.list(ethnicities),
                  selected="Maori"), 
    
     
    
    conditionalPanel(
      condition = "input.theTabs === 'Scatter plot'",
    
      checkboxInput("EqualCoords", "Force both scales to be equal?", TRUE),
      checkboxInput("equality", "Show equality line", TRUE),
      checkboxInput("regression", "Show regression line", FALSE)    
    )    
    ),
  
  #===================Draw main panel================
  mainPanel(
    tabsetPanel(id="theTabs",
                tabPanel("Scatter plot", plotOutput("motion", height="700px")), 
                tabPanel("Barchart", plotOutput("bar", height="700px")),
                tabPanel("Data table", dataTableOutput("Data"))
                
    )
  ) 
  
  )
)