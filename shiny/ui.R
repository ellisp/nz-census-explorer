library(shiny)
library(election2011)


load("ethnicities.rda")
load("variables.rda")

# Define UI 
shinyUI(pageWithSidebar(
  
    # Application title
  headerPanel("New Zealand census data by ethnicity and district"),
  
  #===============================Draw side panel================
  sidebarPanel(
    
    h3('Data choices'),
    
    
    #----------------------options that apply to all-------------------------
    h4("Basic options"),
    selectInput("Year", "Census year:",
                 c(2013, 2006, 2001)),
    
    selectInput("GeoType", "Geographical area type:",
                choices=list("Territorial Authority", "Regional Council"),
                selected="Territorial Authority"),
    
    selectInput("Variable", "Census variable:",
                choices=as.list(variables$name),
                selected=variables$name[1]),
    
    
    
    #-----------------------conditional options------------------------
    h4("Choice of ethnicities and how they are shown"),
    conditionalPanel(
      condition = "input.theTabs != 'Barchart'",
      selectInput("variabley", "Vertical axis:",
                choices=as.list(ethnicities),
                selected="European")
      ), 
    
        
    selectInput("variablex", "Horizontal axis and colour:",
                  choices=as.list(ethnicities),
                  selected="Maori"), 
    
    h4("Plot options to help you make comparisons"),
    conditionalPanel(
      condition = "input.theTabs != 'Data table'",
      checkboxInput("EqualCoords", "Force scales for all plots with this variable to be the same", TRUE)
    ),
    conditionalPanel(
      condition = "input.Variable != 'Proportion with no education' &&
                    input.Variable != 'Proportion with higher education' &&
                    input.Variable != 'Unemployment Rate Percent' &&
                    input.theTabs != 'Data table'" ,
      checkboxInput("logs", "Logarithmic scale", FALSE)
    ),
    conditionalPanel(
      condition = "input.theTabs === 'Scatter plot'",
      h5("Special options for scatter plots"),
      checkboxInput("ForcedZero", "Force both axes to start at zero and be equally spaced", TRUE),
      checkboxInput("equality", "Show equality line", TRUE),
      checkboxInput("regression", "Show regression line", FALSE),
      sliderInput("TextSize", "Size of the names of areas:", 1, 10, 5)
    )
    
    ),
  
  #===================Draw main panel================
  mainPanel(
    tabsetPanel(id="theTabs",
                tabPanel("Compare two ethnicities - plot", value="Scatter plot", plotOutput("motion", height="700px")), 
                tabPanel("Compare two ethnicities - table", value="Data table", dataTableOutput("Data")),
                tabPanel("One ethnicity - plot", value="Barchart", plotOutput("bar", height="700px"))
                
                
    )
  ) 
  
  )
)