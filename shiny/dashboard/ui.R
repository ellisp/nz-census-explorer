#=======================header material for nz-census-explorer dashboard app=================
library(shiny)

load("../data/Regions.rda")
load("../data/TAs.rda")

# Define UI 
shinyUI(pageWithSidebar(
  
    # Application title
  headerPanel("New Zealand census data by ethnicity and district - dashboards"),
  
  #===============================Draw side panel================
  sidebarPanel(
        
    #----------------------options that apply to all-------------------------
    h4("Basic options"),
    selectInput("Year", "Census year:",
                 c(2013, 2006, 2001)),
    
    selectInput("GeoType", "Geographical area type:",
                choices=list("Territorial Authority", "Regional Council"),
                selected="Regional Council"),
    
    conditionalPanel(
      condition = "input.GeoType === 'Territorial Authority'",
      selectInput("area1", "First Territorial Authority:",
                  choices=as.list(TAs),
                  selected=TAs[1]),
      selectInput("area2", "Comparison Territorial Authority:",
                  choices=as.list(TAs),
                  selected=TAs[2])
    ),
    
    conditionalPanel(
      condition = "input.GeoType === 'Regional Council'",
      selectInput("area3", "First Regional Council:",
                  choices=as.list(Regions),
                  selected=Regions[1]),
      selectInput("area4", "Comparison Regional Council:",
                  choices=as.list(Regions),
                  selected=Regions[2])
    )
    
    ),
  
  #===================Draw main panel================
  mainPanel(
    plotOutput("dash", height="700px")
    )
  ) 
  )