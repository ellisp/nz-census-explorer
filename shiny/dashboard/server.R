#=================header material for nz-census-explorer dashboard Shiny app==========================
library(shiny)
require(extrafont)
library(ggplot2)
library(scales)
library(mbie)

MyFont <- "Verdana"    
load("../data/census_m.rda")
load("../data/variables.rda")

# input <- data.frame(area1="Rodney", area2="Whangarei", GeoType = "Territorial Authority", Year=2013)
# input <- data.frame(area1="Auckland", area2="Wellington", GeoType = "Regional Council", Year=2013)


#===================begin shinyServer()===============================
shinyServer(function(input, output) {
  
  InAreas <- reactive({
    tmp <- c(as.character(input$area1), as.character(input$area2), as.character(input$area3), as.character(input$area4))
    if(input$GeoType == "Regional Council"){
      tmp <- tmp[3:4]
    } else {
      tmp <- tmp[1:2]
    }
  })
  
  census2 <- reactive({
                  census_m[census_m$Year==input$Year &
                             census_m$NAME %in% InAreas() &
                             as.character(census_m$Geography_Type) == input$GeoType, ]
                  })
    
  p3 <- reactive({
            ggplot(census2(), aes(x=NAME, fill=Ethnicity, weight=value)) +
                    geom_bar(position="dodge") +
                    facet_wrap(~variable, scales="free_y", ncol=2) +
                    scale_y_continuous("", label=comma) +
                    labs(x="") +
                    theme_bw(15, base_family=MyFont) +
                    scale_fill_brewer(palette="Set2") +
                    theme(panel.margin = unit(2, "lines"))  
  })
  
  
  output$dash <- renderPlot({  
    print(p3())
  })
}
)