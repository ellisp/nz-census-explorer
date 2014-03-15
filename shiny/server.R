library(shiny)
library(election2011)
library(RColorBrewer)
library(extrafont)

MyFont <- "Verdana"    
load("IncomeTA_median.rda")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  datasetInput <- reactive({
    tmp <- IncomeTA_median[ , c("NAME", "Year",
                                as.character(input$variablex), 
                                as.character(input$variabley), 
                                as.character(input$variablex),
                                as.character(input$variabley))]
    names(tmp) <- c("TA", "Year", "x", "y", "colour", "size")
    tmp <- subset(tmp, Year==input$Year & 
                    TA != "Area Outside Territorial Authority")
    tmp$TA <- with(tmp, factor(TA, levels=TA[order(x)]))
    return(tmp)
  })
    
  output$motion <- renderPlot({
    if(input$regression){
      ExtraLine1 <- geom_smooth(method="lm")
    } else{
      ExtraLine1 <- NULL
    }
    
    if(input$equality){
      ExtraLine2 <- geom_abline(slope=1, xintercept=0, color="white") 
    } else{
      ExtraLine2 <- NULL
    } 
    
    
      p <- ggplot(datasetInput(), aes(x=x, y=y, size=size, colour=colour, label=TA)) +
              ExtraLine1 + 
              ExtraLine2 +
              geom_text(family="Comic Sans MS") +
              scale_x_continuous(input$variablex, label=dollar, limits=input$xRange) +
              scale_y_continuous(input$variabley, label=dollar, limits=input$yRange) +
              scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), 
                                    label=dollar, breaks=
                                      round(seq(input$xRange[2], input$xRange[1], length.out=10), -4)) +
              scale_size_continuous(input$variabley, label=dollar, breaks=
                                      round(seq(input$yRange[2], input$yRange[1], length.out=10), -4)) +
              theme_grey(base_family=MyFont)  +
              coord_equal() +
              guides(colour = guide_legend(order = 2), 
                  size = guide_legend(order = 1))
                
      print(p)
  })
  
  output$bar <- renderPlot({
    
    p <- ggplot(datasetInput(), aes(y=TA, x=x, colour=x)) +
      geom_point(aes(size=x)) +
      geom_segment(aes(yend=as.numeric(TA), y=as.numeric(TA)), xend=0) +
      labs(y="") +
      scale_x_continuous(input$variablex, label=dollar, limits=input$xRange) +
      scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), label=dollar) +
      scale_size_continuous(input$variablex, label=dollar) +
      theme_grey(base_family=MyFont) +
      theme(legend.position="none")
      
    
    print(p)
  })
  

  
})