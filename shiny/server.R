#=================header material==========================
library(shiny)
library(election2011)
library(RColorBrewer)
library(extrafont)

MyFont <- "Verdana"    
load("census_combined.rda")

# For debugging
# input <- data.frame(variablex=="Maori", variabley="Maori", "GeoType" = "Territorial Authority")

# TODO - fix labels on scales (comma, dollar, percent, etc)

#===================begin shinyServer()===============================
shinyServer(function(input, output) {
  
  
  #----------------------define datasetInput-----------------------
  datasetInput <- reactive({
    tmp <- census_combined[ , c("NAME", "Year", "Geography_Type", "variable",
                                as.character(input$variablex), 
                                as.character(input$variabley), 
                                as.character(input$variablex),
                                as.character(input$variabley))]
    names(tmp) <- c("NAME", "Year", "Geography_Type", "variable", "x", "y", "colour", "size")
    tmp <- subset(tmp, Year==input$Year & 
                    Geography_Type == input$GeoType &
                    variable == input$Variable)
    tmp$NAME <- with(tmp, factor(NAME, levels=NAME[order(x)]))
    return(tmp)
  })
  
  #--------------------Define scatter plot-----------------------------
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
    
    if(input$EqualCoords){
      ExtraCoords <- coord_equal() 
    } else{
      ExtraCoords <- NULL
    } 
    
      p <- ggplot(datasetInput(), aes(x=x, y=y, size=size, colour=colour, label=NAME)) +
              ExtraLine1 + 
              ExtraLine2 +
              geom_text(family="Comic Sans MS") +
              scale_x_continuous(input$variablex, label=dollar) +
              scale_y_continuous(input$variabley, label=dollar) +
              scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), 
                                    label=dollar) +
              scale_size_continuous(input$variabley, label=dollar) +
              theme_grey(base_family=MyFont)  +
              ExtraCoords +
              guides(colour = guide_legend(order = 2), 
                  size = guide_legend(order = 1))
                
      print(p)
  })
  
  
  #-------------------Define "bar" plot ------------------
  output$bar <- renderPlot({
    
    p <- ggplot(datasetInput(), aes(y=NAME, x=x, colour=x)) +
      geom_point(aes(size=x)) +
      geom_segment(aes(yend=as.numeric(NAME), y=as.numeric(NAME)), xend=0) +
      labs(y="") +
      scale_x_continuous(input$variablex, label=dollar) +
      scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), label=dollar) +
      scale_size_continuous(input$variablex, label=dollar) +
      theme_grey(base_family=MyFont) +
      theme(legend.position="none")
      
    
    print(p)
  })
  
  #-------------------Define data table----------------------
  
  output$Data <- renderDataTable({
    tmp <- datasetInput()[ , c("NAME", "Year", "x", "y")]
    tmp$x <- format(tmp$x, big.mark=",")
    tmp$y <- format(tmp$y, big.mark=",")
    names(tmp)[3:4] <- c(input$variablex, input$variabley)
    tmp
  })
  

  
})