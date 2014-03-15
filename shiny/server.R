#=================header material==========================
library(shiny)
library(election2011)
library(RColorBrewer)
library(extrafont)

MyFont <- "Verdana"    
load("census_combined.rda")
load("variables.rda")

# For debugging
# input <- data.frame(variablex="Maori", variabley="Maori", "GeoType" = "Territorial Authority", Variable = "Total People")

# TODO - fix labels on scales (comma, dollar, percent, etc)

#===================begin shinyServer()===============================
shinyServer(function(input, output) {
  
  
  #----------------------define datasetInput and variable labels-----------------------
  datasetInput <- reactive({
    tmp <- census_combined[ , c("NAME", "Year", "Geography_Type", "variable",
                                as.character(input$variablex), 
                                as.character(input$variabley))]
    names(tmp) <- c("NAME", "Year", "Geography_Type", "variable", "x", "y")
    tmp <- subset(tmp, Year==input$Year & 
                    Geography_Type == input$GeoType &
                    variable == input$Variable)
    tmp$NAME <- with(tmp, factor(NAME, levels=NAME[order(x)]))
    return(tmp)
  })
  
  labels <- reactive({
    c(variables[variables$name==input$Variable, "label"],
    variables[variables$name==input$Variable, "label"])
  })
  
  Trans <- reactive({
    if(input$logs){
      "log10"
    } else {
      "identity"
    }
  })
  
  range_variable <- reactive({
    if(input$EqualCoords){
      rv <- c(variables[variables$name == input$Variable, "Min"], variables[variables$name == input$Variable, "Max"])
      if(input$logs){
        rv[1] <- rv[2] / 100 # otherwise it is zero and crashes the programme
      }
    } else{
      rv <- NULL
    }
      return(rv)
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
    
      p <- ggplot(datasetInput(), aes(x=x, y=y, colour=x, label=NAME)) +
              ExtraLine1 + 
              ExtraLine2 +
              geom_text(family="Comic Sans MS") +
              scale_x_continuous(paste0("\n", input$variablex), 
                                 label=get(labels()[1]), trans=Trans(),
                                 limits=range_variable()) +
              scale_y_continuous(paste0(input$variabley, "\n"), 
                                 label=get(labels()[2]), trans=Trans(),
                                 limits=range_variable()) +
              scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), 
                                    label=get(labels()[1]), trans=Trans()) +
              theme_grey(base_family=MyFont)  +
              ExtraCoords +
              ggtitle(input$Variable)
                
      print(p)
  })
  
  
  #-------------------Define "bar" plot ------------------
  output$bar <- renderPlot({
    
    p <- ggplot(datasetInput(), aes(y=NAME, x=x, colour=x)) +
      geom_point(aes(size=x)) +
      geom_segment(aes(yend=as.numeric(NAME), y=as.numeric(NAME)), xend=0) +
      labs(y="") +
      scale_x_continuous(paste0("\n", input$variablex), label=get(labels()[1]), trans=Trans()) +
      scale_color_gradientn(input$variablex, colours=c("red", "grey50", "blue"), 
                            label=get(labels()[1]), trans=Trans()) +
      scale_size_continuous(input$variablex, label=get(labels()[1]), trans=Trans()) +
      theme_grey(base_family=MyFont) +
      theme(legend.position="none") +
      ggtitle(input$Variable)
      
    
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