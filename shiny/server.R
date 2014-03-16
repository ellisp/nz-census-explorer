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
  
  
  #----------------------define datasetInput-----------------------
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
  
  #---------------------define common variables for labels, scales etc
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
    rv <- NULL
    if(input$EqualCoords){
      rv <- c(variables[variables$name == input$Variable, "Min"], variables[variables$name == input$Variable, "Max"])
      if(input$logs){
        rv[1] <- rv[2] / 100 # otherwise it is zero and crashes the programme
      }
    } else {
      if(input$ForcedZero & input$theTabs != "Barchart"){
        rv[1] <- 0
        rv[2] <- max(c(datasetInput()$x, datasetInput()$y), na.rm=TRUE) * 1.05
      }
    } 
    return(rv)
  })
  
  #--------------------Define reactive variabls used just for scatter plot-----------------------------
    
  ExtraLine1 <- reactive({
    if(input$regression){
      geom_smooth(method="lm")
    } else{
      NULL
    }    
    })
    
  ExtraLine2 <- reactive({
    if(input$equality){
      geom_abline(slope=1, xintercept=0, color="white") 
    } else{
      NULL
    }
  })
     
  ExtraCoords <- reactive({
    if(input$EqualCoords | input$ForcedZero){
      coord_equal() 
    } else{
      NULL
    }
  })
  #---------------------define the scatter plot-----------------------         
    p1 <- reactive({
      ggplot(datasetInput(), aes(x=x, y=y, colour=x, label=NAME)) +
        ExtraLine1() + 
        ExtraLine2() +
        ExtraCoords() +
        geom_text(family="Comic Sans MS", size=input$TextSize) +
        scale_x_continuous(paste0("\n", input$variablex), 
                           label=get(labels()[1]), trans=Trans(),
                           limits=range_variable()) +
        scale_y_continuous(paste0(input$variabley, "\n"), 
                           label=get(labels()[2]), trans=Trans(),
                           limits=range_variable()) +
        scale_color_gradientn(wrap(paste(input$variablex, input$Variable, sep="\n"), 15), colours=c("red", "grey50", "blue"), 
                              label=get(labels()[1]), trans=Trans(),
                              limits=range_variable()) +
        theme_grey(base_family=MyFont)  +
        
        ggtitle(input$Variable) +
        theme(legend.title.align=0.5)    
      
    })
   
  output$motion <- renderPlot({              
      print(p1())
  })
  
  #-------------------Define "bar" plot ------------------
    p2 <- reactive({
      ggplot(datasetInput(), aes(y=NAME, x=x, colour=x)) +
        geom_point(aes(size=x)) +
        geom_segment(aes(yend=as.numeric(NAME), y=as.numeric(NAME)), xend=0) +
        labs(y="") +
        scale_x_continuous(paste0("\n", input$variablex), label=get(labels()[1]), trans=Trans(),
                           limits=range_variable()) +
        scale_color_gradientn("", colours=c("red", "grey50", "blue"), 
                              label=get(labels()[1]), trans=Trans(),
                              limits=range_variable()) +
        scale_size_continuous(wrap(paste(input$variablex, input$Variable, sep="\n"), 15), label=get(labels()[1]), trans=Trans(),
                              limits=range_variable()) +
        theme_grey(base_family=MyFont) +
        guides(colour = guide_legend(order = 2, reverse=TRUE), size = guide_legend(order = 1, reverse=TRUE)) +
        ggtitle(input$Variable) +
        theme(legend.title.align=0.5)  
    })
       
  output$bar <- renderPlot({  
    print(p2())
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