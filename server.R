#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(ggplot2)
library(timevis)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){ data <- fromJSON(txt="StreamingHistory.json")
    data[,1] <- sub(',.*$','', data[,1])  
    data
    }else{
    data <- fromJSON(txt=inFile$datapath)
    data[,1] <- sub(',.*$','', data[,1])  
    data}
  
    
  })
  
  # output$var1 <- renderUI({
  #   selectInput("var1_output", label = "Choose a variable to display",
  #               choices = c("Artists", "Tracks"))
  # })
  
  output$secondSelection <- renderUI({
    dataAll <- myData()
    dataAll[,3] <- sub(' .*$','', dataAll[,3])
    if(input$var1 == "Artists"){chs <- dataAll$artistName}else{chs <- dataAll$trackName}
    selectInput("Selection", label = "Artist/Track: ", choices = as.character(chs))
  })
  
  output$contents <- renderDT({
    datatable(myData())
})
  output$barplot <- renderPlot({
    dataAll <- myData()
    data.selected <- switch(input$var,
                   "Artists" = dataAll$artistName,
                   "Tracks" = dataAll$trackName,
                   "Time" = dataAll$time
                   )

    cutoff <- input$cutoff

    if(input$var == "Artists" | input$var == "Tracks"){
      rank1 <- as.data.frame(table(data.selected))
      names(rank1) <- c("artistName", "Count")
      rank1 <- rank1[order(rank1$Count, decreasing=TRUE),]
      ggplot(rank1[rank1$Count > cutoff,], aes(x=reorder(artistName,Count), y=Count)) + geom_bar(stat="identity") + coord_flip() + labs(x="Artist", y="Play count")
    }else{
      # observeEvent(input$var == "Time", {showModal(modalDialog(title="important message", "this is important!"))})
      Message <- function(var){if(var == "Time"){showModal(modalDialog(title="Important message", "Nothing for time yet... :(", easyClose = TRUE))}}
      Message(input$var)
    }
    
  })
  
  output$timeline1 <- renderDT({
    dataAll <- myData()
    dataAll[,3] <- sub(' .*$','', dataAll[,3])
    # dataAll
    if(input$var1 == "Artists"){data_band <- dataAll[dataAll$artistName==input$Selection,]}else{data_band <- dataAll[dataAll$trackName==input$Selection,]}
    datatable(data_band)
  
  })
  
  output$timeline2 <- renderTimevis({
    dataAll <- myData()
    dataAll[,3] <- sub(' .*$','', dataAll[,3])
    # dataAll
    if(input$var1 == "Artists"){data_band <- dataAll[dataAll$artistName==input$Selection,]}else{data_band <- dataAll[dataAll$trackName==input$Selection,]}
    data_timeline <- data.frame(
      id      = c(1:dim(data_band)[1]),
      content = c(data_band$trackName),
      start   = c(data_band$time),
      end     = c(rep(NA, times=length(data_band$time)))
    )
    timevis(data_timeline)
    
    
  })
  
  # interactive activity plot, highlighting and showing top 3 bands when scrolled over
  
  output$timeline3 <- renderPlot({
    dataAll <- myData()
    dataAll[,3] <- sub(' .*$','', dataAll[,3])
    data.act <- as.data.frame(table(dataAll[,3]))
    data.act[,1] <- as.Date(data.act[,1])
    plot.activity2 <- ggplot(data.act, aes(x=Var1, y = Freq)) + geom_segment(aes(x=Var1, xend = Var1, y = 0, yend=Freq)) + geom_point(size=1, color="lightblue", fill=alpha("grey", 0.3), alpha=0.7, shape=21, stroke=2)
    plot.activity2
    
    
  })
  
})