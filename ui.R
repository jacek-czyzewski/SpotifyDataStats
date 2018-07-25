#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(timevis)
library(DT)


# Define UI for application that draws a histogram
shinyUI(navbarPage("Spotify Data Stats",
  tabPanel("Data upload",
  fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose SpotifyHistory.json file',
                accept=c("json"))
    ),
    mainPanel(
      DTOutput('contents')
    )
  )
)
), tabPanel("Statistics",
         fluidPage(
           titlePanel("Statistics"),
           helpText("Some basic facts about your listening history"),
           sidebarLayout(
             sidebarPanel(
             ),
             mainPanel(
               
             )
           )
           
           
           
         )
), tabPanel("Data exploration",
            fluidPage(
              titlePanel("Data exploration"),
              
              
              helpText("Plots and characteristics of the data set"),
              sidebarLayout(
                
                sidebarPanel(
                  selectInput("var", 
                              label = "Choose a variable to display",
                              choices = c("Artists", "Tracks",
                                          "Time"),
                              selected = "Artists"),
                  
                  sliderInput("cutoff",
                              "Select cutoff:",
                              min = 1,
                              max = 50,
                              value = 25)
                ),
                
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Bar Plots", plotOutput("barplot"))
                  )
                )
              )
            )
), tabPanel("Timelines",
              fluidPage(
                titlePanel("Timelines"),
                
                
                helpText("See when you listened to given bands or tracks"),
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("var1", 
                                label = "Choose a variable to display",
                                choices = c("Artists", "Tracks"),
                                selected = "Artists"),
                    
                    uiOutput("secondSelection")
                  ),
                  
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Data", DTOutput("timeline1")),
                      tabPanel("Timeline", timevisOutput("timeline2"))
                    )
                  )
                )
              )
)
)

)
