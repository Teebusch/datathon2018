#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Cycling Data"),
  fluidRow(
    column(width = 12, 
           sidebarPanel(
             sliderInput("bins", "Interval:",
                         min = 1, max = 1984,
                         value = 146, step = 30,
                         animate = TRUE))
    )
  ),
    
  fluidRow(
    column(width = 12,
       plotOutput("map"),
       plotOutput("detail"),
       plotOutput("speed"),
       plotOutput("cadence"),
       plotOutput("power"),
       plotOutput("altitude"),
       plotOutput("heartrate"),
       plotOutput("gear", height = "800px")

    )
  )
))
