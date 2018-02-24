#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(navbarPage("Peloton Viewer",
                   tabPanel("Race (2017-10-17)"),
                   # Sidebar with a slider input for number of bins 
                   fluidPage(  fluidRow(
                     column(width = 12, 
                            sliderInput("bins",
                                        "Number of bins:",
                                        min = 108,
                                        max = 144,
                                        value = 108,
                                        animate = animationOptions(interval = 1000, loop = TRUE)),
                            plotOutput("areaPlot", height = "300px"),
                            plotOutput("distPlot", height = "600px")
                   ))),
                   tabPanel("Component 2"),
                   tabPanel("Component 3")
))
