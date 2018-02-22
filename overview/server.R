#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  dataPlot <- reactive({
    if (as.numeric(input$bins) < 300) {
      return(df_sum %>% filter(time <= as.numeric(input$bins)))
    } else{
      return(df_sum %>% filter(time <= as.numeric(input$bins) & time > 300))
    }
  })
  

  output$map <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    dataPlot() %>%
      ggplot(aes(x = longitude, y = latitude, color = qid)) +
      geom_path(show.legend = TRUE) +
      scale_x_continuous(limits = c(min(df_sum$longitude), max(df_sum$longitude) )) +
      scale_y_continuous(limits = c(min(df_sum$latitude), max(df_sum$latitude) )) +
      facet_wrap(~qid)
      
  })
  
  output$detail <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    dataPlot() %>% 
      filter(time > (max(time) - 30)) %>%
      ggplot(aes(x = longitude, y = latitude, color = qid, alpha = 1/time)) +
      geom_path(show.legend = TRUE) +
      geom_point(aes(size = 1/time))
    
  })
  
  output$speed <- renderPlot({
    
    dataPlot() %>%
      ggplot(aes(x = time, y = speed, color = qid, group = qid)) +
      geom_line() +
      geom_point() +
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_continuous(limits = c(min(df_sum$speed), max(df_sum$speed) )) + 
      facet_grid(qid ~ .) + 
      ggtitle("Speed")
  })
  
  
  output$heartrate <- renderPlot({
    
    dataPlot() %>%
      ggplot(aes(x = time, y = heartrate, color = qid, group = qid)) +
      geom_line() +
      geom_point() + 
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_continuous(limits = c(min(df_sum$heartrate), max(df_sum$heartrate) )) + 
      facet_grid(qid ~ .) +
      ggtitle("Heart Rate")
  })
  
  output$gear <- renderPlot({
    # as.numeric(input$bins)
    dataPlot() %>%
      ggplot(aes(x = time, y = gear, color = qid, group = qid)) +
      geom_line() +
      geom_point() +
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_discrete(limits = levels_gears) + 
      facet_grid(qid ~ .) +
      ggtitle("Gear: Front | Rear derailleur")
  })
  
  output$altitude <- renderPlot({
    # as.numeric(input$bins)
    dataPlot() %>%
      ggplot(aes(x = time, y = altitude, color = qid, group = qid)) +
      geom_line() +
      geom_point() +
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_continuous(limits = c(min(df_sum$altitude), max(df_sum$altitude) )) + 
      facet_grid(qid ~ .) +
      ggtitle("Altitude")
  })
  
  output$power <- renderPlot({
    # as.numeric(input$bins)
    dataPlot() %>%
      ggplot(aes(x = time, y = power, color = qid, group = qid)) +
      geom_line() +
      geom_point() +
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_continuous(limits = c(min(df_sum$power), max(df_sum$power) )) + 
      facet_grid(qid ~ .) +
      ggtitle("Power")
  })
  
  
  output$cadence <- renderPlot({
    # as.numeric(input$bins)
    dataPlot() %>%
      ggplot(aes(x = time, y = cadence, color = qid, group = qid)) +
      geom_line() +
      geom_point() +
      #scale_x_continuous(limits = c(min(df3$time), max(df3$time) )) +
      scale_y_continuous(limits = c(min(df_sum$cadence), max(df_sum$cadence) )) + 
      facet_grid(qid ~ .) +
      ggtitle("Cadence")
  })

  
  
})
