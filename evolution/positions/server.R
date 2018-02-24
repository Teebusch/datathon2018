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
  
  output$areaPlot = renderPlot({
    df1_distance %>%
      ggplot(aes(x = bin_plot, y = distance, group = check)) +
      geom_area() +
      ggalt::stat_xspline(spline_shape=0) +
      scale_x_continuous(breaks = seq(0, 274, 3) ) +
      xlab("Time (minutes)") +
      ylab("Distance (meters)") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("Distance between cyclists (2017-10-17 UTC)") + 
      geom_vline(xintercept = as.numeric(input$bins), color = "yellow", size = 2)
  })
   
  output$distPlot <- renderPlot({
    zdf_plot = df1_plot %>% filter(bin_half == as.numeric(input$bins)) 
    
    zdf_plot %>%
    ggplot(aes(x = long, y = lat)) +
      geom_polygon(data = hulls %>% filter(bin_half == as.numeric(input$bins))) +
      scale_x_continuous(limits= c(-max(df1_plot$long), max(df1_plot$long) )) +
      scale_y_continuous(limits= c(-max(df1_plot$lat) ,max(df1_plot$lat) )) +
      ggalt::geom_encircle(s_shape=0.5, fill="#585858") + 
      geom_point(data = zdf_plot %>% filter(order == 1 ),
                 aes(x=long, y=lat), color = "red", size = 20, shape=3) +
      theme_bw()
  })
  
})
