library(leaflet)
library(shiny)



ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("uniSmap", width = "100%", height = "100%"),
  
  absolutePanel(top = 50, right = 50,
                sliderInput("range", "Satisfaction Score", min(my_shiny_Data$`Satisfaction (%) 2016 Registered`), max(my_shiny_Data$`Satisfaction (%) 2016 Registered`),
                            
                            value = range(my_shiny_Data$`Satisfaction (%) 2016 Registered`), step = 1
                )
  )
)




server <- function(input, output, session) {
  
  
  
  # Reactive expression for the data subsetted to what the user selected
  
  reactiveData <- reactive({
    
    my_shiny_Data[my_shiny_Data$`Satisfaction (%) 2016 Registered` >= input$range[1] & my_shiny_Data$`Satisfaction (%) 2016 Registered` <= input$range[2],]
    
  })
  
  
  
  output$uniSmap <- renderLeaflet({
    
    
    
    # Use leaflet() here for the static map
    
    
    
    uniSmap = leaflet() %>%
      
      
      
      addTiles() %>%
      
      
      
      setView(lng = -2.2, lat = 54.5, zoom = 6)
    
  })
  
  
  
  # Incremental changes to the map performed in an observer.
  
  
  
  observe({
    
    
    
    leafletProxy("uniSmap", data = reactiveData()) %>%
      
      clearShapes() %>% clearPopups() %>% clearMarkers() %>%
      
      
      
      addMarkers(lng = my_shiny_Data$Longitude,
                 
                 lat = my_shiny_Data$Latitude,
                 
                 popup = paste(my_shiny_Data$Institution, "<br>",
                               
                               "Overall Satisfaction:", my_shiny_Data$`Satisfaction (%) 2016 Registered`, "<br>"))        
    
    
    
  })
  
  
  
}




shinyApp(ui, server)