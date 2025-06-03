# Load required packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidygeocoder)
library(sf)
library(dplyr)

# Load app data
load("wellness_app_data.RData")

ui <- fluidPage(
  # titlePanel("Recruitment Address Screener App"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Enter address (e.g., 4305 W Madison St, Chicago, IL 60624):", ""),
      actionButton("submit", "Submit"),
      verbatimTextOutput("tract_output")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  # Base map
  output$map <- renderLeaflet({
    wellness_center_map
  })
  
  observeEvent(input$submit, {
    req(input$address)
    
    result <- tryCatch({
      geo(address = input$address, method = 'osm', lat = latitude, long = longitude, full_results = FALSE)
    }, error = function(e) {
      NULL
    })
    
    # Validate that result is not NULL and has expected columns
    if (!is.null(result) && nrow(result) > 0 && !is.na(result$longitude) && !is.na(result$latitude)) {
      coords <- st_as_sf(result, coords = c("longitude", "latitude"), crs = 4326)
      
      tract_match <- st_join(coords, catchment_areas, join = st_within)
      
      if (nrow(tract_match) > 0 && !is.na(tract_match$name[1])) {
        tract_id <- tract_match$name[1]
        
        output$tract_output <- renderText({
          paste("Catchment Area Name:", tract_id)
        })
        
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(lng = result$longitude, lat = result$latitude, popup = paste("Inside", tract_id)) %>%
          addCircleMarkers(
            lng = -87.73343171837095,
            lat = 41.880466384462274,
            label = "4305 W Madison",
            weight = 1,
            color = "black",
            fillColor = "darkorange",
            group = "Wellness Center",
            fillOpacity = 1,
            options = pathOptions(pane = "Wellness Center")
          )
        
      } else {
        output$tract_output <- renderText({"Address is outside designated catchment areas."})
        
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(lng = result$longitude, lat = result$latitude) %>%
          addCircleMarkers(
            lng = -87.73343171837095,
            lat = 41.880466384462274,
            label = "4305 W Madison",
            weight = 1,
            color = "black",
            fillColor = "darkorange",
            group = "Wellness Center",
            fillOpacity = 1,
            options = pathOptions(pane = "Wellness Center")
          )
      }
      
    } else {
      output$tract_output <- renderText({"Address could not be geocoded."})
      leafletProxy("map") %>%
        clearMarkers()
    }
  })
  
}

shinyApp(ui, server)

