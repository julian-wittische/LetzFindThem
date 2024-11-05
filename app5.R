################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: utility functions to be used by app
################################################################################

###### Libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)

###### Non-package functions
source("utils.R")

# Updated list of scientific species names
species_list <- c("Panthera leo",  # Lion
                  "Ailuropoda melanoleuca",  # Giant Panda
                  "Elephas maximus",  # Asian Elephant
                  "Ursus arctos",  # Brown Bear
                  "Acinonyx jubatus",  # Cheetah
                  "Lynx lynx",  # Eurasian Lynx
                  "Bubo bubo",  # Eurasian Eagle-Owl
                  "Gallus gallus",  # Domestic Chicken
                  "Canis lupus familiaris",  # Domestic Dog
                  "Felis catus")  # Domestic Cat

# Generate random points near Luxembourg
set.seed(123)
new_points <- data.frame(
  name = paste("Point", 1:30),
  lng = runif(30, min = 6.10, max = 6.20),
  lat = runif(30, min = 49.55, max = 49.70),
  species = sample(species_list, 30, replace = TRUE)
)

# Convert to sf object
new_points_sf <- st_as_sf(new_points, coords = c("lng", "lat"), crs = 4326)

# Combine the new points with the existing points
all_points <- new_points_sf

# Define the UI
ui <- fluidPage(
  tags$style(type = "text/css", "
    #map {
      height: 50vh; 
      width: 100%;
    }
    .species-table {
      width: 100%;
      border-collapse: collapse;
    }
    .species-table th, .species-table td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: center;
    }
    .species-table th {
      background-color: #f2f2f2;
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
      gap: 10px;
      margin-top: 20px;
    }
    .grid img {
      width: 100%;
      height: auto;
    }
  "),
  
  div(
    leafletOutput("map", width = "100%", height = "50vh"),
    style = "height: 50vh;"
  ),
  
  #   div(
  #     style = "height: 50vh; padding: 20px;",
  #     h3("Annulus Information"),
  #     verbatimTextOutput("annulusInfo"),
  #     uiOutput("speciesInfo"),
  #     div(id = "imageGrid", class = "grid")  # Container for the image grid
  #   )
  # )
  # Information and Images
  div(
    style = "padding: 20px;",
    h3("Annulus Information"),
    verbatimTextOutput("annulusInfo"),
    uiOutput("speciesInfo"),
    titlePanel("Random Species Images"),
    uiOutput("imageGrid",
             class = "grid",
             style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; margin-top: 20px;"
    ))  
  )

random_species <- sample(species_list, 5) 

# Define the server logic
server <- function(input, output, session) {
  
  inner_radius <- 1000  # 1 km inner radius
  outer_radius <- 5000  # 5 km outer radius
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 6.13, lat = 49.61, zoom = 12) %>%
      addMarkers(data = all_points, popup = ~name)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(
        lng = lng, lat = lat, radius = outer_radius,
        color = "blue", fillColor = "blue", fillOpacity = 0,
        layerId = "outerCircle"
      ) %>%
      addCircles(
        lng = lng, lat = lat, radius = inner_radius,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.2,
        layerId = "innerCircle"
      )
    
    output$annulusInfo <- renderPrint({
      paste("Annulus centered at: Lat:", lat, "Lng:", lng)
    })
    
    annulus_center <- st_as_sf(data.frame(lng = lng, lat = lat),
                               coords = c("lng", "lat"), crs = 4326)
    
    distances <- st_distance(annulus_center, all_points)
    
    within_outer_circle <- as.numeric(distances) <= outer_radius
    outside_inner_circle <- as.numeric(distances) > inner_radius
    
    points_in_annulus <- all_points[within_outer_circle & outside_inner_circle, ]
    
    output$speciesInfo <- renderUI({
      if (nrow(points_in_annulus) == 0) {
        return(h4("No species within the outer but not inner circle."))
      } else {
        species_count <- points_in_annulus %>%
          group_by(species) %>%
          summarise(count = n(), .groups = 'drop') %>%
          arrange(desc(count))
        
        species_table <- tags$table(class = "species-table",
                                    tags$thead(
                                      tags$tr(
                                        tags$th("Species"),
                                        tags$th("Count")
                                      )
                                    ),
                                    tags$tbody(
                                      lapply(1:nrow(species_count), function(i) {
                                        tags$tr(
                                          tags$td(species_count$species[i]),
                                          tags$td(species_count$count[i])
                                        )
                                      })
                                    )
        )
        
        return(species_table)
      }
    })
    
    # Render the image grid
    output$imageGrid <- renderUI({
      # Fetch images for random species
      images <- lapply(random_species, function(species) {
        image_urls <- get_images(species)
        if (!is.null(image_urls)) {
          return(image_urls)
        } else {
          return(NULL)
        }
      })
      
      image_elements <- lapply(images, function(image_url) {
        if (!is.null(image_url)) {
          tags$img(src = image_url[1], alt = "Species image", style = "width: 100%; height: auto;")  # Display the first image
        } else {
          NULL
        }
      })
      do.call(tagList, image_elements)
    })
  }
)
}
  
# Run the app
shinyApp(ui, server)