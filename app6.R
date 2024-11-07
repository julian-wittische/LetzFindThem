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
library(bslib)
library(leaflet)
library(sf)
library(dplyr)

###### Non-package functions
source("utils.R")

# Load observation data
source("mdata.R")

reduced <- reduced[, c(1,3,2,4)]
colnames(reduced) <- c("name", "lng", "lat", "species")

new_points <- reduced
all_points <- st_as_sf(new_points, coords = c("lng", "lat"), crs = 4326)
all_points <- st_transform(all_points, 2169)
all_points_coords <- st_coordinates(all_points)

library(rtree)
tree <- RTree(all_points_coords)

# Radii values
inner_radius <- 1000  # m inner radius
outer_radius <- 5000  # m outer radius

# Define the UI
ui <- page_fluid(
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
  div(
    style = "padding: 20px;",
    uiOutput("speciesInfo"),
  )
)

lv_points_in_circle <- function(tree, center, radius) {
  unlist(withinDistance(tree, center, radius)[1])
}

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$map_click, {
    pre <- Sys.time()

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
    
    ## output$annulusInfo <- renderPrint({
    ##   paste("Annulus centered at: Lat:", lat, "Lng:", lng)
    ## })
    
    center <- st_as_sf(data.frame(lng = lng, lat = lat),
                       coords = c("lng", "lat"), crs = 4326)
    center <- st_transform(center, 2169)

    center_coords <- st_coordinates(center)

    idx_outer <- lv_points_in_circle(tree, center_coords, outer_radius)
    points_outer <- all_points[idx_outer,]
    species_count_outer <- as.data.frame(table(points_outer$species))

    idx_inner <- lv_points_in_circle(tree, center_coords, inner_radius)
    points_inner <- all_points[idx_inner,]
    species_count_inner <- as.data.frame(table(points_inner$species))

    species_annulus <- setdiff(species_count_outer$Var1, species_count_inner$Var1)
    
    species_count_annulus <- species_count_outer[species_count_outer$Var1 %in% species_annulus,]
    species_count_annulus <- species_count_annulus[order(species_count_annulus$Freq, decreasing = TRUE),]
    
    output$speciesInfo <- renderUI({
      if (nrow(species_count_annulus) == 0) {
        return(h4("No species within the outer but not inner circle."))
      } else {
        ## species_table <- tags$table(class = "species-table",
        ##                             tags$thead(
        ##                               tags$tr(
        ##                                 tags$th("Species"),
        ##                                 tags$th("Count")
        ##                               )
        ##                             ),
        ##                             tags$tbody(
        ##                               lapply(1:nrow(species_count_annulus), function(i) {
        ##                                 tags$tr(
        ##                                   tags$td(species_count_annulus$Var1[i]),
        ##                                   tags$td(species_count_annulus$Freq[i])
        ##                                 )
        ##                               })
        ##                             )
        ## )
          ## return(species_table)
          layout_column_wrap(
              width="200px", fixed_width=TRUE,
              !!!lapply(1:nrow(species_count_annulus), function(i) {
                  card(
                      card_header(species_count_annulus$Var1[i]),
                      card_body(
                          paste("Count in donut", species_count_annulus$Freq[i])
                      )
                  )
              })
          )
      }
    })

    print(Sys.time() - pre)
  })

  output$map <- renderLeaflet({
      leaflet() %>%
          addTiles() %>%
          setView(lng = 6.13, lat = 49.61, zoom = 12) #%>%
          #addMarkers(data = all_points, popup = ~name)
  })
}

# Run the app
shinyApp(ui, server, options=list(port=8080, shiny.autoreload=TRUE))
