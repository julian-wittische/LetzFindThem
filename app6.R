################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: utility functions to be used by app
################################################################################

# Radii values
inner_radius <- set_units(1000, "m")  # m inner radius
outer_radius <- set_units(5000, "m")  # m outer radius

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

#try(random_species <- sample(species_list, 5))


# Define the server logic
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 6.13, lat = 49.61, zoom = 12) #%>%
    #addMarkers(data = all_points, popup = ~name)
  })
  
  # CHECKPOINT START
  pre <- Sys.time()
  
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    print(paste("post click", Sys.time() - pre))
    
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
    
    # CHECKPOINT
    print(paste("post map creation", Sys.time() - pre))
    
    annulus_center <- st_as_sf(data.frame(lng = lng, lat = lat),
                               coords = c("lng", "lat"), crs = 4326)
    
    distances <- st_distance(annulus_center, all_points)
    annu <- distances <= outer_radius & distances > inner_radius
    points_in_annulus <- all_points[annu, ]
    
    # CHECKPOINT
    print(paste("post annulus points selection", Sys.time() - pre))
    
    species_count <- as.data.frame(table(as.data.frame(points_in_annulus)$species))
    colnames(species_count) <- c("species", "count")
    
    # CHECKPOINT
    print(paste("post species table computation", Sys.time() - pre))
    
    output$speciesInfo <- renderUI({
      if (nrow(species_count) == 0) {
        return(h4("No species within the outer but not inner circle."))
      } else {
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
    
    # CHECKPOINT
    print(paste("post species table rendering", Sys.time() - pre))
    
    # random_species <- species_count$species#sample(unique(species_count$species),100)
    # # Render the image grid
    # output$imageGrid <- renderUI({
    #   # Fetch images for random species
    #   images <- lapply(random_species, function(species) {
    #     image_urls <- get_images(species)
    #     if (!is.null(image_urls)) {
    #       return(image_urls)
    #     } else {
    #       return(NULL)
    #     }
    #   })
    #   
    #   image_elements <- lapply(images, function(image_url) {
    #     if (!is.null(image_url)) {
    #       tags$img(src = image_url[1], alt = "Species image", style = "width: 100%; height: auto;")  # Display the first image
    #     } else {
    #       NULL
    #     }
    #   })
    #   do.call(tagList, image_elements)
    # })
    
    # # CHECKPOINT
    # print(paste("post image rendering", Sys.time() - pre))
  }
  )
}

# Run the app
shinyApp(ui, server)