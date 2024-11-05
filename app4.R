################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal:  app
################################################################################

library(shiny)
library(httr)
library(jsonlite)

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

source("utils.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Random Species Images"),
  uiOutput("imageGrid",
           class = "grid",
           style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; margin-top: 20px;"
  )
)

# Select a random sample of species
random_species <- sample(species_list, 5)  # Change the number to display more or fewer images



# Define the server logic
server <- function(input, output, session) {

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

# Run the app
shinyApp(ui, server)
