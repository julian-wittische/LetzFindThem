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

# Function to get images from Wikimedia Commons
get_images <- function(species_name) {
  # Step 1: Search for the species page
  search_url <- paste0("https://commons.wikimedia.org/w/api.php?action=query&list=search&srsearch=",
                       URLencode(species_name), "&srlimit=1&format=json")
  
  search_response <- GET(search_url)
  search_data <- fromJSON(content(search_response, as = "text"), flatten = TRUE)
  
  # Check if the search returned valid results
  if (!is.null(search_data$query$search) && nrow(search_data$query$search) > 0) {
    title <- search_data$query$search$title
    
    # Step 2: Get images for the page
    image_api_url <- paste0("https://commons.wikimedia.org/w/api.php?action=query&titles=", 
                            URLencode(title), "&prop=images&format=json")
    
    image_response <- GET(image_api_url)
    image_data <- fromJSON(content(image_response, as = "text"), flatten = TRUE)
    
    # Extract the page ID
    page_id <- names(image_data$query$pages)
    
    # Check if the page ID is valid and not -1
    if (page_id != "-1") {
      # Check if the page has images
      if (!is.null(image_data$query$pages[[page_id]]$images) && 
          length(image_data$query$pages[[page_id]]$images) > 0) {
        
        # Extract image titles
        image_titles <- image_data$query$pages[[page_id]]$images$title
        
        # Construct URLs for the images based on the titles
        if (length(image_titles) > 0) {
          # Construct URLs for each image
          image_urls <- sapply(image_titles, function(title) {
            image_file_name <- gsub("File:", "", title)  # Remove "File:" prefix
            paste0("https://commons.wikimedia.org/wiki/Special:FilePath/", URLencode(image_file_name))
          })
          print(image_urls)
          return(image_urls)
        } else {
          print("No valid image titles found.")
        }
      } else {
        print("No images found for this species.")
      }
    } else {
      print("Invalid page ID.")
    }
  } else {
    print("No search results found.")
  }
  
  return(NULL)  # Return NULL if no image is found
}

# Define the UI
ui <- fluidPage(
  titlePanel("Random Species Images"),
  div(
    id = "imageGrid", class = "grid",
    style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; margin-top: 20px;"
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Select a random sample of species
  random_species <- sample(species_list, 5)  # Change the number to display more or fewer images
  
  # Fetch images for random species
  images <- lapply(random_species, function(species) {
    image_urls <- get_images(species)
    if (!is.null(image_urls)) {
      return(image_urls)
    } else {
      return(NULL)
    }
  })
  
  # Render the image grid
  output$imageGrid <- renderUI({
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
