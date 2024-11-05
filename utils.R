################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: utility functions to be used by app
################################################################################

###### Function to get images from Wikimedia Commons
# Function to get images from Wikimedia Commons with upload URLs
get_images <- function(species_name) {
  # Step 1: Search for the species page
  search_url <- paste0("https://commons.wikimedia.org/w/api.php?action=query&list=search&srsearch=",
                       URLencode(species_name), "&srlimit=1&format=json")
  
  search_response <- GET(search_url)
  search_data <- fromJSON(content(search_response, as = "text"), flatten = TRUE)
  
  # Check if the search returned valid results
  if (!is.null(search_data$query$search) && nrow(search_data$query$search) > 0) {
    title <- search_data$query$search$title[1]
    
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
        
        # Fetch the actual file URLs from upload.wikimedia.org
        if (length(image_titles) > 0) {
          image_urls <- lapply(image_titles, function(title) {
            file_name <- gsub("File:", "", title)  # Remove "File:" prefix
            # Call the API to get the direct file URL
            file_info_url <- paste0("https://commons.wikimedia.org/w/api.php?action=query&titles=File:",
                                    URLencode(file_name), "&prop=imageinfo&iiprop=url&format=json")
            file_info_response <- GET(file_info_url)
            file_info_data <- fromJSON(content(file_info_response, as = "text"), flatten = TRUE)
            page_id_file <- names(file_info_data$query$pages)
            file_url <- file_info_data$query$pages[[page_id_file]]$imageinfo$url
            return(file_url)
          })
          return(unlist(image_urls))  # Return the URLs
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