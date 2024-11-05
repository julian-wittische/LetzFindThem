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