################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: find coldspots and create layer for app
################################################################################

library(sf)
#library(eks)
library(ggplot2)
library(rgeoboundaries)
library(spatstat)
library(raster)

lux_borders <- geoboundaries(country = "Luxembourg", adm_lvl = "adm0")
lux_borders <- st_transform(lux_borders, crs = 2169)

source("config.R")
source("mdata.R")

#contour_data <- load_data(DATA_PATH)
contour_data <- contour_data["geometry"]
contour_data <- st_crop(contour_data, lux_borders)




# ##### SPATSTAT
# 
# ppp_points <- as.ppp(st_coordinates(contour_data), W = as.owin(st_bbox(contour_data)))
# 
# density_result <- density(ppp_points, sigma = 100)
# 
# density_raster <- raster(density_result)
# 
# contours <- rasterToContour(density_raster)
# 
# contour_sf <- st_as_sf(contours)
# 
# # Plot contours with sf
# plot(st_geometry(contour_sf))



##### EKS DOES NOT WORK FOR BIG NUMBERS ???
# Create KDE
# all_pointsOLD <-all_points
# skdeOLD <-skde

set.seed(2)
skde <- st_kde(contour_data[sample(1:nrow(contour_data), 500000),], gridsize= c(100,100))

# skde <- st_kde(contour_data, binned = TRUE, gridsize= c(10,10))
# skde2 <- st_kde(contour_data, binned=TRUE)
# skde_forced_multi <- skde 

contours <- st_get_contour(skde, cont=c(85, 90, 96, 97, 98, 99, 99.999))
contours <-  st_crop(contours, lux_borders)

theme_set(ggthemes::theme_map())


# Actual kde
gs <- ggplot(skde, stoke=NA) #+ geom_sf(data=lux_borders) #+ geom_sf(data=all_points) no points


# Colors
percentile_colors <- c(# Light purple, semi-transparent
  "85%" = alpha("black", 0),
  "90%" = alpha("purple4", 0.1),
  "96%" = alpha("purple4", 0.3),        # Medium purple, more opaque
  "97%" = alpha("purple4", 0.5),   # Dark purple, mostly opaque
  "98%" = alpha("purple4", 0.8),
  "99%" = alpha("purple4", 0),        # Medium purple, more opaque
  "99.999%" = alpha("purple4", 0)    # Dark purple, mostly opaque - transparent because border effect
)

# Remove border effect
lux_buf500 <- st_buffer(lux_borders, dist=-1500)
cont_corr <- st_intersection(contours, lux_buf500)

# Plot
gs + geom_sf(data=contours, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()

# Plot
gs + geom_sf(data=cont_corr, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()

st_write(cont_corr, "contours_1500.geojson")




