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
library(eks)
library(ggplot2)
library(rgeoboundaries)
library(spatstat)
library(raster)

lux_borders <- geoboundaries(country = "Luxembourg", adm_lvl = "adm0")
lux_borders <- st_transform(lux_borders, crs = 2169)

source("config.R")
source("mdata.R")
source("global.R")

contour_data <- all_points

contour_data <- load_data(DATA_PATH)
contour_data <- contour_data["geometry"]
contour_data <- st_crop(contour_data, lux_borders)

set.seed(2)
skde <- st_kde(contour_data[sample(1:nrow(contour_data), 500000),], gridsize= c(100,100))

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

# Full
gs + geom_sf(data=contours, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()

# With 1.5km removed to avoid showing border artifacts
gs + geom_sf(data=cont_corr, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()

# Change CRS to alleviate problem with leaflet?
cont <- st_transform(cont_corr, crs=4326)
colocont <- "orange3"

# Colors for END USE in app
cont$color <- rev(unname(percentile_colors <- c(# Light purple, semi-transparent
    "85%" = alpha("black", 0),
    "90%" = alpha(colocont, 0.4),
    "96%" = alpha(colocont, 0.6),        # Medium purple, more opaque
    "97%" = alpha(colocont, 0.8),   # Dark purple, mostly opaque
    "98%" = alpha(colocont, 1),
    "99%" = alpha(colocont, 0),        # Medium purple, more opaque
    "99.999%" = alpha(colocont, 0)    # Dark purple, mostly opaque - transparent because border effect
  )))

# To test colors
gs + geom_sf(data=cont, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()

save(cont, file="cont.RData")
