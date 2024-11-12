################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: find coldspots and create layer for app
################################################################################

# Create KDE
skde <- st_kde(all_points)

##### Plotting
# # km scale
# gsc <- ggspatial::annotation_scale(data=all_points, location="br", width_hint=0.2, bar_cols=1)

contours <- st_get_contour(skde, cont=c(95, 96, 97, 98, 99, 99.999))
contours <-  st_crop(contours, lux_borders_buff)

theme_set(ggthemes::theme_map())


# Actual kde
gs <- ggplot(skde, stoke=NA) #+ geom_sf(data=lux_borders) #+ geom_sf(data=all_points) no points

# Colors
percentile_colors <- c(# Light blue, semi-transparent
  "95%" = alpha("black", 0),
  "96%" = alpha("blue", 0.1),        # Medium blue, more opaque
  "97%" = alpha("blue", 0.2),   # Dark blue, mostly opaque
  "98%" = alpha("blue", 0.3),
  "99%" = alpha("blue", 0.4),        # Medium blue, more opaque
  "99.999%" = alpha("blue", 0.5)    # Dark blue, mostly opaque
)

# Colors
percentile_colors <- c(# Light purple, semi-transparent
  "95%" = alpha("black", 0),
  "96%" = alpha("purple4", 0.1),        # Medium purple, more opaque
  "97%" = alpha("purple4", 0.2),   # Dark purple, mostly opaque
  "98%" = alpha("purple4", 0.3),
  "99%" = alpha("purple4", 0.4),        # Medium purple, more opaque
  "99.999%" = alpha("purple4", 0.5)    # Dark purple, mostly opaque
)

# Colors
percentile_colors <- c(# Light purple, semi-transparent
  "95%" = alpha("black", 0),
  "96%" = alpha("orange", 0.1),        # Medium purple, more opaque
  "97%" = alpha("orange", 0.2),   # Dark purple, mostly opaque
  "98%" = alpha("orange", 0.3),
  "99%" = alpha("orange", 0.4),        # Medium purple, more opaque
  "99.999%" = alpha("orange", 0.5)    # Dark purple, mostly opaque
)

# Plot
gs + geom_sf(data=contours, aes(fill=label_percent(contlabel)), color=NA) + 
  scale_fill_manual(values=percentile_colors) +
  ggthemes::theme_map()
