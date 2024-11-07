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
skde1 <- st_kde(all_points[1:50,])

##### Plotting
# km scale
gsc <- ggspatial::annotation_scale(data=all_points, location="br", width_hint=0.2, bar_cols=1)

# theme
theme_set(ggthemes::theme_map())
theme_update(legend.position=c(0.99,0.99), legend.justification=c(1,1))

# 
gs <- ggplot(skde1) + geom_sf(data=lux_borders) + geom_sf(data=all_points)
gs

# 
geom_sf(data=st_get_contour(skde1), aes(fill=label_percent(contlabel))) + 
  scale_fill_discrete_sequential(h1=275)  + #coord_sf(xlim=xlim, ylim=ylim) +
  ggthemes::theme_map()

# Plot with inverted color scale


# Invert
kde_inv <- 1 / (kde + 1e-6)

# Normalize
kde_inv <- kde_inv / max(kde_inv)



