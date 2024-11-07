################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: testing script
################################################################################

###### Libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(units)
library(parallel)

###### Non-package functions
source("utils.R")

# Testing with random observations
#source("randomspecies.R")

# Actual species
source("mdata.R")

# Put Lat after Long
reduced <- reduced[, c(1,3,2,4)]
# Change columns names
colnames(reduced) <- c("name", "lng", "lat", "species")
# Test with 500
#reduced <- reduced[order(reduced$preferred),]
red500 <- reduced[1:500000,]

new_points <- red500
new_points_sf <- st_as_sf(new_points, coords = c("lng", "lat"), crs = 4326)
all_points <- new_points_sf

# Radii values
inner_radius <- set_units(1000, "m")  # m inner radius
outer_radius <- set_units(5000, "m")  # m outer radius

# Random location in Lux City
lng <- 6.13
lat <- 49.61

annulus_center <- st_as_sf(data.frame(lng = lng, lat = lat),
                           coords = c("lng", "lat"), crs = 4326)
# OLD APPROACH
pre <- Sys.time()
distances <- st_distance(annulus_center, all_points)
annu <- distances <= outer_radius & distances > inner_radius
print(Sys.time() - pre)
beepr::beep(10)

# # NEW APPROACH 1
# pre <- Sys.time()
# bounding_box <- st_buffer(annulus_center, outer_radius)
# pre_filtered_points <- all_points[st_intersects(all_points, bounding_box, sparse = FALSE), ]
# distances <- st_distance(annulus_center, pre_filtered_points)
# couronne <- distances <= outer_radius & distances > inner_radius
# print(Sys.time() - pre)
# beepr::beep(10)
# 
# # NEW APPROACH 2
# pre <- Sys.time()
# pre_filtered_points <- st_join(annulus_center, all_points, join=st_is_within_distance, outer_radius)
# distances <- st_distance(annulus_center, pre_filtered_points)
# couronne <- distances <= outer_radius & distances > inner_radius
# print(Sys.time() - pre)
# beepr::beep(10)

# SPATIAL INDEXING BULLSHIT
pre <- Sys.time()
within_outer_circle <- st_join(annulus_center, all_points, join=st_is_within_distance, outer_radius)
within_inner_circle <- st_join(annulus_center, within_outer_circle, join=st_is_within_distance, inner_radius)
couronne <- st_difference(within_outer_circle, within_inner_circle)
print(Sys.time() - pre)
beepr::beep(5)