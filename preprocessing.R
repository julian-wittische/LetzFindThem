################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: Load all necessary libraries and data
################################################################################

###### Libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(eks)
library(ggplot2)
library(colorspace)
library(ggspatial)
library(rgeoboundaries)
library(units)
library(parallel)

###### Non-package functions
source("utils.R")

# Actual species
mdata <- read.csv("C:/Users/YNM724/Downloads/mnhn_observations_csv (3)/observations.csv", encoding="latin1")
mdata <- mdata[complete.cases(mdata$Lat),]
mdata <- mdata[complete.cases(mdata$Long),]
mdata <- mdata[complete.cases(mdata$preferred),]

# Keep only strict minimum# Keep onlpreferredy strict minimum
reduced <- mdata[,c("Observation_Key","Lat", "Long", "preferred")]
# Put Lat after Long
reduced <- reduced[, c(1,3,2,4)]
# Change columns names
colnames(reduced) <- c("name", "lng", "lat", "species")

red500 <- reduced

new_points <- red500
new_points_sf <- st_as_sf(new_points, coords = c("lng", "lat"), crs = 4326)

# Remove observations outside of Luxembourg
lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:4326")
all_points <- new_points_sf[st_within( new_points_sf, lux_borders, sparse=FALSE),]
beepr::beep(8)
