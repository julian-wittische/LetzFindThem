################################################################################
############### PROJECT: national database exploration app #####################
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Patrick Michaely
# Start: Fall 2024
# Data: MNHNL/Web
# Script goal: utility functions to be used by app
################################################################################

###### Libraries
source("config.R")
source("taxon_info.R")

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(digest)
library(rtree)
if (DEBUG) {
  library(tictoc)
}

lv_points_in_circle <- function(tree, center, radius) {
  unlist(withinDistance(tree, center, radius)[1])
}

thumb <- function(url) {
    filename <- URLdecode(tail(strsplit(url, split="/")[[1]], n=1))
    filename <- gsub(" ", "_", filename)
    md5 <- digest(filename, algo="md5", serialize=FALSE)
    src <- paste("https://upload.wikimedia.org/wikipedia/commons/thumb/",
                 substr(md5, 1, 1), "/",
                 substr(md5, 1, 2), "/",
                 filename, "/",
                 "200px-", filename,
                 sep="")
    src
}

maybetic <- function(msg) {
  if (DEBUG) {
    tic(msg)
  }
}

maybetoc <- function() {
  if (DEBUG) {
    toc()
  }
}

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(
        lng = lng, lat = lat, radius = OUTER_RADIUS,
        color = "blue", fillColor = "blue", fillOpacity = 0,
        layerId = "outerCircle"
      ) %>%
      addCircles(
        lng = lng, lat = lat, radius = INNER_RADIUS,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.2,
        layerId = "innerCircle"
      )
    
    center <- st_as_sf(data.frame(lng=lng, lat=lat),
                       coords = c("lng", "lat"), crs = 4326)
    center <- st_transform(center, 2169)

    center_coords <- st_coordinates(center)

    maybetic("Find outer obs")
    idx_outer <- lv_points_in_circle(tree, center_coords, OUTER_RADIUS)
    points_outer <- all_points[idx_outer,]
    species_count_outer <- as.data.frame(table(points_outer$preferred))
    maybetoc()

    maybetic("Find outer obs")
    idx_inner <- lv_points_in_circle(tree, center_coords, INNER_RADIUS)
    points_inner <- all_points[idx_inner,]
    species_count_inner <- as.data.frame(table(points_inner$preferred))
    maybetoc()

    maybetic("Calc diff")
    species_annulus <- setdiff(species_count_outer$Var1, species_count_inner$Var1)
    maybetoc()

    maybetic("Histogram")
    species_count_annulus <- species_count_outer[species_count_outer$Var1 %in% species_annulus,]
    species_count_annulus <- species_count_annulus[order(species_count_annulus$Freq, decreasing = TRUE),]
    maybetoc()

    maybetic("Render species")
    output$speciesInfo <- renderUI({
      if (nrow(species_count_annulus) == 0) {
        return(h4("No species within the outer but not inner circle."))
      } else {
        layout_column_wrap(
              width="200px", fixed_width=TRUE,
              !!!lapply(seq_len(nrow(species_count_annulus)), function(i) {
                  taxon_name <- species_count_annulus$Var1[i]
                  taxon_count <- species_count_annulus$Freq[i]
                  src <- "no-image.png"

                  ti <- find_taxon_info(taxon_info, taxon_name)[1,]

                  if (nrow(ti) > 0 && !is.na(ti$imageUrl)) {
                      src <- thumb(ti$imageUrl)
                  }

                  if (!is.na(ti$wikipediaLinkEn)) {
                      tagEn <- tags$dd(tags$a(href=ti$wikipediaLinkEn,
                                              target="_blank", ti$commonNameEn))
                  } else {
                      tagEn <- tags$dd(ti$commonNameEn)
                  }

                  if (!is.na(ti$wikipediaLinkFr)) {
                      tagFr <- tags$dd(tags$a(href=ti$wikipediaLinkFr,
                                              target="_blank", ti$commonNameFr))
                  } else {
                      tagFr <- tags$dd(ti$commonNameFr)
                  }

                  if (!is.na(ti$wikipediaLinkDe)) {
                      tagDe <- tags$dd(tags$a(href=ti$wikipediaLinkDe,
                                              target="_blank", ti$commonNameDe))
                  } else {
                      tagDe <- tags$dd(ti$commonNameDe)
                  }

                  if (!is.na(ti$wikipediaLinkLb)) {
                      tagLb <- tags$dd(tags$a(href=ti$wikipediaLinkLb,
                                              target="_blank", ti$commonNameLb))
                  } else {
                      tagLb <- tags$dd(ti$commonNameLb)
                  }

                  card(
                      card_header(taxon_name),
                      card_body(
                          tags$img(src=src),
                          tags$dl(
                                   tags$dt("English: "), tagEn,
                                   tags$dt("Français: "), tagFr,
                                   tags$dt("Deutsch: "), tagDe,
                                   tags$dt("Lëtzebuergesch: "), tagLb,
                                   tags$dt("Count in donut"), tags$dd(taxon_count),
                                   tags$dt("Higher taxonomy"),
                                   tags$div(class="small",
                                                  ti$class, ">",
                                                  ti$order, ">",
                                                  ti$family)
                               )
                      )
                  )
              })
          )
      }
    })
  })
  maybetoc()

  maybetic("Leaflet")
  output$map <- renderLeaflet({
      leaflet() %>%
          addTiles() %>%
          setView(lng = 6.13, lat = 49.61, zoom = 12) #%>%
          #addMarkers(data = all_points, popup = ~name)
  })
  maybetoc()
}
