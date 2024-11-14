dim(mdata)
# PROBLEM: not the expected number of rows

###### Where is the problem?
dim(read.csv(files[1], encoding = "latin1")) # PROBLEM (403964) SHOULD BE MORE
dim(read.csv(files[2], encoding = "latin1")) # Fine
dim(read.csv(files[3], encoding = "latin1")) # Fine
dim(read.csv(files[4], encoding = "latin1")) # Fine
dim(read.csv(files[5], encoding = "latin1")) # Fine
dim(read.csv(files[6], encoding = "latin1")) # Fine

### Different fields?
# Check the number of fields in each line
fields <- count.fields(files[1], sep = ",")

# Find lines with an inconsistent number of fields
inconsistent_lines <- which(fields != max(fields))

# Output inconsistent lines for inspection
inconsistent_lines

test <- read.csv(files[1], encoding = "windows-1252")
dim(test)
test <- read.csv(files[1], encoding = "latin1")
dim(test)
lol <- read.csv(files[2], encoding = "latin1")
colnames(test) == colnames(lol)
# RESULT: no problem with column names or number

### Bad lines?
# Read the file line by line
lines <- readLines(files[1], warn = TRUE, encoding="latin1")
linesnoinsidecommas <- gsub("\\\\,", ";", lines)
#raw <- gsub("\\\"\\\"", "\"", lines)
lines2 <- read.table(text=raw, header = TRUE, sep=",")

# Loop through each line to detect where issues might arise
for (i in seq_along(lines[1:5])) {
  tryCatch({
    # Try parsing each line individually
    temp <- read.csv(text = lines[i], stringsAsFactors = FALSE, encoding="latin1")
  }, warning = function(w) {
    message("Warning in line ", i, ": ", conditionMessage(w))
  }, error = function(e) {
    message("Error in line ", i, ": ", conditionMessage(e))
  })
}
# RESULT: Plenty of invalid multibyte strings at different positions in the line

lines[2:4]

# TESTS
find_offending_character <- function(x, maxStringLength=256){  
  print(x)
  for (c in 1:maxStringLength){
    offendingChar <- substr(x,c,c)
    print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }    
}

lapply(lines[1], find_offending_character)
lapply(lines[4], find_offending_character)

test <- read.csv(files[1], fileEncoding = "latin1", encoding= "latin1")
dim(test)

lol <- read.csv(files[2], encoding = "latin1")

text <- "priorit\xe9 2"
# Convert the encoding to UTF-8
corrected_text <- iconv(text, from = "latin1", to = "UTF-8")
print(corrected_text)  # Should output "priorité 2"

options(encoding = "windows-1252")
test <- read.csv(files[1], fileEncoding = "latin1", quote="\"", sep=",")
dim(test)

testtext <- read.table("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA/-1997TESTING.txt", header=TRUE, sep="\t", encoding = "latin1")



###### Redo of the original idea after correction
files <- list.files("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA", full.names = TRUE)[c(1, 2, 3, 4, 5, 7)]
mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))
dim(mdata)

testt <- fread("W:/01_Services/SCR_Informations Patrimoine Naturel/00_Projects/2024_LetzFindThem_DATA/-1997TESTING.csv", encoding = )

########################### leaflet heatmap
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
library(leaflet.extras)
library(RColorBrewer)
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

taxon_card <- function(species_count) {
  src <- "no-image.png"
  
  taxon_name <- species_count$Var1
  taxon_count <- species_count$Freq
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
      tags$dl(tags$dt("English: "), tagEn,
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
}

# Define the server logic
server <- function(input, output, session) {
  output$speciesInfo <- renderUI({
    h4("Click anywhere on the map to see what species you can probably find in the inner circle.")
  })
  
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
    if (nrow(points_outer) == 0) {
      output$speciesInfo <- renderUI({
        h4("No observations recorder here.")
      })
      return()
    }
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
        h4("No species within the outer but not inner circle.")
      } else {
        lapply(seq_len(nrow(species_count_annulus[1:min(nrow(species_count_annulus),TAXA_DISPLAY_MAX),])), function(i) {
          taxon_card(species_count_annulus[i,])
        })
      }
    })
  })
  maybetoc()
  
  maybetic("Leaflet")
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addHeatmap(lng = coords[,1], lat = coords[,2],
                 blur = 1.5, max = 1, radius = 1.5, minOpacity=0.25,
                 gradient= brewer.pal(9, "BuPu") ) %>%
      setView(lng = 6.13, lat = 49.61, zoom = 12) #%>%
    #addMarkers(data = all_points, popup = ~name)
  })
  maybetoc()
}

# in global
a <- st_transform(all_points, crs = 4326)
coords <- st_coordinates(a)