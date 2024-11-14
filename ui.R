library(shiny)
library(bslib)
library(leaflet)

# Define the UI
ui <- page_fluid(
    tags$style(type = "text/css", '
#map {
  height: 40vh; 
  width: 100%;
}

dt:before {
  content: "";
  display: block;
}
.card {
  width: 250px;
  margin-right: 20px;
}
img {
  max-height: 140px;
  max-width: 100%;
  width: auto  ;
  height: auto;
  object-fit: contain;
}
'),
  div(
    leafletOutput("map", width = "100%", height = "50vh"),
    style = "height: 50vh;"
  ),
  div(
    style = "padding-top: 20px; display: flex; flex-wrap: wrap;",
    uiOutput("speciesInfo")
  )
)
