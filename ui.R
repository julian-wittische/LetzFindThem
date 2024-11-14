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
dt, dd {
  display: inline;
}
'),
  div(
    leafletOutput("map", width = "100%", height = "50vh"),
    style = "height: 50vh;"
  ),
  div(
    style = "padding: 20px;",
    uiOutput("speciesInfo"),
  )
)
