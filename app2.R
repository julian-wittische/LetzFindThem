library(shiny)

# Direct URLs pointing to actual image files
image_urls <- c(
  "https://upload.wikimedia.org/wikipedia/commons/9/9d/AsianElephants_CincinnatiZoo.jpg",  # Earth image
  "https://upload.wikimedia.org/wikipedia/commons/6/61/Hubro_%28Bubo_bubo%29.JPG"  # Moon image
)

# UI
ui <- fluidPage(
  titlePanel("Sample Images"),
  fluidRow(
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px;",
      lapply(image_urls, function(url) {
        tags$img(src = url, alt = "Sample image", style = "width: 100%; height: auto;")
      })
    )
  )
)

# Server
server <- function(input, output) {}

# Run the app
shinyApp(ui, server)
