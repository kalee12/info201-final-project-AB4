server <- function(input, output) {

  output$logo <- renderImage({
    list(src = "data/info201logo.png",
         contentType = 'image/png',
         width = 200,
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  }
shinyServer(server)