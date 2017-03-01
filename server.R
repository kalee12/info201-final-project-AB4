server <- function(input, output) {
  
  output$summary <- renderText({
    paste("RN = registered nurse",
          "CNA = certified nurse assistent", 
          "LPN = Licensed practical nurse", sep = "\n")
  })
  
  output$table <- renderDataTable({
    data <- info %>% select(Provider.State)
    return(data)
  })
}
shinyServer(server)