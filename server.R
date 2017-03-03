# Karen

#install.packages("choroplethrMaps")
#install.packages("leaflet")
library(choroplethrMaps)
library(leaflet)

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)

data(state.map)
data(state)
houses <- read.csv("data/Provider_Info.csv", stringsAsFactors = FALSE) %>% arrange(Federal.Provider.Number)

server <- function(input, output) {
  data <- state.map
  interest <- reactive({
    if (input$state != "National") {
      return(houses.data <- filter(houses, Provider.State == state.abb[match(input$state, state.name)]))
    } else {
      return(houses)
    }
  })
  
  output$map <- renderPlot({
    data <- state.map
    p <- ggplot(data = data) + geom_polygon(aes(x = long, y = lat, group = group)) + coord_fixed(1.3)
    houses.data <- interest()
    if (input$state != "National") {
      # Filters state map data for selected state
      state <- filter(data, region == tolower(input$state))
      # Filters nursing home data for selected state
      houses.data.location <- houses.data$Location
      long <- vector()
      lat <- vector()
      for (i in 1:length(houses.data$Location)) {
        long <- c(long, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[3]))
        lat <- c(lat, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[2]))
      }
      points <- na.omit(data.frame(houses.data$Provider.Name, long, lat, stringsAsFactors = FALSE))
      print(length(houses$Provider.Name))
      print(colnames(houses.data))
      
      p <- ggplot() + geom_polygon(data = state, aes(x = long, y = lat, group = group)) + coord_fixed(1.3) +
        geom_point(data = points, aes(x = long, y = lat), color = "blue", size = 3) +
        geom_point(data = points, aes(x = long, y = lat), color = "black", size = 2)
      g <- list(scope = "usa")
      x <- plot_geo(data = points, x = ~long, y = ~lat) %>% layout(geo = g)
    }
    
    return(x)
    #return(p)
  })
  
  output$lemap <- renderLeaflet({
    houses.data <- interest()
    if (input$state != "National") {
      # Filters state map data for selected state
      state <- filter(data, region == tolower(input$state))
      # Filters nursing home data for selected state
      houses.data.location <- houses.data$Location
      long <- vector()
      lat <- vector()
      for (i in 1:length(houses.data$Location)) {
        long <- c(long, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[3]))
        lat <- c(lat, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[2]))
      }
      points <- na.omit(data.frame(houses.data$Provider.Name, long, lat, stringsAsFactors = FALSE))
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 60, iconHeight = 50
      )
      m <- leaflet(data = points) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(points$long[1], points$lat[1], zoom = 6) %>% 
        addMarkers(~long, ~lat, popup = "hi", label = "hello", icon = icon)
      
    } else {
      m <- leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(39.0119, 98.4842, zoom = 1)
    }
    return(m)
  })
  
  output$text <- renderText({
    return(input$state)
  })
  
  output$house <- renderText({
    return("something here")
  })
  
  output$table <- renderDataTable({
    return(interest())
  })
}

shinyServer(server)

#unlist(strsplit(unlist(strsplit(dummy$Location[1], "\n"))[3], "[(),]"))
