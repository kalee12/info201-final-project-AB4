library(choroplethrMaps)
library(leaflet)

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)

data(state.map)
data(state)
ratings <- read.csv("data/Star_Ratings.csv", stringsAsFactors = FALSE)

penalty <- read.csv("data/Penalties.csv", stringsAsFactors = FALSE)

houses <- read.csv("data/Provider_Info.csv", stringsAsFactors = FALSE) 

houses.info <- houses %>% select(Provider.Name, Provider.Phone.Number, 
                                 Provider.Address, Provider.City, Provider.State)

colnames(houses.info) <- c("Name", "Phone.Number", "Address", "City", "State")

general.info <- houses %>% select(Provider.Name, Provider.Phone.Number, 
                                  Provider.Address, Provider.City, Provider.State,
                                  Number.of.Certified.Beds, 
                                  Number.of.Residents.in.Certified.Beds,
                                  Reported.CNA.Staffing.Hours.per.Resident.per.Day,
                                  Reported.LPN.Staffing.Hours.per.Resident.per.Day,
                                  Reported.RN.Staffing.Hours.per.Resident.per.Day, 
                                  Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day,
                                  Expected.CNA.Staffing.Hours.per.Resident.per.Day,
                                  Expected.LPN.Staffing.Hours.per.Resident.per.Day,
                                  Expected.RN.Staffing.Hours.per.Resident.per.Day, 
                                  Expected.Total.Nurse.Staffing.Hours.per.Resident.per.Day)
colnames(general.info) <- c("name", "phone.number", "address", "city", "state", 
                            "certified.beds", "residents.in.beds", "reported.cna",
                            "reported.lpn", "reported.rn", "reported.total",
                            "expected.cna", "expected.lpn", "expected.rn", "expected.total")
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
        setView(-95.712891, 37.090240, zoom = 3)
    }
    return(m)
  })
  
  output$table <- DT::renderDataTable(houses.info, server = TRUE, selection = "single")
  
  # print the selected indices
  output$general <- renderPrint({
    s = input$table_rows_selected
    name <- houses$Provider.Name[s]
    address <- houses$Provider.Address[s]
    city <- houses$Provider.City[s]
    state <- houses$Provider.State[s]
    phone <- houses$Provider.Phone.Number[s]
    zip <- houses$Provider.Zip.Code[s]
    if (length(s)) {
      cat(name, "\n\n", 
          "Address: ",address, ", ", city, ", ", state, zip, "\n\n",
          "Phone Number: ", phone )
    }
  })
  output$ratings <- renderPrint({
    s = input$table_rows_selected
    overall <- ratings$Overall.Rating[s]
    health <- ratings$Health.Inspection.Rating[s]
    staff <- ratings$Staffing.Rating[s]
    rn <- ratings$RN.Staffing.Rating[s]
    if (length(s)) {
      cat("Overall Rating: ",overall, "\n\n", 
          "Health Inspection Rating: ", health, "\n\n",
          "Staffing Rating: ",staff, "\n\n", 
          "RN Staffing Rating: ", rn)
    }
  })
  output$penalties <- renderPrint({
    s = input$table_rows_selected
    type <- penalty$Penalty.Type[s]
    amount <- penalty$Fine.Amount[s]
    denial <- penalty$Payment.Denial.Length.in.Days[s]
    if (length(s)) {
      if(type == "Fine"){
        
        cat("Penatly Type: ", type, "\n\n", "Fine Amount: ", amount)
        
      }else if(type == "Payment Denial"){
        
        cat("Penatly Type: ", type, "\n\n", "Payment Denial Length in Days: ", denial)
        
      }
    }
    
  })
  
  output$other <- renderPrint({
    s = input$table_rows_selected
    number.beds <- general.info$certified.beds[s]
    residents.in.beds <- general.info$residents.in.beds[s]
    reported.cna <- round(general.info$reported.cna[s], 2)
    reported.lpn <- round(general.info$reported.lpn[s], 2)
    reported.rn <- round(general.info$reported.rn[s], 2)
    reported.total <- round(general.info$reported.total[s], 2)
    expected.cna <- round(general.info$expected.cna[s], 2)
    expected.lpn <- round(general.info$expected.lpn[s], 2)
    expected.rn <- round(general.info$expected.rn[s], 2)
    expected.total <- round(general.info$expected.total[s], 2)
    if (length(s)) {
      cat("Number of Residents in Certified Beds: ", residents.in.beds, "\n\n",
          "Number of Certified Beds: ", number.beds, "\n\n",
          reported.cna, "reported CNA hours out of", expected.cna, "expected hours", "\n\n",
          reported.lpn, "reported LPN hours out of", expected.lpn, "expected hours", "\n\n",
          reported.rn, "reported RN hours out of", expected.rn, "expected hours", "\n\n",
          reported.total, "reported total nurse hours out of", expected.total, "expected hours", "\n\n")
    }
  })
  
}

shinyServer(server)