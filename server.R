#library(choroplethrMaps)
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

houses.info <- houses %>% select(Federal.Provider.Number, Provider.Name, Provider.Phone.Number, 
                                 Provider.Address, Provider.City, Provider.State, 
                                 Number.of.Fines, Overall.Rating, Location)

colnames(houses.info) <- c("Federal.Provider.Number", "Name", "Phone.Number", "Address", "City", 
                           "State", "Number.of.Fines", "Overall.Rating", "Location")

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

fines <- c()

for(i in 1:nrow(houses.info)){
  if(houses.info$Number.of.Fines[i] == "0"){
    fines <- append(fines, "No")
  }else{
    fines <- append(fines, "Yes")
  }
}

houses.info <- mutate(houses.info, Fines = fines) %>% select(-Number.of.Fines, -Federal.Provider.Number)

#View(houses.info)
server <- function(input, output) {
  
  table.filter <- reactive({
    
    
    if (input$state != "National") {
      houses.info <- filter(houses.info, State == state.abb[match(input$state, state.name)])
    }
    
    houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, Overall.Rating, Location)
    
    houses.info <- houses.info %>% filter(Overall.Rating >= input$ratings[1] & Overall.Rating <= input$ratings[2]) 
    
    if(input$radio == 1){
      houses.info<- houses.info %>% filter(Fines == "Yes")
    }else if(input$radio == 2){
      houses.info<- houses.info %>% filter(Fines == "No")
    }else{
      houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, Overall.Rating, Location)
      
    }
    
    return(houses.info)
  }
  )
  
  output$logo <- renderImage({
    list(src = "data/info201logo.png",
         contentType = 'image/png',
         width = 200,
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  
  output$lemap <- renderLeaflet({
    houses.data <- table.filter()
    if (input$state != "National") {
      # Filters state map data for selected state
      #state <- filter(data, region == tolower(input$state))
      # Filters nursing home data for selected state
      houses.data.location <- houses.data$Location
      long <- vector()
      lat <- vector()
      for (i in 1:length(houses.data$Location)) {
        long <- c(long, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[3]))
        lat <- c(lat, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[2]))
      }
      points <- na.omit(data.frame(houses.data$Name, houses.data$Overall.Rating, long, lat, stringsAsFactors = FALSE))
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 60, iconHeight = 50
      )
      m <- leaflet(data = points) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(points$long[1], points$lat[1], zoom = 6) %>%
        addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
      
    } else {
      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.712891, 37.090240, zoom = 3)
    }
    return(m)
  })
  
  output$table <- DT::renderDataTable(select(table.filter(), -Location), server = TRUE, selection = "single")
  
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
    total <- houses$Total.Amount.of.Fines.in.Dollars[s]
    if (length(s)) {
      if(houses.info$Fines[s] == "Yes"){
        cat("Total amount of fines in dollars:", total)
      }else{
        cat("No fines")
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

