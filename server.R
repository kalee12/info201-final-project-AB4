# Simran

# loading the different libraries required
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)

# using the data set for state map and names required for filtering
data(state.map)
data(state)

# loading the data for Ratings of the Nursing Homes
ratings <- read.csv("data/Star_Ratings.csv", stringsAsFactors = FALSE)

# loading the data for the Penalties incurred by the Nursing Homes
penalty <- read.csv("data/Penalties.csv", stringsAsFactors = FALSE)

# loading the data for the Provider Information for each active Nursing home
houses <- read.csv("data/Provider_Info.csv", stringsAsFactors = FALSE) 

# creating a new data set selecting the necessary provider information for each house
houses.info <- houses %>% select(Federal.Provider.Number, Provider.Name, Provider.Phone.Number, 
                                 Provider.Address, Provider.City, Provider.State, 
                                 Number.of.Fines, Overall.Rating, Location)

# renaming the column names for user readabuility
colnames(houses.info) <- c("Federal.Provider.Number", "Name", "Phone.Number", "Address", "City", 
                           "State", "Number.of.Fines", "Overall.Rating", "Location")

# creating a new data set selecting the specific statistics for each house
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

# renaming the column names for user readability
colnames(general.info) <- c("name", "phone.number", "address", "city", "state", 
                            "certified.beds", "residents.in.beds", "reported.cna",
                            "reported.lpn", "reported.rn", "reported.total",
                            "expected.cna", "expected.lpn", "expected.rn", "expected.total")

# function to help assign whether the houses have a penalty or not, by creating a vector
fines <- c()
for(i in 1:nrow(houses.info)){
  if(houses.info$Number.of.Fines[i] == "0"){
    fines <- append(fines, "No")
  }else{
    fines <- append(fines, "Yes")
  }
}

# adding the above vector as a column in the provider information table
houses.info <- mutate(houses.info, Fines = fines) %>% select(-Number.of.Fines, -Federal.Provider.Number)

# defining the server function
server <- function(input, output) {
  
  # creating the table for display, which changes according to the state filter
  table.filter <- reactive({
    
    # changing the data set if it is not national, according to the input of the state
    if (input$state != "National") {
      houses.info <- filter(houses.info, State == state.abb[match(input$state, state.name)])
    }
    
    # displaying the contact information and basic rating and penalties for the nursing homes selected
    houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, Overall.Rating, Location)
    
    # filtering the table according the filter chosen
    houses.info <- houses.info %>% filter(Overall.Rating >= input$ratings[1] & Overall.Rating <= input$ratings[2]) 
    
    # changing the table on the basis of penalty option (with, without or all)
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
  
  # displaying the logo for our project
  output$logo <- renderImage({
    list(src = "data/info201logo.png",
         contentType = 'image/png',
         width = 500,
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  
  # displaying the map of the country or state according to the filter chosen
  output$lemap <- renderLeaflet({
    
    # filtering the table according to the location chosen
    houses.data <- table.filter()
    houses.data.location <- houses.data$Location
    
    # vectors for longititude and latitude
    long <- vector()
    lat <- vector()
    
    # Extracts coordinates from given location information
    for (i in 1:length(houses.data$Location)) {
      long <- c(long, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[3]))
      lat <- c(lat, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i], "\n"))[3], "[(),]"))[2]))
    }
    
    # a data frame for the name and the overall rating of the nursing home
    points <- na.omit(data.frame(houses.data$Name, houses.data$Overall.Rating, long, lat, stringsAsFactors = FALSE))
    
    # setting icons for the nursing houses shown
    icon <- makeIcon(
      iconUrl = "data/pin.png",
      iconWidth = 60, iconHeight = 50
    )
    
    # sets the map according to the nursing homes, setting the view according to latitude and longitude chosen,
    # adds markers and labels for each nursing home
    m <- leaflet(data = points) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(points$long[1], points$lat[1], zoom = 6) %>%
      addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
    
    return(m)
  })
  
  # summary for number of nursing homes shown in the map of the state chosen
  output$summary <- renderText({
    num.homes <- nrow(table.filter())
    paste("There are", num.homes, "nursing homes in", input$state)
  })
  
  # shows the table with provider information for the nursing homes of the chosen states
  output$table <- DT::renderDataTable(select(table.filter(), -Location), server = TRUE, selection = "single")
  
  # prints the contact information summary for the chosen specific house
  output$general <- renderPrint({
    
    # represents the selected nursing home by the user
    s = input$table_rows_selected
    
    # the name of the selected nursing home
    name <- houses$Provider.Name[s]
    
    # the address of the selected nursing home
    address <- houses$Provider.Address[s]
    
    # the city of the selected nursing home
    city <- houses$Provider.City[s]
    
    # the State of the selected nursing home
    state <- houses$Provider.State[s]
    
    # the contact phone number of the selected nursing home
    phone <- houses$Provider.Phone.Number[s]
    
    # the zip code of the location of the selected nursing home
    zip <- houses$Provider.Zip.Code[s]
    
    # outputs the summary of the address and phone number of the selected nursing home
    if (length(s)) {
      cat(name, "\n\n", 
          "Address: ",address, ", ", city, ", ", state, zip, "\n\n",
          "Phone Number: ", phone )
    }
  })
  
  # prints the rating summary of the selected nursing home by the user
  output$ratings <- renderPrint({
    
    # represents the selected nursing home by the user
    s = input$table_rows_selected
    
    # the overall rating of the selected nursing home
    overall <- ratings$Overall.Rating[s]
    
    # the health inspection rating of the selected nursing home
    health <- ratings$Health.Inspection.Rating[s]
    
    # the staffing  rating of the selected nursing home
    staff <- ratings$Staffing.Rating[s]
    
    # the registered nurse rating of the selected nursing home
    rn <- ratings$RN.Staffing.Rating[s]
    
    # prints the summary of the overall rating, the health inspection rating, 
    # the staffing rating and the registered nurses staffing rating
    if (length(s)) {
      cat("Overall Rating: ",overall, "\n\n", 
          "Health Inspection Rating: ", health, "\n\n",
          "Staffing Rating: ",staff, "\n\n", 
          "RN Staffing Rating: ", rn)
    }
  })
  
  # prints the summary of the penalties charged to the selected nursing home by the user
  output$penalties <- renderPrint({
    
    # represents the selected nursing home by the user
    s = input$table_rows_selected
    
    # the type of penalty charged by the selected nursing home
    type <- penalty$Penalty.Type[s]
    
    # the amount of fine paid by the selected nursing home
    amount <- penalty$Fine.Amount[s]
    
    # the total amount of fines in dollars paid by the selected nursing hoem
    total <- houses$Total.Amount.of.Fines.in.Dollars[s]
    
    # outputs the total amount of fines for the selected nursing home, depending 
    # whether the nursing home has a fine or not
    if (length(s)) {
      if(houses.info$Fines[s] == "Yes"){
        cat("Total amount of fines in dollars:", total)
      }else{
        cat("No fines")
      }
    }
  })
  
  # outputs the summary of the important relavent general information not covered in the 
  # above information for the selected nursing home by the user
  output$other <- renderPrint({
    
    # represents the selected nursing home by the user
    s = input$table_rows_selected
    
    # the maximum capacity in the selected nursing home
    number.beds <- general.info$certified.beds[s]
    
    # the number of current residents in the selected nursing home
    residents.in.beds <- general.info$residents.in.beds[s]
    
    # the reported working hours of the certified nurse for the selected nursing home
    reported.cna <- round(general.info$reported.cna[s], 2)
    
    # the reported working hours of the licensed practical nurse for the selected nursing home
    reported.lpn <- round(general.info$reported.lpn[s], 2)
    
    # the reported working hours of the registered nurse for the selected nursing home
    reported.rn <- round(general.info$reported.rn[s], 2)
    
    # the reported working hours of all of the nurses in the selected nursing home
    reported.total <- round(general.info$reported.total[s], 2)
    
    # the expectecd working hours for the certified nurses in the selected nursing home
    expected.cna <- round(general.info$expected.cna[s], 2)
    
    # the expectecd working hours for the licensed practical nurses in the selected nursing home
    expected.lpn <- round(general.info$expected.lpn[s], 2)
    
    # the expectecd working hours for the registered nurses in the selected nursing home
    expected.rn <- round(general.info$expected.rn[s], 2)
    
    # the expectecd working hours for all of the nurses in the selected nursing home
    expected.total <- round(general.info$expected.total[s], 2)
    
    # prints the above information for the selected nursing home
    if (length(s)) {
      cat("Number of Residents in Certified Beds: ", residents.in.beds, "\n\n",
          "Number of Certified Beds: ", number.beds, "\n\n",
          reported.cna, "reported CNA hours out of", expected.cna, "expected hours", "\n\n",
          reported.lpn, "reported LPN hours out of", expected.lpn, "expected hours", "\n\n",
          reported.rn, "reported RN hours out of", expected.rn, "expected hours", "\n\n",
          reported.total, "reported total nurse hours out of", expected.total, "expected hours", "\n\n")
    }
  })
  
  # outputs a pie chart for the frequencies of overall ratings for the nursing homes in the selected location
  output$pie <- renderPlot({
    
    # according to the selected state it filters the information for the bursing home
    if (input$state != "National") {
      data <- filter(houses.info, State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else { 
      data <- na.omit(houses.info)
    }
    
    # plots a pie chart for the overall rating frequency of nursing homes in each state
    pie <- ggplot(data, mapping = aes(x = factor(1), fill = factor(Overall.Rating))) + geom_bar(width = 1) + 
      coord_polar(theta = "y") + scale_fill_brewer(palette = "RdYlGn") + 
      labs(x = "", y = "", fill = "Overall Rating")
    return(pie)
  })
  
  output$bar <- renderPlot({
    col <- sub("\\$","", houses$Total.Amount.of.Fines.in.Dollars)
    col <- sub("\\.00", "", col)
    col <- as.numeric(col)
    
    houses <- mutate(houses, Total.Fines = col)
    if (input$state != "National") {
      data <- filter(houses, Provider.State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses)
    }
    bar <- ggplot(data, mapping = aes(x = Overall.Rating, y = Total.Fines, fill = Overall.Rating)) + geom_col() + 
      labs(x = "Overall Rating", y = "Fine in Dollars", fill = "Overall Rating")
    return(bar)
  })
  
  # title and description for the pie chart plotted 
  output$viz <- renderText({
    return(paste("Frequency of Ratings in", input$state))
  })
  
  output$viz2 <- renderText({
    return(paste("Comparison between Rating and something", input$state))
  })
}

shinyServer(server)