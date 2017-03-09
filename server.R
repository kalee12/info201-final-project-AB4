# # Karen

x <- c("$5",  "$44",  "$66")
strsplit(x, " ")

sub("\\$","", x)


x <- "Split the words in a sentence."
strsplit(x, "$")

library(leaflet)
library(DT)
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
  
  output$introduction <- renderPrint({
    div (
      HTML("<h2> Our Mission: <h2/>
           <br/>
           Nursing Homes are one of the fastest growing practices in the country today. With 60,000
           nursing homes currently running in the country, it is difficult to find one that best suits everyone needs. The nursing home
           finder is the end to all of your problems. This project is intended to ease the search process and help adults (25 and above)
           find a perfect nursing home for their parent(s). We have based this project on the official datasets available on the
           <a href= 'https://www.medicare.gov/'> Medicare.gov </a> Nursing Home Compare Website provided by the Centers for Medicare & Medicaid Services. This dataset allows one to compare
           the quality of care at every Medicare and Medicaid-certified Nursing Home currently running in the country.
           <br/> <br/>
           <h2> Navigating through the application: <h2/> <br/>
           This tool will help you compare nursing homes in 50 states. Initially you would see the national map with a table
           showing the basic provider information(Name, Phone Number, Address, City, State, Fines and Overall Rating).
           You can choose a state from the drop down on the side, and you would see a close up map of your state,
           with location maps for each nursing home. You can hover over the location pins to see the names of the nursing homes.
           Below the map, the table is filtered to the specific state’s nursing homes. You can also filter the overall rating
           (from 1 to 5), and see the nursing homes with/without fines or all of them, according to your choices.
           This will further filter the location points and the table.
           <br/> <br/>
           You can select a specific nursing home at a time, and then toggle between the General Provider Information,
           Ratings and Penalties and Other Information for the specific nursing home on the side. The General Information tab
           will show the Name, Address and the Phone Number of the nursing homes. The Ratings and Penalties tab will show more
           specific ratings and penalties such as Overall Rating, Health Inspection Rating, Staffing Rating,
           Registered Nurse Staffing Rating and the Total Amount of fine due. The Other Information tab has
           information on the maximum capacity of residents(that is, the number of certified beds), the current residents,
           the reported hours out of expected hours for Certified Nursing Assistant(CNA), Licesened Practical Nurses (LPN),
           Registered Nurses(RN) and for all nurses.
           <br/> <br/>
           You also have the ability to search the table and select the number of specific entires you would like to see in the table.
           There is a glossary available for you to help understand the difference between the different types of Nurses available and
           their respective information given in the table."
                                  )
                                )
  })
  
  output$pie.chart.summary <- renderPrint({
    
    ###### Insert summary 1 here ############
  })
  
  output$point.graph.summary <- renderPrint({
    
    ###### Insert summary 2 here ############
  })
  
  output$logo <- renderImage({
    list(src = "data/info201logo.png",
         contentType = 'image/png',
         width = 500,
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  
  output$lemap <- renderLeaflet({
    houses.data <- table.filter()
    #if (input$state != "National") {
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
    
    if (input$state != "National") {
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 60, iconHeight = 50
      )
      m <- leaflet(data = points) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(points$long[1], points$lat[1], zoom = 6) %>%
        addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
    } else {
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 30, iconHeight = 20
      )
      m <- leaflet(data = points) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.712891, 37.090240, zoom = 4) %>% 
        addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
    }
    return(m)
  })
  
  output$summary <- renderText({
    num.homes <- nrow(table.filter())
    paste("There are", num.homes, "homes in", input$state)
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
    else {
      cat("Select an observation on the table below for general data summary. ")
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
    else {
      cat("Select an observation on the table below for ratings/penalty data summary. ")
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
    else {
      cat("Select an observation on the table below for data summary. ")
    }
  })
  
  output$pie <- renderPlot({
    if (input$state != "National") {
      data <- filter(houses.info, State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses.info)
    }
    pie <- ggplot(data, mapping = aes(x = factor(1), fill = factor(Overall.Rating))) + geom_bar(width = 1) + 
      coord_polar(theta = "y") + scale_fill_brewer(palette = "RdYlGn") + 
      labs(x = "", y = "", fill = "Overall Rating")
    
    #pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    #  geom_bar(width = 1)
    #pie <- pie + coord_polar(theta = "y")
    
    #pie <- ggplotly(pie)
    return(pie)
  })
  
  output$bar <- renderPlot({
    col <- sub("\\$","", houses$Total.Amount.of.Fines.in.Dollars)
    col <- sub("\\.00", "", col)
    col <- as.numeric(col) / 10000
    
    houses <- mutate(houses, Total.Fines = col)
    if (input$state != "National") {
      data <- filter(houses, Provider.State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses)
    }
    #print(colnames(houses))
    
    
    
    bar <- ggplot(data, mapping = aes(x = Overall.Rating, y = Total.Fines, color = Overall.Rating)) + geom_point(size = 5) +
      labs(x = "Overall Rating", y = "Fine in (Ten-Thousand) Dollars", fill = "Overall Rating") + guides(color = FALSE) +
      theme(axis.title = element_text(size = 16))
    return(bar)
  })
  
  output$viz <- renderText({
    return(paste("Frequency of Ratings in", input$state))
  })
  
  output$viz2 <- renderText({
    return(paste("Comparison between Rating and something", input$state))
  })
}

shinyServer(server)