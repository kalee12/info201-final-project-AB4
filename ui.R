library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
ratings.range<- range(1,5)
data("state")
state.name
customSentence <- function(numItems, type) {
  strong("Data abbrevations defined (Scrollable):")
}
# Function to call in place of dropdownMenu
dropdownMenuCustom <- function (..., type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

ui <- fluidPage(
  imageOutput("logo", height = "200px", width = "600px"), 
  dashboardPage(skin = "blue", 
                dashboardHeader(title = "Dashboard",
                                dropdownMenuCustom(type = "messages", icon = em(strong("Glossory")), 
                                                   customSentence = customSentence, 
                                                   messageItem(
                                                     from = "Certified Nursing Assistant (CNA):",
                                                     message = HTML("Helps patients or clients with healthcare <br/>needs
                        under the supervision of Registered <br/>Nurse (RN) or a 
                        Practical Nurse <br/> (LPN)."),
                                                     icon = icon("user-md")
                                                   ),
                                                   
                                                   messageItem(
                                                     from = "Registered Nurse (RN):",
                                                     message = HTML("A nurse who
                        has graduated from a <br/>nursing program and has sucesfully
                                              <br/>obtained a license."),
                                                     icon = icon("user-md")                 
                                                   ),
                                                   messageItem(
                                                     from = "Licensed Practical Nurse (LPN):",
                                                     message = HTML("A nurse who cares for
                        people who are sick, <br/>injured, convalescent, or disabled. LPNs <br/>
                        work under the direction of registered <br/>nurses or physicians."),
                                                     icon = icon("user-md") 
                                                   )
                                )
                ),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("About", tabName = "introduction", icon = icon("users")),
                    menuItem("Find Your Home", tabName = "datatable", icon = icon("search")),
                    menuItem("Data Overview", tabName = "graph", icon = icon("table")),
                    selectInput("state", "Location", c("National", state.name)),
                    # selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                    sliderInput("ratings", "Filter by Nursing Home Ratings:",
                                min=ratings.range[1], max=ratings.range[2], value=ratings.range),
                    radioButtons("radio", "Filter by Fines:",
                                 choices = list("Has a fine" = 1,"Doesn't have a fine" = 2, "All" = 3),selected = 3
                    )
                  )
                ),
                dashboardBody(
                  box(width = 20, status = "primary",
                      tabItems(
                        # First tab content
                        tabItem(tabName = "introduction",
                                div (
                                  HTML("Nursing Homes are one of the fastest growing practices in the country today. With 60,000 
                                       nursing homes currently running in the country, it is difficult to find one that best suits everyone needs. The nursing home
                                       finder is the end to all of your problems. This project is intended to ease the search process and help adults (25 and above) 
                                       find a perfect nursing home for their parent(s). We have based this project on the official datasets available on the
                                       <a href= 'https://www.medicare.gov/'> Medicare.gov </a> Nursing Home Compare Website provided by the Centers for Medicare & Medicaid Services. This dataset allows one to compare
                                       the quality of care at every Medicare and Medicaid-certified Nursing Home currently running in the country.
                                       <br/> <br/>
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
                        ),
                        tabItem(tabName = "datatable",
                                tabBox(title = "Selection Summary", 
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset1", height = "470px", width = 11, side = "right",
                                       tabPanel("Map", leafletOutput("lemap"), icon = icon("map-marker")),
                                       tabPanel("Ratings & Penalties", verbatimTextOutput('ratings'), 
                                                hr(),verbatimTextOutput('penalties')),
                                       tabPanel("Other", verbatimTextOutput('other')),
                                       tabPanel("General Info", verbatimTextOutput('general'))
                                ),
                                
                                box(title = "Data Table:", status = "primary", solidHeader = TRUE,
                                    br(), width = 11, 
                                    column(5, DT::dataTableOutput('table'))
                                )
                        ),
                        tabItem(tabName = "graph",
                                verticalLayout(
                                  box(title = textOutput("viz"),
                                      plotOutput("pie")),
                                  box(title = textOutput("viz2"),
                                      plotOutput("bar")))
                        )
                      )
                  )
                )
  )
)



shinyUI(ui)
