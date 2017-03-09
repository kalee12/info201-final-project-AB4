library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
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
                                                                    under the supervision of <br/> Registered Nurse (RN) or a <br/>
                                                                    Licensed Practical Nurse (LPN)."),
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
                                min=ratings.range[1], max=ratings.range[2], value= c(5, 5)),
                    radioButtons("radio", "Filter by Fines:",
                                 choices = list("Has a fine" = 1,"Doesn't have a fine" = 2, "All" = 3),selected = 1
                    )
                  )
                ),
                dashboardBody(
                  box(width = 20, status = "primary",
                      tabItems(
                        # First tab content
                        tabItem(tabName = "introduction",
                                htmlOutput("introduction")
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
                                box(title = textOutput("viz"), solidHeader = TRUE, status = "primary",
                                    plotOutput("pie")),
                                box(title = textOutput("viz2"), solidHeader = TRUE, status = "primary",
                                    plotOutput("bar")),
                                
                                box(title = "Pie Chart Summary", status = "primary",
                                    textOutput("pie.chart.summary")
                                ),
                                box(title = "Point Graph Summary", status = "primary",
                                    textOutput("point.graph.summary")
                                )
                                
                        )
                      )
                  )
                )
  )
)



shinyUI(ui)