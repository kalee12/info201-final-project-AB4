# # Karen
# 
# library(dplyr)
# library(ggplot2)
# library(plotly)
# library(shiny)
# 
# data("state")
# state.name
# 
# ui <- fluidPage(
#     titlePanel("Hello"),
#       tabsetPanel(
#         tabPanel("Introduction",
#                  p("Hello")),
#         
#         tabPanel("Data",
#           verticalLayout(
#             sidebarLayout(
#               sidebarPanel(
#                 #fluidRow(
# 
#                   selectInput("state", "Location", c("National", state.name)),
#                   selectInput("category", "Category", c("All", "Ratings", "Penalties")),
#                   width = 3
# 
#               ),
#             #
#               mainPanel(
#                 #plotOutput("map")
#                 leafletOutput("lemap")
#               )
#           
#           ),
#           sidebarLayout(
#             sidebarPanel(
#               tabsetPanel(
# 
#                 tabPanel("One", textOutput("text")),
#                 tabPanel("Two", textOutput("house"), tags$style(type = "text/css", "#house {font-family:Verdana}"))
#               ),
#               width = 3
#             ),
#             mainPanel(
#               dataTableOutput("table")
#             )
#           )
#           )
#         ),
#         tabPanel("Something Else",
#                  p("Hello"))
#       )
# )
# 
# shinyUI(ui)
# 
# # ui <- fluidPage(
# #   titlePanel("Hello"),
# #   tabsetPanel(
# #     tabPanel("Introduction",
# #              p("Hello")),
# #     
# #     tabPanel("Data",
# #              verticalLayout(
# #                plotOutput("map"),
# #                sidebarLayout(
# #                  sidebarPanel(
# #                    fluidRow(
# #                      column(5, selectInput("state", "Location", c("National", state.name))),
# #                      column(5, selectInput("category", "Category", c("All", "Ratings", "Penalties")))
# #                    )
# #                  ),
# #                  mainPanel(
# #                    verticalLayout(
# #                      textOutput("text")),
# #                    hr(),
# #                    textOutput("house")
# #                  )
# #                ),
# #                dataTableOutput("table")
# #              )
# #     ),
# #     tabPanel("Something Else",
# #              p("Hello"))
# #   )
# # )

#install.packages("shinydashboard")

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)

library(shinydashboard)
ratings.range<- range(1,5)
data("state")
state.name

ui <- fluidPage(
  imageOutput("logo", height = "200px", width = "600px"), 
  dashboardPage(skin = "blue", 
                dashboardHeader(title = "Dashboard",
                                dropdownMenu(icon = icon("font"),  
                                             messageItem(
                                               from = "Sales Dept",
                                               message = "Sales are steady this month."
                                             ))),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("About", tabName = "introduction", icon = icon("users")),
                    menuItem("Data Table", tabName = "datatable", icon = icon("table")),
                    menuItem("Graph", tabName = "graph", icon = icon("map-marker")),
                    selectInput("state", "Location", c("National", state.name)),
                    # selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                    sliderInput("ratings", "Filter by Nursing Home Ratings:",
                                min=ratings.range[1], max=ratings.range[2], value= c(5, 5)),
                    radioButtons("radio", "Filter by Fines:",
                                 choices = list("Has a fine" = 1,"Doesn't have a fine" = 2, "All" = 3),selected = 2
                    )
                    
                    # box(title = "Glossry", height = "250px", width = 12,
                    #     verticalLayout(
                    #       div(
                    #         h4("Glossory (Scrollable):"),
                    #         HTML("<b>1) Certified Nursing Assistant (CNA):</b> A certified nursing assistant,
                    #             or CNA, helps patients or clients with healthcare needs
                    #             under the supervision of a<i> Registered Nurse (RN)</i> or a <i>Licensed
                    #             Practical Nurse (LPN)</i>.</br><b>2) Registered Nurse (RN): </b> A nurse who
                    #             has graduated from a nursing program and has sucesfully obtained a
                    #             license.</br><b>3) Licensed Practical Nurse (LPN):</b> A nurse who cares for
                    #            people who are sick, injured, convalescent, or disabled. LPNs work
                    #              under the direction of registered nurses or physicians."),
                    #         style = "height: 200px; overflow:auto; color:black;"
                    #       )
                    #     )
                    # )
                    
                    
                    
                  )
                ),
                dashboardBody(
                  box(width = 20, status = "primary",
                      tabItems(
                        # First tab content
                        tabItem(tabName = "introduction",
                                helpText("Display Some Text here"),
                                div(HTML("Hello <hr/> Hello"))
                        ),
                        tabItem(tabName = "datatable",
                                tabBox(title = "Selection Summary", 
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset1", height = "480px", width = 11, side = "right",
                                       tabPanel("Map", leafletOutput("lemap"), textOutput("summary")), 
                                       tabPanel("Other", verbatimTextOutput('other')),
                                       tabPanel("Ratings & Penalties", verbatimTextOutput('ratings'), hr(),verbatimTextOutput('penalties')),
                                       tabPanel("General Info", verbatimTextOutput('general'))
                                       
                                ),
                                
                                br(), 
                                box(title = "Data Table:", status = "primary", solidHeader = TRUE,
                                    br(), width = 11, 
                                    column(5, DT::dataTableOutput('table'))
                                )
                                
                        ),
                        tabItem(tabName = "graph",
                                box(title = textOutput("viz"),
                                    plotOutput("pie")),
                                box(title = textOutput("viz2"),
                                    plotOutput("bar"))
                        )
                      )
                  )
                )
                
                # tabsetPanel(
                #   tabPanel("Introduction",
                #            p("Hello")),
                #   
                #   tabPanel("Data",
                #            verticalLayout(
                #              sidebarLayout(
                #                sidebarPanel(
                #                  #fluidRow(
                #                  selectInput("state", "Location", c("National", state.name)),
                #                  selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                #                  width = 3,
                #                  div(
                #                    h4("Glossory (Scrollable):"),
                #                    HTML("<b>1) Certified Nurse (CN):</b> A certified nursing assistant, 
                #                        or CNA, helps patients or clients with healthcare needs 
                #                        under the supervision of a<i> Registered Nurse (RN)</i> or a <i>Licensed 
                #                        Practical Nurse (LPN)</i>.</br><b>2) Registered Nurse (RN): </b> A nurse who 
                #                        has graduated from a nursing program and has sucesfully obtained a 
                #                        license.</br><b>3) Licensed Practical Nurse (LPN):</b> A nurse who cares for 
                #                       people who are sick, injured, convalescent, or disabled. LPNs work 
                #                         under the direction of registered nurses or physicians."),
                #                    style = "height: 200px; overflow:scroll;"
                #                  )
                #                ),
                #                
                #                mainPanel(
                #                  tabsetPanel(
                #                    tabPanel("Map",
                #                             leafletOutput("lemap")
                #                    ),
                #                    tabPanel("Data Table",
                #                             column(5, DT::dataTableOutput('table')),
                #                             sidebarPanel(
                #                                            column(3, sidebarPanel(tabPanel("General Info", verbatimTextOutput('general')),
                #                                                     tabPanel("Ratings", verbatimTextOutput('ratings')),
                #                                                     tabPanel("Penalties", verbatimTextOutput('penalties')),
                #                                                     tabPanel("Other", verbatimTextOutput('other')))
                #                               )
                #                             )
                #                               )
                #                    
                #                             )
                #                    )
                #                  )
                #                )
                #              )
                #            )
  )
)


shinyUI(ui)