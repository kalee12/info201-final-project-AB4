library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)

data("state")
state.name

ui <- fluidPage(
  imageOutput("logo", height = "200px", width = "600px"), 
  dashboardPage(skin = "blue", 
                dashboardHeader(title = "Dashboard"),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("About", tabName = "introduction", icon = icon("users")),
                    menuItem("Data Table", tabName = "datatable", icon = icon("table")),
                    menuItem("Map", tabName = "map", icon = icon("map-marker")),
                    selectInput("state", "Location", c("National", state.name)),
                    selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                    box(title = "Glossry", height = "250px", width = 12,
                        verticalLayout(
                          div(
                            h4("Glossory (Scrollable):"),
                            HTML("<b>1) Certified Nurse (CN):</b> A certified nursing assistant,
                                or CNA, helps patients or clients with healthcare needs
                                under the supervision of a<i> Registered Nurse (RN)</i> or a <i>Licensed
                                Practical Nurse (LPN)</i>.</br><b>2) Registered Nurse (RN): </b> A nurse who
                                has graduated from a nursing program and has sucesfully obtained a
                                license.</br><b>3) Licensed Practical Nurse (LPN):</b> A nurse who cares for
                               people who are sick, injured, convalescent, or disabled. LPNs work
                                 under the direction of registered nurses or physicians."),
                            style = "height: 200px; overflow-y:scroll; color:black;"
                          )
                        )
                    )
                    
                    
                    
                  )
                ),
                dashboardBody(
                  box(width = 20, status = "primary",
                      tabItems(
                        # First tab content
                        tabItem(tabName = "introduction",
                                helpText("Display Some Text here")
                        ),
                        tabItem(tabName = "datatable",
                                tabBox(title = "Selection Summary", 
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset1", height = "270px", width = 11, side = "right",
                                       tabPanel("General Info", verbatimTextOutput('general')),
                                       tabPanel("Ratings", verbatimTextOutput('ratings')),
                                       tabPanel("Penalties", verbatimTextOutput('penalties')),
                                       tabPanel("Other", verbatimTextOutput('other'))
                                ),
                                
                                br(),
                                box(title = "Data Table:", status = "primary", solidHeader = TRUE,
                                    br(), width = 11, 
                                    column(5, DT::dataTableOutput('table'))
                                )
                                # sidebarPanel(
                                #   column(3, sidebarPanel(tabPanel("General Info", verbatimTextOutput('general')),
                                #                          tabPanel("Ratings", verbatimTextOutput('ratings')),
                                #                          tabPanel("Penalties", verbatimTextOutput('penalties')),
                                #                          tabPanel("Other", verbatimTextOutput('other')))
                                #   )
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
