# Karen

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

data("state")
state.name

ui <- fluidPage(
  titlePanel("Hello"),
  tabsetPanel(
    tabPanel("Introduction",
             p("Hello")),
    
    tabPanel("Data",
             verticalLayout(
               sidebarLayout(
                 sidebarPanel(
                   #fluidRow(
                   selectInput("state", "Location", c("National", state.name)),
                   selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                   width = 3
                   
                 ),
                 #
                 mainPanel(
                   plotOutput("map")
                 )
                 
               ),
               sidebarLayout(
                 sidebarPanel(
                   tabsetPanel(
                     tabPanel("One", textOutput("text")),
                     tabPanel("Two", textOutput("house"))
                   ),
                   width = 3
                 ),
                 mainPanel(
                   dataTableOutput("table")
                 )
               )
             )
    ),
    tabPanel("Something Else",
             p("Hello"))
  )
)

shinyUI(ui)

# ui <- fluidPage(
#   titlePanel("Hello"),
#   tabsetPanel(
#     tabPanel("Introduction",
#              p("Hello")),
#     
#     tabPanel("Data",
#              verticalLayout(
#                plotOutput("map"),
#                sidebarLayout(
#                  sidebarPanel(
#                    fluidRow(
#                      column(5, selectInput("state", "Location", c("National", state.name))),
#                      column(5, selectInput("category", "Category", c("All", "Ratings", "Penalties")))
#                    )
#                  ),
#                  mainPanel(
#                    verticalLayout(
#                      textOutput("text")),
#                    hr(),
#                    textOutput("house")
#                  )
#                ),
#                dataTableOutput("table")
#              )
#     ),
#     tabPanel("Something Else",
#              p("Hello"))
#   )
# )