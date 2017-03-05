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
                 
                 mainPanel(
                   plotOutput("map")
                 )
                 
               ),
               
               fluidRow(
                 
                 column(5, tabsetPanel(tabPanel("General Info", verbatimTextOutput('general')),
                                       tabPanel("Ratings", verbatimTextOutput('ratings')),
                                       tabPanel("Penalties", verbatimTextOutput('penalties')),
                                       tabPanel("Other", verbatimTextOutput('other')))),
                 column(7, DT::dataTableOutput('table'))
                 
                 
                 
               )
             )
    )
  )
)

shinyUI(ui)


