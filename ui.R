# Simran
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
                   #plotOutput("map")
                   leafletOutput("lemap")
                 )
                 
               ),
               sidebarLayout(
                 sidebarPanel(
                   tabsetPanel(
                     tabPanel("One", textOutput("text")),
                     tabPanel("Two", textOutput("house"))
                   ),
                   width = 3, 
                   sliderInput("ratings", "Nursing Home Ratings:",
                               min=0, max=5, value=1), 
                   sliderInput("penalties", "Nursing Home Penalties:",
                               min=0, max=100, value=50)
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