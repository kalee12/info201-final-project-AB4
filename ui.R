library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

data("state")
state.name 
ratings.range<- range(1,5)


ui <- fluidPage(
  imageOutput("logo", height = "150px", width = "200px"), 
  tabsetPanel(
    tabPanel("Introduction",
             p("Hello")),
    
    tabPanel("Data",
             verticalLayout(
               sidebarLayout(
                 sidebarPanel(
                   #fluidRow(
                   selectInput("state", "Location", c("National", state.name)),
                   #selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                   
                   width = 3, 
                   sliderInput("ratings", "Filter by Nursing Home Ratings:",
                               min=ratings.range[1], max=ratings.range[2], value=ratings.range),
                   radioButtons("radio", "Filter by Fines:",
                                choices = list("Has a fine" = 1,"Doesn't have a fine" = 2, "All" = 3),selected = 3
                   )
                   
                   
                 ), 
                 
                 
                 
                 mainPanel(
                   leafletOutput("lemap")
                 )
               ),
               
               
               fluidRow(
                 
                 column(5, tabsetPanel(tabPanel("General Info", verbatimTextOutput('general')),
                                       tabPanel("Ratings & Penalties", verbatimTextOutput('ratings'), hr(),verbatimTextOutput('penalties')),
                                       
                                       tabPanel("Other", verbatimTextOutput('other')))),
                 column(7, DT::dataTableOutput('table'))
                 
                 
                 
               )
             )
    )
  )
)



shinyUI(ui)


