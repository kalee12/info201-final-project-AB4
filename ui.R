library(dplyr)
library(ggplot2)
library(plotly)

info <- read.csv('data/Provider_Info.csv')

#View(info)

ui <- fluidPage(
          
  navbarPage(
    tabPanel("About the Project" 
             
    ),
    tabPanel("Data Analysis",
           plotOutput("map"),
           selectInput('filter_state', label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           ),
           selectInput('filter_stars',  label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           ),
           selectInput('filter_penalties', label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           ),
           verbatimTextOutput('summary'),
           dataTableOutput('table')
           
           
    )

  )
)
  
shinyUI(ui)
  
