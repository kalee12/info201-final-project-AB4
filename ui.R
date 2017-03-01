library(dplyr)
library(ggplot2)
library(plotly)

info <- read.csv('data/Provider_Info.csv')

#View(info)

ui <- fluidPage(
  
  titlePanel("Nursing Home Finder"),
  navbarPage(
    tabPanel("About the Project" 
             
    ),
    tabPanel("Data Analysis",
           plotOutput("map"),
           
           column(2, selectInput('filter_state', label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           )),
           column(2,selectInput('filter_stars',  label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           )),
           column(2,selectInput('filter_penalties', label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           )),
           br(),
           br(),
           br(),
           br(),
           dataTableOutput('table')
    )

  )
  
)
  
shinyUI(ui)
  
