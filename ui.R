library(dplyr)
library(ggplot2)
library(plotly)


ui <- flowLayout(
          
  navbarPage(
    tabPanel("About the Project",
             
    ),
    tabPanel("Data Analysis",
           plotOutput("map"),
           selectInput('filter_state', label = "Filter by:", choices = 
                         c('comparative data', 'individual data')
           ),
           selectInput('filter_stars',  choices = 
                         c('comparative data', 'individual data')
           ),
           selectInput('filter_penalties', choices = 
                         c('comparative data', 'individual data')
           )
           
           
    ),

  )
  

  
)