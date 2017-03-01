library(dplyr)
library(ggplot2)
library(plotly)


ui <- flowLayout(
          
  navbarPage(
    tabPanel("About the Project",
             
    ),
    tabPanel("Data Analysis",
           plotOutput("map"),
           
           
    ),

  )
  

  
)