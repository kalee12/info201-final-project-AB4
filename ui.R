library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

data("state")
state.name

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
                   selectInput("category", "Category", c("All", "Ratings", "Penalties")),
                   width = 3,
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
                     style = "height: 200px; overflow:scroll;"
                   ),
                   
                   
                   
                   mainPanel(
                     leafletOutput("lemap")
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
)

shinyUI(ui)
