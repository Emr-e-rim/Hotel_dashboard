#SET PATH

setwd("PATH")
##
source("functions.R")
library(leaflet)
library(shiny)
library(DT)
#vignette('rworldmap')

ui <- fluidPage(
  fluidRow(
    column(8,br(),leafletOutput("map", height="600px")),
    column(4,br(),br(),br(),br(),plotlyOutput("pie", height="400px"))
  ),
  DT::dataTableOutput("table")
)

server <- function(input, output, session) {
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  output$map <- renderLeaflet({
    showmap()
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Show pie chart depending on selected hotel
  output$pie=renderPlotly({
    my_place=data_of_click$clickedMarker$id
    PiePerHotel(data_of_click$clickedMarker$id)
  })
  
  output$table <- DT::renderDataTable({
    my_place=data_of_click$clickedMarker$id
    ShowReviews(data_of_click$clickedMarker$id)
  })
}

shinyApp(ui = ui, server = server)
