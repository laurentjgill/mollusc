# R Shiny app for Mollusc Data
# May 4th 2023
#Lauren Gill
#Using data from Bill Merilees Collection housed at Beaty Biodiversity museum

#Loading libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(tidyverse)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("The Merilees Micromollusc Collection"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Select region of interest ----
      selectInput(inputId = "region",
                  label = "Select a region:",
                  choices = c("Vancouver Island", "Gulf Islands", "Sunshine Coast"),
                  selected = "Vancouver Island"),
      
      # Select year of interest ----
      sliderInput(inputId = "year",
                  label = "Select a year:",
                  min = 2010, max = 2017, value = 2010, step = 1),
      
      # Select tide height of interest ----
      sliderInput(inputId = "tide",
                  label = "Select a tide height:",
                  min = 0, max = 4, value = 0, step = 0.1),
      
      # Select mollusc family of interest ----
      selectInput(inputId = "family",
                  label = "Select a mollusc family:",
                  choices = unique(mollusc_data$family),
                  selected = "Nuculidae"),
      
      # Select number of species to display ----
      numericInput(inputId = "num_species",
                   label = "Number of species to display:",
                   value = 10)
      
    ),
    
    # Main panel for displaying output ----
    mainPanel(
      
      # Map of collection sites ----
      leafletOutput("map", height = "500px"),
      
      # Bar plot of species by year ----
      plotOutput("species_by_year"),
      
      # Histogram of tide heights ----
      plotOutput("tide_histogram"),
      
      # Pie chart of family composition ----
      plotOutput("family_pie_chart")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Filter data based on user inputs ----
  mollusc_data_filtered <- reactive({
    mollusc_data %>%
      filter(region == input$region,
             year == input$year,
             tide_height >= input$tide,
             family == input$family) %>%
      group_by(species) %>%
      summarize(n = n()) %>%
      top_n(input$num_species, n)
  })
  
  # Create map of collection sites ----
  output$map <- renderLeaflet({
    leaflet(mollusc_data_filtered()) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~long, popup = ~species)
  })
  
  # Create bar plot of species by year ----
  output$species_by_year <- renderPlot({
    ggplot(mollusc_data_filtered(), aes(x = year, y = n)) +
      geom_bar(stat = "identity") +
      labs(x = "Year", y = "Number of Species", title = "Mollusc Species by Year")
  })
  
  # Create histogram of tide heights ----
  output$tide_histogram <- renderPlot({
    ggplot(mollusc_data_filtered(), aes(x = tide_height)) +
      geom_histogram(binwidth = 0.1) +
      labs(x = "Tide Height", y = "Frequency", title = "Tide Height Distribution")
  })
  
  # Create pie chart of family composition ----
  output$family_pie_chart <- renderPlot({
    ggplot(m
           
