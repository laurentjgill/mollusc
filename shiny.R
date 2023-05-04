# R Shiny app for Mollusc Data
# May 4th 2023
#Lauren Gill
#Using data from Bill Merilees Collection housed at Beaty Biodiversity museum

#Loading libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(dplyr)

# Load mollusc data
mollusc_data <- read.csv("full_data_amalgimated_data_analysis.csv", header = TRUE) %>%
  separate(date, into = c('month', 'day', 'year'), sep = " ", remove = FALSE) %>%
  mutate(across(everything(),
                ~str_replace_all(., "[^[:alnum:]///' ]", ""))) %>%
  drop_na(year) %>%
  mutate(year = as.factor(year))
  
# Load location data
location_data <- read_csv("locations.csv")

# Define UI
ui <- fluidPage(
  
  # Set page title
  titlePanel("Merilees MicroMollusc Collection Analysis (Work in Progress)"),
  
  # Define sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Add input widgets for filtering data
      selectInput("region", "Select Region", unique(mollusc_data$region)),
      selectInput("year", "Select Year", unique(mollusc_data$year)),
      
      # Add action button for updating the map and graphs
      actionButton("update_button", "Update")
    ),
    
    # Define main panel with output elements
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Bar Plot", plotOutput("bar_plot")),
        tabPanel("Line Plot", plotOutput("line_plot")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Create reactive data filtered by user input
  filtered_data <- reactive({
    mollusc_data %>%
      filter(region == input$region,
             year == input$year)
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet(data = location_data) %>%
      addTiles() %>%
      addMarkers(lng = ~long,
                 lat = ~lat, 
                 popup = ~paste(region, ": ", species_count, " species"))
  })
  
  
  # Render bar plot
  output$bar_plot <- renderPlot({
    reactive <- filtered_data()
    reactive %>%
      group_by(genus) %>%
      mutate(number = as.numeric(number)) %>%
      summarize(count = sum(number)) %>%
      arrange(desc(count)) %>%
      slice(1:10) %>%
      mutate(genus = fct_reorder(genus, desc(count))) %>%
      ggplot(aes(x = genus, y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Genus") +
      ylab("Number of individuals Collected") +
      theme_bw
  })

  # Render line plot
  output$line_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = year, y = number)) +
      geom_line() +
      labs(x = "Year", y = "Number Collected")
  })
  
  # Render scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = tide_height, y = number_collected)) +
      geom_point() +
      labs(x = "Tide Height", y = "Number Collected")
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
