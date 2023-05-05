# R Shiny app for Mollusc Data
# May 4th 2023
#Lauren Gill
#Using data from Bill Merilees Collection housed at Beaty Biodiversity museum

#Loading libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(dplyr)

# Load mollusc data
mollusc_data <- read.csv("full_data_amalgimated_data_analysis.csv", header = TRUE) %>%
  separate(date, into = c('month', 'day', 'year'), sep = " ", remove = FALSE) %>%
  mutate(across(everything(),
                ~str_replace_all(., "[^[:alnum:]///' ]", ""))) %>%
  mutate_all(na_if,"") %>%
  mutate(year = as.factor(year))
  
# Load location data
location_data <- read_csv("locations.csv")

#choice list for year
choise_list=list("2012" = "2012", "2013" = "2013","2014" = "2014", "2015" = "2015",
                 "2016" = "2016", "2017" = "2017", "2018" = "2018","2019" = "2019",
                 "2020" = "2020")

# Define UI

ui <- navbarPage(
  #set the theme
  theme = shinytheme("cerulean"),
  title = "App",
  # Home page with a link to the app
  tabPanel(
    "Home",
    imageOutput("home_img"),
    br(),
    p("Welcome - click on the Data tab to explore")
  ),
  # App tab
  tabPanel(
    "The Data",
  # Set page title
  titlePanel("Merilees MicroMollusc Collection Analysis (Work in Progress)"),
  
  # Define sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Add input widgets for filtering data
      selectInput("region", "Select Region", unique(mollusc_data$region)),
      checkboxGroupInput("year", "Select Year", unique(mollusc_data$year),
                         choices = choise_list,
                         selected = choise_list),
      
      # Add action button for updating the map and graphs
      actionButton("update_button", "Update")
    ),
    
    # Define main panel with output elements
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Abundant Genuses", plotOutput("bar_plot")),
        tabPanel("Species by Class", plotOutput("line_plot")),
        tabPanel("Methods Used", plotOutput("scatter_plot")),
        tabPanel("About the Data", textOutput("text"))
      )
    )
  )
))

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
      theme_bw()
  })

  # Render line plot
  output$line_plot <- renderPlot({
    reactive <- filtered_data()
    reactive %>%
      select(date, number, class) %>%
      rename(species_number = number) %>%
      separate(date, into = c('month', 'day', 'year'), sep = " ") %>%
      mutate(across(everything(),
                    ~str_replace_all(., "[^[:alnum:]///' ]", ""))) %>%
      mutate(year = as.factor(year),
             species_number = as.numeric(species_number)) %>%
      group_by(class, year) %>%
      summarize(species_number = sum(species_number, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = species_number, fill = class)) +
      geom_bar(stat = "identity") +
      labs(x = "Year", y = "Number of Animals Collected", fill = "Class") +
      theme_minimal()
  })
  
  # Render pie plot
  output$scatter_plot <- renderPlot({
    reactive <- filtered_data()
    reactive %>%
      group_by(methods) %>%
      summarize(count = n()) %>%
      ggplot(aes(x="", y=count, fill=methods)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()
  })
  
  # Render text
  output$text <- renderPrint({
    cat("This dataset contains intertidal mollusc data collected by Bill Merilees from 2012-2020 in different regions of British Columbia and Washington. A map is provided to visualize the collection locations, and graphs are included to display species abundance and the methods used to collect the data. The collections are now housed at the Beaty Biodiversity Museum in Vancouver, British Columbia, Canada. Use the input controls to explore the data for each region. Data analysis and R Shiny App created by Lauren Gill")
  })
  
  #Rende photo for home page
  output$home_img <- renderImage({
    
    list(src = "www/header.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
}

# Run app
shinyApp(ui = ui, server = server)
