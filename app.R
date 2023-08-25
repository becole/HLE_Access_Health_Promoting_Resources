# Load necessary libraries
library(ggplot2)
library(shiny)
library(dplyr)
library(rsconnect)
library(sf)
library(viridis)

HLE_data <- readRDS("HLE_data.rds")

# Define UI
geo_ui <- fluidPage(
  titlePanel("Geospatial Distribution of Health Life Expectancy (HLE) and Life Expectancy (LE)"),
  
  # Description
  headerPanel(h3("Interactive visualization of HLE and LE across different regions.")),
  
  sidebarLayout(
    sidebarPanel(
      tags$hr(),  # Horizontal line for separation
      
      # Dropdown for outcome selection with descriptive label
      tags$label("Select an outcome:"),
      selectInput("outcome", NULL, choices = names(rename_map <- c("hle_years_female" = "Female HLE (years)",
                                                                   "le_years_female" = "Female LE (years)",
                                                                   "hle_years_male" = "Male HLE (years)",
                                                                   "le_years_male" = "Male LE (years)"
      ))),
      
      tags$br(),  # Break for spacing
      
      # Dropdown for region selection with descriptive label
      tags$label("Select a region:"),
      selectInput("region", NULL, choices = unique(HLE_data$Region)),
      
      tags$hr()  # Horizontal line for separation
    ),
    
    mainPanel(
      plotOutput("geospatialPlot", height = "600px")  # Set a fixed height for consistent rendering
    )
  )
)

# Define server logic
geo_server <- function(input, output) {
  output$geospatialPlot <- renderPlot({
    HLE_data_region <- HLE_data %>% filter(Region == input$region)
    
    # Use the geospatial plotting code you provided
    geospatial_plot <- ggplot() +
      geom_sf(data = HLE_data_region, aes(geometry = geometry, fill = !!sym(input$outcome)), color = "black", size = 0.1) +
      scale_fill_viridis_c(option = "D") +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude", title = paste0("Geospatial Distribution of ", input$outcome, " in ", input$region)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), text = element_text(size = 16), legend.position = "bottom")
    
    return(geospatial_plot)
  })
}

shinyApp(ui = geo_ui, server = geo_server)
