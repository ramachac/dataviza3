#"/Users/chandhiny/DV A3/IHME_GBD_2010_MORTALITY_1970_2010.CSV"
# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Load the dataset
file_path <- "/Users/chandhiny/DV A3/IHME_GBD_2010_MORTALITY_1970_2010.CSV"
data <- read.csv(file_path, encoding = "latin1", stringsAsFactors = FALSE)

# Clean the dataset by handling NA values
data$death_abs <- suppressWarnings(as.numeric(data$death_abs))
data$death_rate <- suppressWarnings(as.numeric(data$death_rate))

# Replace NA values with 0 (or mean, if preferred)
data$death_abs[is.na(data$death_abs)] <- 0
data$death_rate[is.na(data$death_rate)] <- 0

# UI Layout
ui <- fluidPage(
  titlePanel("Global Mortality Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Select Region:",
        choices = unique(data$region_name),
        selected = unique(data$region_name)[1]
      ),
      selectInput(
        inputId = "gender",
        label = "Select Gender:",
        choices = unique(data$sex_name),
        selected = "Both"
      ),
      sliderInput(
        inputId = "year",
        label = "Select Year:",
        min = min(data$year),
        max = max(data$year),
        value = max(data$year),
        step = 1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
        tabPanel("Heatmap", plotlyOutput("heatmap")),
        tabPanel("Box Plot", plotlyOutput("boxPlot")),
        tabPanel("Line Chart", plotlyOutput("lineChart"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Scatter Plot with Facets
  output$scatterPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(region_name == input$region, sex_name == input$gender, year == input$year)
    
    plot <- ggplot(filtered_data, aes(x = death_rate, y = death_abs, color = age_name, size = death_abs)) +
      geom_point() +
      labs(
        title = "Mortality Rate vs. Absolute Deaths by Age Group",
        x = "Mortality Rate",
        y = "Number of Deaths"
      ) +
      theme_minimal()
    ggplotly(plot)
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    heatmap_data <- data %>%
      group_by(region_name, year) %>%
      summarise(death_rate = mean(death_rate, na.rm = TRUE))
    
    plot <- ggplot(heatmap_data, aes(x = year, y = region_name, fill = death_rate)) +
      geom_tile() +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(
        title = "Mortality Rate Trends Over Time by Region",
        x = "Year",
        y = "Region",
        fill = "Mortality Rate"
      ) +
      theme_minimal()
    ggplotly(plot)
  })
  
  # Box Plot
  output$boxPlot <- renderPlotly({
    box_data <- data %>%
      filter(region_name == input$region, sex_name == input$gender, year == input$year)
    
    plot <- ggplot(box_data, aes(x = age_name, y = death_rate, fill = age_name)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 8, alpha = 0.7) +
      labs(
        title = "Mortality Rate Distribution by Age Group",
        x = "Age Group",
        y = "Mortality Rate"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(plot)
  })
  
  # Line Chart
  output$lineChart <- renderPlotly({
    line_data <- data %>%
      filter(region_name == input$region, sex_name == input$gender) %>%
      group_by(year) %>%
      summarise(death_abs = sum(death_abs, na.rm = TRUE))
    
    plot <- ggplot(line_data, aes(x = year, y = death_abs, group = 1)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = "Trends of Absolute Deaths Over Time",
        x = "Year",
        y = "Number of Deaths"
      ) +
      theme_minimal()
    ggplotly(plot)
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)