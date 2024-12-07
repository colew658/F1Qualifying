library(shiny)
library(bslib)
library(reactable)
library(ggplot2)
library(tidymodels)
library(ranger)

final_fit <- readRDS("Data/final_fit.rds")

ui <- page_navbar(
  title = "F1 Qualifying Analysis Dashboard",
  theme = bs_theme(version = 5),
  
  nav_panel("Predictions",
            layout_sidebar(
              sidebar = sidebar(
                title = "Input Parameters",
                numericInput("airtemp", "Air Temperature (°C):", value = 0),
                numericInput("humidity", "Relative Humidity (%):", value = 0),
                numericInput("pressure", "Pressure (mbar):", value = 0),
                selectInput("rainfall", "Is is raining?:", choices = c("No", "Yes")),
                numericInput("tracktemp", "Track Temperature (°C):", value = 0),
                numericInput("windspeed", "Wind Speed (m/s):", value = 0),
                numericInput("rainlaps", "# of Raining Laps:", value = 0)
              ),
              card(
                card_header("Model Prediction"),
                textOutput("prediction_value")
              ),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Feature Importance"),
                  plotOutput("importance_plot")
                ),
                card(
                  card_header("ALE Plot"),
                  selectInput("ale_feature", "Select Feature:", 
                              choices = c("Air Temperature", "Humidity",
                                          "Pressure", "Rainfall",
                                          "Track Temperature", "Wind Speed",
                                          "# of Raining Laps")),
                  plotOutput("ale_plot")
                )
              )
            )
  ),
  
  nav_panel("Driver Summary",
            card(
              reactableOutput("driver_summary")
            )
  ),
  
  nav_panel("Best Laps",
            card(
              reactableOutput("lap_reactable")
            )
  ),
  
  nav_panel("Weather Summary",
            card(
              reactableOutput("weather_reactable")
            )
  )
)

server <- function(input, output) {
  # Reactive prediction
  prediction <- reactive({
    new_data <- data.frame(
      AIR_TEMPERATURE = input$airtemp,
      HUMIDITY = input$humidity,
      PRESSURE = input$pressure,
      RAINFALL = factor(ifelse(input$rainfall == "Yes", 1, 0)),
      TRACK_TEMPERATURE = input$tracktemp,
      WIND_SPEED = input$windspeed,
      rain_laps = input$rainlaps
    )
    predict(final_fit, new_data)
  })
  
  # Prediction output
  output$prediction_value <- renderText({
    paste("Predicted Lap Time (s):", round(prediction(), 3))
  })
  
  # Variable importance plot
  output$importance_plot <- renderPlot({
    plot(readRDS("Data/imp_weatheronly.rds")) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12))
  })
  
  # ALE plot
  output$ale_plot <- renderPlot({
    if (input$ale_feature == "Air Temperature") {
      aleplot <- readRDS("Data/airtemp_ale.rds")
    } else if (input$ale_feature == "Humidity") {
      aleplot <- readRDS("Data/humidity_ale.rds")
    } else if (input$ale_feature == "Pressure") {
      aleplot <- readRDS("Data/pressure_ale.rds")
    } else if (input$ale_feature == "Rainfall") {
      aleplot <- readRDS("Data/rainfall_ale.rds")
    } else if (input$ale_feature == "Track Temperature") {
      aleplot <- readRDS("Data/tracktemp_ale.rds")
    } else if (input$ale_feature == "Wind Speed") {
      aleplot <- readRDS("Data/windspeed_ale.rds")
    } else if (input$ale_feature == "# of Raining Laps") {
      aleplot <- readRDS("Data/rainlaps_ale.rds")
    }
    
    plot(aleplot) +
      labs(x = input$ale_feature, y = "ALE of Lap Duration") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12))
  })
  
  # Driver summary table
  output$driver_summary <- renderReactable({
    readRDS("Data/driver_reactable.rds")
  })
  
  # Best laps summary table
  output$lap_reactable <- renderReactable({
    readRDS("Data/lap_reactable.rds")
  })
  
  # Weather summary table
  output$weather_reactable <- renderReactable({
    readRDS("Data/weather_reactable.rds")
  })
}

shinyApp(ui, server)
